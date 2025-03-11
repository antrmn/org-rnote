;;; org-rnote.el --- Rnote integration for Org Mode  -*- lexical-binding: t -*-

;; Author: Antonio Romano <n58r@pm.me>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (org "9.8-pre"))
;; Keywords: productivity, org, drawing
;; URL: https://github.com/antrmn/org-rnote

;;; Commentary:

;;; Code:

(require 'org)
(require 'org-macs)
(require 'org-persist)
(require 'ol)
(require 'filenotify)

(defconst rnote-file-regexp "\\.rnote$"
  "Regular expression matching Rnote files.")

(defgroup org-rnote nil
  "Customization for `org-rnote'."
  :group 'org
  :prefix "org-rnote-")

;;; Export related functions

(defconst org-rnote--container-header
  '((elisp-data "org-rnote") (version "0.1"))
  "Identifier for org-persist container.
Used to distinguish and version cached data.")

(defun org-rnote--export-file (source dest callback)
  "Export SOURCE file to DEST asynchronously using rnote-cli and call CALLBACK.
CALLBACK is a function or list of functions that receive the output
path on success or nil on failure."
  (let* ((expanded-source (expand-file-name (substitute-in-file-name source)))
         (expanded-dest (expand-file-name (substitute-in-file-name dest))))
    (org-async-call
     (list "rnote-cli" "export" "selection"
           "--output-file" expanded-dest "--no-background"
           "all"  expanded-source)
     :buffer "*Rnote export output*"
     :failure (lambda (_ __ ___)
                (message "Rnote export failed")
                (dolist (cb (ensure-list callback))
                  (funcall cb nil)))
     :success (lambda (_ __ ___)
                (dolist (cb (ensure-list callback))
                  (funcall cb expanded-dest))))))

(defun org-rnote--get-file-stamp (file)
  "Generate a unique stamp for FILE based on modification time and size.
Returns a cons cell of (modification-time . file-size)."
  (let ((remote-file-inhibit-cache t)
        (attr (file-attributes file)))
    (cons (file-attribute-modification-time attr)
          (file-attribute-size attr))))

(defun org-rnote--get-cached (source)
  "Retrieve cached export for SOURCE if valid.
Returns the cached export or nil if not found or outdated."
  (when-let* ((cached (org-persist-read org-rnote--container-header
                                        source
                                        nil nil :read-related t)))
    (let ((stamp (org-rnote--get-file-stamp source))
          (cached-stamp (nth 3 cached)))
      (when (equal stamp cached-stamp)
        (message "fetched %s export from cache" source)
        (nth 2 cached)))))

(defun org-rnote--write-to-cache (source stamp exported)
  "Cache an EXPORTED version of SOURCE by validation STAMP.
SOURCE is the input file, EXPORT-PATH is the exported file path,
STAMP is a a unique stamp for SOURCE obtained with `org-rnote--get-file-stamp'."
  (message "writing %s 's export in cache" source)
  (let* ((container `(,@org-rnote--container-header
                      (file ,exported)
                      (elisp-data ,stamp)))
         (associated `(:file ,source)))
    (nth 2 (org-persist-write container associated))))

(defun org-rnote--maybe-compose (f g)
  "Compose functions F and G with conditional execution.
Calls F on G's result only when G returns non-nil or the input is non-nil."
  (lambda (x)
    (if (or x (funcall g x))
        (funcall f (funcall g x))
      x)))

(defun org-rnote--get-cached-or-export (source callback)
 "Retrieve or create cached export for SOURCE file, then call CALLBACK.
Calls CALLBACK with the export from the cache directory.
The export occurs asynchronously"
  (if-let* ((cached (org-rnote--get-cached source)))
      (funcall callback cached)
    (let* ((stamp (org-rnote--get-file-stamp source))
           (temp (make-temp-file "org-rnote-export" nil ".png"))
           (write-cache-fun (apply-partially #'org-rnote--write-to-cache
                                             source
                                             stamp)))
      ;; TODO Remove this. Rnote-cli appears to be bugged and
      ;; ignores the '--on-conflict overwrite' option in
      ;; non-interactive terminals
      (delete-file temp)
      (org-rnote--export-file source
                              temp
                              (list (org-rnote--maybe-compose callback
                                                              write-cache-fun)
                                    #'delete-file)))))

;;; File watch related functions

(defun org-rnote--remove-overlay-watch (ov after &rest _)
"Remove the file watch associated with overlay OV if it is set.
This function is intended to be used as an overlay post-modification hook.
AFTER must be non-nil to indicate that the modification has occurred."
  (when (and ov after)
    (when-let* ((watch (overlay-get ov 'watch-descriptor)))
               (file-notify-rm-watch watch))))

(defun org-rnote--add-file-watch (file ov)
 "Add a file watch for FILE and associate it with overlay OV.
This function sets up a file watch on FILE for `change` and
`attribute-change` events.  When the file is modified, the overlay
region is updated."
 (let* ((refresh-fun (lambda (_)
                       (message "Trying add-file-watch")
                       (when (overlay-buffer ov)
                         (message "Overlay exists")
                         (org-link-preview-region t
                                                  t
                                                  (overlay-start ov)
                                                  (overlay-end ov)))))
        (watch (file-notify-add-watch file '(change attribute-change) refresh-fun)))
   (overlay-put ov 'watch-descriptor watch)
   (overlay-put ov 'modification-hooks
                (append (overlay-get ov 'modification-hooks)
                        (list #'org-rnote--remove-overlay-watch)))))

(defun org-rnote--remove-watches (overlays)
  "Remove file watches from OVERLAYS that have a `watch-descriptor` property."
  (dolist (ov overlays)
    (when-let* ((watch-descriptor (overlay-get ov 'watch-descriptor)))
      (file-notify-rm-watch watch-descriptor))))

(defun org-rnote--remove-all-watches ()
  "Remove all file watches associated with link preview overlays."
  (org-rnote--remove-watches org-link-preview-overlays))

;;; Minor mode definition

;;;###autoload
(define-minor-mode org-rnote-preview-mode
  "Minor mode to enable Rnote previews in Org link previews.
When enabled, this mode patches `org-link-preview-file` to handle Rnote files
specially."
  :lighter nil
  (if (derived-mode-p 'org-mode)
      (progn
        (if org-rnote-preview-mode
            (add-hook 'kill-buffer-hook #'org-rnote--remove-all-watches nil t)
          (remove-hook 'kill-buffer-hook #'org-rnote--remove-all-watches t)
          (org-rnote--remove-all-watches)))
    (when org-rnote-preview-mode
      (message "This minor mode can only be enabled in org-mode or its derivatives."))))

;;; Advice functions

(defun org-rnote--preview-org-link (old-fun ov path link)
  "Advice for `org-link-preview-file' to handle Rnote files specially.

This function is intended to be used as an :around advice for
`org-link-preview-file'.

OLD-FUN is the original function being advised.
OV is the overlay where the preview is displayed.
PATH is the file path of the link.
LINK is the Org link object."
  (if (and org-rnote-preview-mode
           (display-graphic-p)
           (string-match-p rnote-file-regexp (expand-file-name
                                              (substitute-in-file-name path))))
      (org-rnote--get-cached-or-export path (lambda (cache-patch)
                                              (when (funcall old-fun ov cache-patch link)
                                                (org-rnote--add-file-watch path ov))))
    (funcall old-fun ov path link)))

(when (advice-member-p #'org-rnote--preview-org-link 'org-link-preview-file)
  (advice-remove 'org-link-preview-file #'org-rnote--preview-org-link))
(advice-add 'org-link-preview-file :around #'org-rnote--preview-org-link)

(defun org-rnote--link-preview-clear-with-watch (orig-fun &rest args)
  "Advice for `org-link-preview-clear' to remove file watches when clearing.

This function is intended to be used as an :around advice for
`org-link-preview-clear'.  It ensures that when link previews are
cleared, any associated file watches are also removed.

ORIG-FUN is the original function being advised.  ARGS are its argument."
  (if org-rnote-preview-mode
      (let ((before org-link-preview-overlays))
        (apply orig-fun args)
        (let ((after org-link-preview-overlays))
          (org-rnote--remove-watches (seq-difference before after))
         (apply orig-fun args)))))

(when (advice-member-p #'org-rnote--link-preview-clear-with-watch 'org-link-preview-clear)
  (advice-remove 'org-link-preview-clear #'org-rnote--link-preview-clear-with-watch))
(advice-add 'org-link-preview-clear :around #'org-rnote--link-preview-clear-with-watch)

(provide 'org-rnote)
;;; org-rnote.el ends here
