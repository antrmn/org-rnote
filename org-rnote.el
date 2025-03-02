;;; org-rnote.el --- Rnote integration for Org Mode  -*- lexical-binding: t -*-

;; Author: Antonio Romano <n58r@pm.me>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (org "9.7"))
;; Keywords: productivity, org, drawing
;; URL: https://github.com/antrmn/org-rnote

;;; Commentary:

;;; Code:

(require 'org)
(require 'org-macs)
(require 'ol)

(defconst rnote-file-regexp "\\.rnote$"
  "Regular expression matching Rnote files.")

(defgroup org-rnote nil
  "Customization for `org-rnote'."
  :group 'org
  :prefix "org-rnote-")

(defun org-rnote--export-file (input-file output-file callback)
  "Export INPUT-FILE to OUTPUT-FILE using rnote-cli and call CALLBACK when done.
The callback will receive the output file path as its argument."
  (let ((expanded-input (expand-file-name (substitute-in-file-name input-file)))
        (expanded-output (expand-file-name (substitute-in-file-name output-file))))
    (org-async-call (list "rnote-cli" "export" "selection" "--output-file"
                          expanded-output "all" expanded-input)
                    :failure "Rnote error."
                    :success (lambda (_ __ ___)
                               (funcall callback expanded-output)))))

(defun org-rnote--get-cached-or-export (path callback)
  "Get cached export of PATH or create it, then call CALLBACK with result path.
Uses file MD5 hash for caching in a temporary directory."
  (let ((expanded-path (expand-file-name (substitute-in-file-name path))))
    (org-async-call (list "md5sum" expanded-path)
                    :failure "Error"
                    :filter (lambda (_ content __)
                              (let* ((md5 (car (split-string content)))
                                     (filename (concat md5 ".png"))
                                     (cache-path (expand-file-name filename
                                                                   (temporary-file-directory))))
                                (if (file-exists-p cache-path)
                                    (funcall callback cache-path)
                                  (org-rnote--export-file expanded-path cache-path callback)))))))

(defun org-rnote--preview-org-link (old-fun ov path link)
  "Advice for `org-link-preview-file' to handle Rnote files specially.

This function is intended to be used as an :around advice for
`org-link-preview-file'.

OLD-FUN is the original function being advised.
OV is the overlay where the preview is displayed.
PATH is the file path of the link.
LINK is the Org link object."
  (when (display-graphic-p)
    (if (string-match-p rnote-file-regexp (expand-file-name (substitute-in-file-name path)))
        (org-rnote--get-cached-or-export path (lambda (cache-path)
                                                (funcall old-fun ov cache-path link)))
      (funcall old-fun ov path link))))

;;;###autoload
(define-minor-mode org-rnote-preview-mode
  "Global minor mode to enable Rnote previews in Org link previews.
When enabled, this mode advises `org-link-preview-file` to handle Rnote files
specially."
  :global t
  :lighter nil
  (if org-rnote-preview-mode
      (advice-add 'org-link-preview-file :around #'org-rnote--preview-org-link)
    (advice-remove 'org-link-preview-file #'org-rnote--preview-org-link)))


(provide 'org-rnote)

;;; org-rnote.el ends here
