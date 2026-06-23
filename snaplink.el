;;; snaplink.el --- Upload local image links and replace them with public URLs -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Mox
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: files, multimedia, tools

;; This file is not part of GNU Emacs.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;; Upload a local image path at point with rclone and replace that path
;; with a public URL in Org or Markdown links.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup snaplink nil
  "Upload local image links and replace them with public URLs."
  :group 'files
  :prefix "snaplink-")

(defcustom snaplink-rclone-remote ""
  "Rclone remote destination, for example \"r2:blog-images\"."
  :type 'string)

(defcustom snaplink-public-base-url ""
  "Public base URL used to build the final image URL."
  :type 'string)

(defcustom snaplink-allowed-extensions
  '("png" "jpg" "jpeg" "gif" "webp" "svg" "avif")
  "Allowed file extensions for uploads."
  :type '(repeat string))

(defconst snaplink--supported-link-patterns
  '((org . "\\[\\[\\([^]\n]+\\)\\]\\]")
    (markdown-image . "!\\[[^]\n]*\\](\\([^)\n]+\\))")
    (markdown-link . "\\[[^]\n]*\\](\\([^)\n]+\\))"))
  "Patterns used to detect supported link forms at point.")

(defun snaplink--assert-configured ()
  "Ensure required configuration is present."
  (unless (and (stringp snaplink-rclone-remote)
               (not (string-empty-p snaplink-rclone-remote)))
    (user-error "snaplink-rclone-remote is not configured"))
  (unless (and (stringp snaplink-public-base-url)
               (not (string-empty-p snaplink-public-base-url)))
    (user-error "snaplink-public-base-url is not configured")))

(defun snaplink--path-bounds-at-point ()
  "Return the bounds of the supported path at point.
The return value is a cons cell of buffer positions."
  (save-excursion
    (let ((origin (point))
          (line-start (line-beginning-position))
          (line-end (line-end-position))
          result)
      (dolist (entry snaplink--supported-link-patterns)
        (unless result
          (goto-char line-start)
          (while (and (not result)
                      (re-search-forward (cdr entry) line-end t))
            (let ((beg (match-beginning 1))
                  (end (match-end 1)))
              (when (and (<= beg origin) (<= origin end))
                (setq result (cons beg end)))))))
      result)))

(defun snaplink--path-at-point ()
  "Return the local path at point."
  (let ((bounds (snaplink--path-bounds-at-point)))
    (unless bounds
      (user-error "Point is not on a supported local path"))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

(defun snaplink--absolute-path-p (path)
  "Return non-nil when PATH is absolute."
  (or (file-name-absolute-p path)
      (string-prefix-p "~" path)))

(defun snaplink--contains-parent-reference-p (path)
  "Return non-nil when PATH contains a parent directory reference."
  (member ".." (split-string path "/" t)))

(defun snaplink--validate-path (path)
  "Validate PATH for Snaplink usage."
  (when (string-empty-p path)
    (user-error "Empty path is not allowed"))
  (when (snaplink--absolute-path-p path)
    (user-error "Absolute paths are not supported: %s" path))
  (when (snaplink--contains-parent-reference-p path)
    (user-error "Parent directory references are not supported: %s" path))
  path)

(defun snaplink--resolve-local-file (path)
  "Resolve PATH relative to `buffer-file-name'."
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (let ((local-file (expand-file-name path (file-name-directory buffer-file-name))))
    (unless (file-exists-p local-file)
      (user-error "Local file does not exist: %s" path))
    local-file))

(defun snaplink--allowed-extension-p (path)
  "Return non-nil when PATH has an allowed file extension."
  (let ((ext (downcase (or (file-name-extension path) ""))))
    (member ext snaplink-allowed-extensions)))

(defun snaplink--ensure-allowed-extension (path)
  "Raise a `user-error' when PATH has a disallowed extension."
  (unless (snaplink--allowed-extension-p path)
    (user-error "Unsupported file extension: %s" path)))

(defun snaplink--object-key (path)
  "Build an object key from PATH."
  (string-remove-prefix "./" path))

(defun snaplink--public-url (object-key)
  "Build the public URL for OBJECT-KEY."
  (concat (string-remove-suffix "/" snaplink-public-base-url)
          "/"
          object-key))

(defun snaplink--upload-file (local-file object-key)
  "Upload LOCAL-FILE to OBJECT-KEY using rclone."
  (let* ((remote (string-remove-suffix "/" snaplink-rclone-remote))
         (destination (concat remote "/" object-key))
         (buffer (get-buffer-create "*snaplink-rclone*"))
         (exit-code (process-file "rclone" nil buffer t "copyto" local-file destination)))
    (unless (zerop exit-code)
      (error "rclone upload failed with exit code %s" exit-code))))

;;;###autoload
(defun snaplink-upload-at-point ()
  "Upload the local image path at point and replace it with a public URL."
  (interactive)
  (snaplink--assert-configured)
  (let* ((bounds (or (snaplink--path-bounds-at-point)
                     (user-error "Point is not on a supported local path")))
         (raw-path (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (snaplink--validate-path raw-path)
    (snaplink--ensure-allowed-extension raw-path)
    (let* ((local-file (snaplink--resolve-local-file raw-path))
           (object-key (snaplink--object-key raw-path))
           (url (snaplink--public-url object-key)))
      (snaplink--upload-file local-file object-key)
      (save-excursion
        (goto-char (car bounds))
        (delete-region (car bounds) (cdr bounds))
        (insert url))
      (message "Snaplinked %s" url))))

(provide 'snaplink)

;;; snaplink.el ends here
