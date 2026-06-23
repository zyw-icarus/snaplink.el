;;; snaplink-test.el --- Tests for snaplink.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'snaplink)

(defmacro snaplink-test--with-temp-file-buffer (file-name content &rest body)
  "Visit FILE-NAME with CONTENT, then run BODY."
  (declare (indent 2) (debug (form form body)))
  `(with-temp-buffer
     (setq buffer-file-name ,file-name)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(defun snaplink-test--file (name)
  "Return a test file path for NAME."
  (expand-file-name name temporary-file-directory))

(ert-deftest snaplink-object-key-removes-leading-dot-slash ()
  (should (equal (snaplink--object-key "./assets/a.png") "assets/a.png"))
  (should (equal (snaplink--object-key "assets/a.png") "assets/a.png")))

(ert-deftest snaplink-public-url-joins-cleanly ()
  (let ((snaplink-public-base-url "https://img.example.com/"))
    (should (equal (snaplink--public-url "assets/a.png")
                   "https://img.example.com/assets/a.png")))
  (let ((snaplink-public-base-url "https://img.example.com"))
    (should (equal (snaplink--public-url "assets/a.png")
                   "https://img.example.com/assets/a.png"))))

(ert-deftest snaplink-validate-path-rejects-invalid-forms ()
  (should-error (snaplink--validate-path "") :type 'user-error)
  (should-error (snaplink--validate-path "/tmp/a.png") :type 'user-error)
  (should-error (snaplink--validate-path "~/a.png") :type 'user-error)
  (should-error (snaplink--validate-path "../a.png") :type 'user-error)
  (should-error (snaplink--validate-path "foo/../a.png") :type 'user-error))

(ert-deftest snaplink-allowed-extension-checks-case-insensitively ()
  (let ((snaplink-allowed-extensions '("png" "jpg")))
    (should (snaplink--allowed-extension-p "foo.PNG"))
    (should-not (snaplink--allowed-extension-p "foo.txt"))))

(ert-deftest snaplink-path-bounds-detect-org-link ()
  (snaplink-test--with-temp-file-buffer "/tmp/post.org" "[[./assets/a.png]]"
    (search-forward "assets")
    (let ((bounds (snaplink--path-bounds-at-point)))
      (should bounds)
      (should (equal (buffer-substring-no-properties (car bounds) (cdr bounds))
                     "./assets/a.png")))))

(ert-deftest snaplink-path-bounds-detect-markdown-image-link ()
  (snaplink-test--with-temp-file-buffer "/tmp/post.md" "![](assets/a.png)"
    (search-forward "a.png")
    (let ((bounds (snaplink--path-bounds-at-point)))
      (should bounds)
      (should (equal (buffer-substring-no-properties (car bounds) (cdr bounds))
                     "assets/a.png")))))

(ert-deftest snaplink-path-bounds-detect-markdown-link ()
  (snaplink-test--with-temp-file-buffer "/tmp/post.md" "[](assets/a.png)"
    (search-forward "assets")
    (let ((bounds (snaplink--path-bounds-at-point)))
      (should bounds)
      (should (equal (buffer-substring-no-properties (car bounds) (cdr bounds))
                     "assets/a.png")))))

(ert-deftest snaplink-path-bounds-requires-point-on-path ()
  (snaplink-test--with-temp-file-buffer "/tmp/post.org" "[[./assets/a.png]]"
    (goto-char (point-min))
    (should-not (snaplink--path-bounds-at-point))))

(ert-deftest snaplink-resolve-local-file-uses-buffer-directory ()
  (let* ((dir (make-temp-file "snaplink-dir-" t))
         (subdir (expand-file-name "assets" dir))
         (post (expand-file-name "post.org" dir))
         (image (expand-file-name "a.png" subdir)))
    (make-directory subdir t)
    (write-region "" nil image nil 'silent)
    (snaplink-test--with-temp-file-buffer post "[[./assets/a.png]]"
      (should (equal (snaplink--resolve-local-file "./assets/a.png") image)))))

(ert-deftest snaplink-upload-at-point-replaces-org-path-only ()
  (let* ((dir (make-temp-file "snaplink-dir-" t))
         (subdir (expand-file-name "assets" dir))
         (post (expand-file-name "post.org" dir))
         (image (expand-file-name "a.png" subdir))
         (snaplink-rclone-remote "r2:bucket")
         (snaplink-public-base-url "https://img.example.com")
         (snaplink-allowed-extensions '("png"))
         (call-args nil))
    (make-directory subdir t)
    (write-region "" nil image nil 'silent)
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest args)
                 (setq call-args args)
                 0)))
      (snaplink-test--with-temp-file-buffer post "before [[./assets/a.png]] after"
        (search-forward "a.png")
        (snaplink-upload-at-point)
        (should (equal (buffer-string)
                       "before [[https://img.example.com/assets/a.png]] after"))
        (should (equal call-args
                       (list "rclone" nil (get-buffer-create "*snaplink-rclone*") t
                             "copyto" image "r2:bucket/assets/a.png")))))))

(ert-deftest snaplink-upload-at-point-replaces-markdown-image-path-only ()
  (let* ((dir (make-temp-file "snaplink-dir-" t))
         (subdir (expand-file-name "assets" dir))
         (post (expand-file-name "post.md" dir))
         (image (expand-file-name "a.png" subdir))
         (snaplink-rclone-remote "r2:bucket")
         (snaplink-public-base-url "https://img.example.com/")
         (snaplink-allowed-extensions '("png")))
    (make-directory subdir t)
    (write-region "" nil image nil 'silent)
    (cl-letf (((symbol-function 'process-file) (lambda (&rest _) 0)))
      (snaplink-test--with-temp-file-buffer post "![](./assets/a.png)"
        (search-forward "assets")
        (snaplink-upload-at-point)
        (should (equal (buffer-string)
                       "![](https://img.example.com/assets/a.png)"))))))

(ert-deftest snaplink-upload-at-point-stops-on-parent-reference ()
  (let ((snaplink-rclone-remote "r2:bucket")
        (snaplink-public-base-url "https://img.example.com")
        (snaplink-allowed-extensions '("png")))
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest _)
                 (ert-fail "process-file should not run"))))
      (snaplink-test--with-temp-file-buffer "/tmp/post.org" "[[../assets/a.png]]"
        (search-forward "../")
        (should-error (snaplink-upload-at-point) :type 'user-error)
        (should (equal (buffer-string) "[[../assets/a.png]]"))))))

(ert-deftest snaplink-upload-at-point-stops-on-missing-file ()
  (let ((snaplink-rclone-remote "r2:bucket")
        (snaplink-public-base-url "https://img.example.com")
        (snaplink-allowed-extensions '("png")))
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest _)
                 (ert-fail "process-file should not run"))))
      (snaplink-test--with-temp-file-buffer "/tmp/post.org" "[[./assets/a.png]]"
        (search-forward "assets")
        (should-error (snaplink-upload-at-point) :type 'user-error)
        (should (equal (buffer-string) "[[./assets/a.png]]"))))))

(ert-deftest snaplink-upload-at-point-preserves-buffer-on-upload-failure ()
  (let* ((dir (make-temp-file "snaplink-dir-" t))
         (subdir (expand-file-name "assets" dir))
         (post (expand-file-name "post.org" dir))
         (image (expand-file-name "a.png" subdir))
         (snaplink-rclone-remote "r2:bucket")
         (snaplink-public-base-url "https://img.example.com")
         (snaplink-allowed-extensions '("png")))
    (make-directory subdir t)
    (write-region "" nil image nil 'silent)
    (cl-letf (((symbol-function 'process-file) (lambda (&rest _) 7)))
      (snaplink-test--with-temp-file-buffer post "[[./assets/a.png]]"
        (search-forward "assets")
        (should-error (snaplink-upload-at-point) :type 'error)
        (should (equal (buffer-string) "[[./assets/a.png]]"))))))

(ert-deftest snaplink-upload-at-point-rejects-unsupported-extension ()
  (let ((snaplink-rclone-remote "r2:bucket")
        (snaplink-public-base-url "https://img.example.com")
        (snaplink-allowed-extensions '("png")))
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest _)
                 (ert-fail "process-file should not run"))))
      (snaplink-test--with-temp-file-buffer "/tmp/post.org" "[[./assets/a.txt]]"
        (search-forward "assets")
        (should-error (snaplink-upload-at-point) :type 'user-error)))))

(provide 'snaplink-test)

;;; snaplink-test.el ends here
