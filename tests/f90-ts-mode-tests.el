(require 'ert)
(require 'f90-ts-mode)

;; mode activation

(defun f90-ts-mode-tests ()
  "Run all tests."
  (ert #'f90-ts-mode-activates)
  (ert #'f90-ts-mode-resources)
  )


(ert-deftest f90-ts-mode-test-activates ()
  (with-temp-buffer
    (insert "program activation\nend program activation\n")
    (f90-ts-mode)
    (should (derived-mode-p 'f90-ts-mode))))


(ert-deftest f90-ts-mode-test-resources ()
  "Run all f90 files in folder resources and compare with pre-generated compare files."
  (dolist (file (directory-files (f90-ts-mode-tests--resources-dir) t "\\.f90\\'"))
    (let ((label (f90-ts-mode-test-single file nil)))
      (when label
        (ert-fail
         (format "%s differs in %s"
                 label
                 (file-name-nondirectory file)))))))


(defun f90-ts-mode-test-single (file diff)
  "Run a single f90 file in folder resources and compare with pre-generated compare files.
Return nil if test passes, otherwise the test category (indentation, font-lock etc.) which failed."
  (interactive
   (list (let* ((rdir (file-name-as-directory (f90-ts-mode-tests--resources-dir)))
                (files (directory-files rdir nil "\\.f90\\'")))
           (expand-file-name
            (completing-read "select F90 file: " files nil t)
            rdir))
         (read-string "diff command (empty for none): "
                      f90-ts-mode-tests-diff-command)))

  (let ((compare-file (f90-ts-mode-tests--compare-file file)))
    (ert-info ((format "testing file: %s" (file-name-nondirectory file)))
      (should (file-exists-p compare-file))
      (let ((compare (with-temp-buffer
                       (insert-file-contents compare-file)
                       (read (current-buffer))))
            (current   (f90-ts-mode-tests--with-test-indent
                        file
                        (lambda ()
                          (indent-region (point-min) (point-max))
                          (f90-ts-mode-tests--capture-buffer-state)))))
        (let ((result (or (f90-ts-mode-tests--compare-and-diff file compare current :indentation diff)
                          (f90-ts-mode-tests--compare-and-diff file compare current :font-lock diff)
                          (f90-ts-mode-tests--compare-and-diff file compare current :tree diff))))
          (when (called-interactively-p 'any)
            (if result
                (message "test file %s failed" (file-name-nondirectory file))
              (message "test file %s succeeded" (file-name-nondirectory file))))

          result)
        ))))


;;------------------------------------------------------------------------------
;; generate compare files for automatic testing

(defun f90-ts-mode-tests--compare-file (f90-file)
  "Get the compare-with file name for an F90-FILE."
  (concat (file-name-sans-extension f90-file) ".compare"))


(defun f90-ts-mode-tests--faces ()
  "Return the list of all faces found in the curren buffer."
  (let ((faces)
        (fcollect)
        (fstart))
    (goto-char (point-min))
    (setq fstart (point))

    (while (not (eobp))
      ;; get face at point and compare with currently collected face fcollect
      (let ((fpoint (get-text-property (point) 'face)))
        (when (and (or fcollect fpoint)
                   (not (eq fpoint fcollect)))
          ;; we have at least one face, at previous or current position, and if we
          ;; have two faces, they are not equal
          (when fcollect
            ;; we are past currently collected face, store it in faces
            ;; (face at point fpoint might or might not be nil)
            (push (list (line-number-at-pos fstart) fstart (point) fcollect) faces))

          ;; in any case, save the new face at point and its starting position
          ;; (even if nil, then we remember there there is no face currently collected)
          (setq fcollect fpoint)
          (setq fstart (point))))

      (forward-char))
    (nreverse faces)))


(defun f90-ts-mode-tests--capture-buffer-state ()
  "Capture the current buffer state for comparison purposes."
  (list
   :indentation (substring-no-properties (buffer-string))
   :font-lock (f90-ts-mode-tests--faces)
   :tree (treesit-node-string (treesit-buffer-root-node))))


(defun f90-ts-mode-tests--print-state (state state-file)
  "Print STATE ot STATE-FILE in a way suitable for comparison as well as human reading."
  (with-temp-file state-file
    (let ((print-length nil)
          (print-level nil)
          (print-circle nil)
          (print-escape-newlines nil))
      ;; this short form is not very readable, add some newlines
      ;; (still readable by read)
      ;;(prin1 state (current-buffer))
      (insert "(")
      (prin1 :indentation (current-buffer))
      (insert "\n")
      (prin1 (plist-get state :indentation) (current-buffer))
      (insert "\n\n")
      (prin1 :font-lock (current-buffer))
      (insert "\n")
      (prin1 (plist-get state :font-lock) (current-buffer))
      (insert "\n\n")
      (prin1 :tree (current-buffer))
      (insert "\n")
      (prin1 (plist-get state :tree) (current-buffer))
      (insert ")")
      (insert "\n")
      )))


(defun f90-ts-mode-tests--update-compare (file)
  "Generate and save compare file for FILE."
  (f90-ts-mode-tests--with-test-indent
   file
   (lambda ()
     (font-lock-ensure)
     (indent-region (point-min) (point-max))
     (let ((state (f90-ts-mode-tests--capture-buffer-state))
           (compare-file (f90-ts-mode-tests--compare-file file)))
       (f90-ts-mode-tests--print-state state compare-file)
       (message "saved compare file for %s" (file-name-nondirectory file))))))


(defun f90-ts-mode-tests-update ()
  "Generate compare files for all resource files."
  (interactive)
  (dolist (file (directory-files (f90-ts-mode-tests--resources-dir) t "\\.f90\\'"))
    (f90-ts-mode-tests--update-compare file)))


;;------------------------------------------------------------------------------
;; compare with compare files for automatic testing

(defcustom f90-ts-mode-tests-diff-command "kompare"
  "External diff tool to use for test comparisons.
Can be 'kompare', 'meld', 'kdiff3', 'diffuse', etc."
  :type 'string
  :group 'f90-ts-mode)


(defun f90-ts-mode-tests--show-diff (file compare current label diff)
  "Show diff between COMPARE and CURRENT using external tool."
  (let* ((basename (file-name-sans-extension (file-name-nondirectory file)))
         (compare-file (make-temp-file (format "f90-ts-mode-compare-%s-%s-" basename label)))
         (current-file (make-temp-file (format "f90-ts-mode-current-%s-%s-" basename label)))
         (printcmd (pcase label
                     ("indentation" (lambda (obj _buffer) (insert obj)))
                     (_             #'pp))))
    (unwind-protect
        (progn
          (with-temp-file compare-file
            (funcall printcmd compare-val (current-buffer)))
          (with-temp-file current-file
            (funcall printcmd current-val (current-buffer)))
          (call-process diff nil 0 nil compare-file current-file)))
    ))


(defun f90-ts-mode-tests--compare-and-diff (file compare current key diff)
  "Compare COMPARE and CURRENT for KEY, show diff if different.
KEY is a plist key like :indentation, :font-lock, :tree.
FILE is the source file being tested.
If DIFF is non-nil it should be string of a diff tool to show differences
with the compare state.
If failure, then return the label which failed, otherwise nil."
  (let* ((compare-val (plist-get compare key))
         (current-val (plist-get current key))
         (label (substring (symbol-name key) 1)) ; remove leading ":" of key
         (cmp (equal compare-val current-val)))
    ;;(message "cmp %s" compare-val)
    ;;(message "cur %s" current-val)
    (unless cmp
      (when diff (f90-ts-mode-tests--show-diff file compare-val current-val label diff)))
    ;; return label in case of an failure
    (unless cmp label)))



;;------------------------------------------------------------------------------
;; Auxiliary stuff

(defun f90-ts-mode-tests--resources-dir ()
  "Get the test resources directory. Use the location of f90-ts-mode-tests"
  (let* ((test-file (symbol-file 'f90-ts-mode-tests 'defun))
         (test-dir (file-name-directory test-file)))
    (expand-file-name "resources" test-dir)))


(defun f90-ts-mode-tests--with-test-indent (file body-fn)
  "Run BODY-FN on FILE with test indentation values. Use different values for each
indentation type, so that selection of indentation rules is tested properly."
  (let ((f90-ts-indent-toplevel 1)
        (f90-ts-indent-contain 3)
        (f90-ts-indent-block 5)
        (f90-ts-indent-continued 7))
    (with-temp-buffer
      (insert-file-contents file)
      (f90-ts-mode)
      (setq-local indent-tabs-mode nil)
      (treesit-parser-create 'fortran)
      (font-lock-ensure)
      (funcall body-fn))))

(provide 'f90-ts-mode-tests)
