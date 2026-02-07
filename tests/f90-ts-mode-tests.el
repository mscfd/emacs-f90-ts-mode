(require 'ert)
(require 'f90-ts-mode)

;; mode activation


(defconst f90-ts-mode-fortran-file-pattern "\\`[^.].*\\.f90\\'"
  "Regexp matching fortran test files.")

(defconst f90-ts-mode-compare-file-pattern "\\`[^.].*\\.cmp\\'"
  "Regexp matching files to compare output with.")


;;------------------------------------------------------------------------------
;; Auxiliary stuff

(defun f90-ts-mode-tests--indent-buffer ()
  "Indent current buffer, including the final line. This is relevant as
we also want to test the final line, which is relevant during typing
at the end of file."
  (indent-region (point-min) (point-max))
  (save-excursion
    (goto-char (point-max))
    (indent-for-tab-command)))


(defun f90-ts-mode-tests--resources-dir ()
  "Get the test resources directory.
Try the location of 'f90-ts-mode-tests, falling back to the current file."
  (let* ((test-file (or load-file-name
                        buffer-file-name))
         (test-dir (when test-file (file-name-directory test-file))))
    (if test-dir
        (expand-file-name "resources" test-dir)
      (error "Could not determine test directory."))))


(defun f90-ts-mode-tests--fortran-files ()
  "Get all fortran test files in the resources directory."
  (directory-files (f90-ts-mode-tests--resources-dir)
                   t
                   f90-ts-mode-fortran-file-pattern))


;;------------------------------------------------------------------------------
;; generate compare files for automatic testing

(defun f90-ts-mode-tests--compare-file (f90-file)
  "Get the compare-with file name for an F90-FILE."
  (file-name-with-extension f90-file ".cmp"))


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
  (f90-ts-mode-tests--run-with-testing
   file
   (lambda ()
     (font-lock-ensure)
     (f90-ts-mode-tests--indent-buffer)
     (let ((state (f90-ts-mode-tests--capture-buffer-state))
           (compare-file (f90-ts-mode-tests--compare-file file)))
       (f90-ts-mode-tests--print-state state compare-file)
       (message "saved compare file for %s" (file-name-nondirectory file))))))


(defun f90-ts-mode-tests-update ()
  "Generate compare files for all resource files."
  (interactive)
  (dolist (file (f90-ts-mode-tests--fortran-files))
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
;; custom variable handling for testing

(defconst f90-ts-mode-tests-managed-custom
  '(f90-ts-indent-toplevel
    f90-ts-indent-contain
    f90-ts-indent-block
    f90-ts-indent-continued
    f90-ts-indent-lists-region
    f90-ts-special-comment-regexp
    require-final-newline)
  "Custom variables whose values can be temporarily overridden.")


(defvar f90-ts-mode-tests-saved-custom-values
  '()
  "Alist of saved custom variable values for temporary overrides.")


(defun f90-ts-mode-tests-save-custom ()
  "Save current custom values of `f90-ts-mode-tests-managed-custom`."
  (setq f90-ts-mode-tests-saved-custom-values
        (mapcar (lambda (var)
                  (cons var (symbol-value var)))
                f90-ts-mode-tests-managed-custom)))


(defconst f90-ts-mode-tests-custom-testing
  '((f90-ts-indent-toplevel . 1)
    (f90-ts-indent-contain . 3)
    (f90-ts-indent-block . 5)
    (f90-ts-indent-continued . 7)
    (f90-ts-indent-lists-region . keep-or-first)
    (f90-ts-special-comment-regexp . "! \\(result\\|=\\{10\\}\\|arguments\\|local\\)$")
    (require-final-newline . nil)
    )
  "Alist of custom variable values for testing purposes.")


(defun f90-ts-mode-tests-set-custom-testing ()
  "Save current values and apply temporary ones for testing purposes."
  (f90-ts-mode-tests-save-custom)
  (dolist (entry f90-ts-mode-tests-custom-testing)
    (set (car entry) (cdr entry))))


(defun f90-ts-mode-tests-restore-custom ()
  "Restore previously saved custom variable values."
  (unless f90-ts-mode-tests-saved-custom-values
    (error "f90-ts-mode-tests: No saved custom variable state to restore"))
  (dolist (entry f90-ts-mode-tests-saved-custom-values)
    (set (car entry) (cdr entry)))
  (setq f90-ts-mode-tests-saved-custom-values nil))


(defmacro f90-ts-mode-tests-with-custom-testing (&rest body)
  `(let ((f90-ts-mode-tests-saved-custom-values nil))
     (unwind-protect
         (progn
           (f90-ts-mode-tests-set-custom-testing)
           ,@body)
       (f90-ts-mode-tests-restore-custom))))


(defun f90-ts-mode-tests--run-with-testing (file body-fn)
  "Run BODY-FN on FILE with custom values for testing. For example, use
different indentation values for each indentation type, so that
selection of indentation rules is tested properly."
  (f90-ts-mode-tests-with-custom-testing
   (let ((f90-ts-log-categories '()))
     (with-temp-buffer
       (insert-file-contents file)
       (f90-ts-mode)
       (setq-local indent-tabs-mode nil)
       (treesit-parser-create 'fortran)
       (font-lock-ensure)
       (funcall body-fn)))))


;;------------------------------------------------------------------------------

(ert-deftest f90-ts-mode-test-activates ()
  "Check whether f90-ts-mode properly starts."
  (with-temp-buffer
    (insert "program activation\nend program activation\n")
    (f90-ts-mode)
    (should (derived-mode-p 'f90-ts-mode))))


(defun f90-ts-mode-tests-register ()
  "Dynamically generate ERT tests for all resource files."
  (cl-loop
   for file in (f90-ts-mode-tests--fortran-files)
   for test-name = (intern (format "f90-ts-mode-test/%s"
                                   (file-name-nondirectory file)))
   do (eval
       `(ert-deftest ,test-name ()
          (let ((label (f90-ts-mode-test-single ,file nil)))
            (when label
              (ert-fail (format "%s differs in %s"
                                label
                                ,(file-name-nondirectory file)))
              ))
          ))
   ))

(ert-deftest f90-ts-mode-test-intrinsic-vs-array ()
  "Test that intrinsics are highlighted as builtins, while arrays/unknowns are not."
  (with-temp-buffer
    ;; test f90 code
    (insert "program test_builtin
  integer :: i
  real :: x, y(10)
  if (allocated(y)) then
     x = min(1.0, 2.0)
     y(1) = unknown(x)
  end if
end program")
    (f90-ts-mode)
    (font-lock-ensure)

    ;; 1. allocated Check (Expect: builtin)
    (goto-char (point-min))
    (search-forward "allocated")
    (goto-char (match-beginning 0))
    (should (eq (get-text-property (point) 'face) 'font-lock-builtin-face))

    ;; 2. min Check (Expect: builtin)
    (goto-char (point-min))
    (search-forward "min")
    (goto-char (match-beginning 0))
    (should (eq (get-text-property (point) 'face) 'font-lock-builtin-face))

    ;; 3. Array 'y' Check (Expect: nil / no function color)
    (goto-char (point-min))
    (search-forward "y(1)")
    (goto-char (match-beginning 0))
    (should (eq (get-text-property (point) 'face) nil))

    ;; 4. Unknown function Check (Expect: nil)
    (goto-char (point-min))
    (search-forward "unknown")
    (goto-char (match-beginning 0))
    (should (eq (get-text-property (point) 'face) nil))))

(ert-deftest f90-ts-mode-test-indent-preproc ()
  "Test indentation for preprocessor directives (#ifdef) and their contents."
  (f90-ts-mode-tests-set-custom-testing)
  ;; Case 1: Module Scope
  (with-temp-buffer
    (f90-ts-mode)
    (insert "module my_mod
public :: foo
#ifdef HOGE
public :: bar
public :: baz
#endif
end module my_mod")
    (f90-ts-mode-tests--indent-buffer)
    (should (string= (buffer-string) "module my_mod
 public :: foo
#ifdef HOGE
 public :: bar
 public :: baz
#endif
end module my_mod")))

  ;; Case 2: Subroutine Scope
  (with-temp-buffer
    (f90-ts-mode)
    (insert "subroutine test_sub
integer :: i
#ifdef DEBUG
print *, 'debug mode'
i = i + 1
#else
i = 0
#endif
end subroutine test_sub")
    (f90-ts-mode-tests--indent-buffer)
    (should (string= (buffer-string) "subroutine test_sub
     integer :: i
#ifdef DEBUG
     print *, 'debug mode'
     i = i + 1
#else
     i = 0
#endif
end subroutine test_sub")))
  ;; Case 3: Nested Preprocessors (Great-Grand-Parent logic)
  (with-temp-buffer
    (f90-ts-mode)
    (setq-local indent-tabs-mode nil)
    (setq-local f90-ts-indent-block 2)
    (insert "program main
#ifdef A
call func_a()
#ifdef B
call func_b()
#elifdef C
call func_c()
#endif
#endif
end program main")
    (f90-ts-mode-tests--indent-buffer)
    (should (string= (buffer-string) "program main
#ifdef A
 call func_a()
#ifdef B
 call func_b()
#elifdef C
 call func_c()
#endif
#endif
end program main")))
)

;; (ert-deftest f90-ts-mode-test-resources ()
;;   "Run all f90 files in folder resources and compare with pre-generated compare files."
;;   (let ((f90-files (f90-ts-mode-tests--fortran-files)))
;;     (dolist (file f90-files)
;;       (let ((label (f90-ts-mode-test-single file nil)))
;;         (when label
;;           (ert-fail
;;            (format "%s differs in %s"
;;                    label
;;                    (file-name-nondirectory file))))))))


(defun f90-ts-mode-test-single (file diff)
  "Run a single f90 file in folder resources and compare with pre-generated compare files.
Return nil if test passes, otherwise the test category (indentation, font-lock etc.) which failed."
  (interactive
   (list (let* ((rdir (file-name-as-directory (f90-ts-mode-tests--resources-dir)))
                (files (f90-ts-mode-tests--fortran-files)))
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
            (current   (f90-ts-mode-tests--run-with-testing
                        file
                        (lambda ()
                          (f90-ts-mode-tests--indent-buffer)
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


;; dynamically register tests
(f90-ts-mode-tests-register)


(provide 'f90-ts-mode-tests)
