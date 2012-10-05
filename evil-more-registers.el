(require 'evil)
(eval-when-compile (require 'cl))

(defvar evil-last-undo-entry nil)
(make-variable-buffer-local 'evil-last-undo-entry)
(defvar evil-current-insertions nil)
(make-variable-buffer-local 'evil-current-insertions)
(defvar evil-last-insertion-command nil)
(make-variable-buffer-local 'evil-last-insertion-command)
(defvar evil-last-insertion nil)
(defvar evil-last-small-deletion nil)

;;; track insertion

(defun evil-concat-ranges (ranges)
  (let ((range (car-safe ranges)) (ranges (cdr ranges)) r)
    (while (and range (setq r (car-safe ranges)))
      (setq range
            (cond ((and (= (cdr r) (car range))) (cons (car r) (cdr range)))
                  ((and (= (cdr range) (car r))) (cons (car range) (cdr r)))))
      (setq ranges (cdr ranges)))
    range))

(defun evil-record-last-insertion ()
  (let* ((list buffer-undo-list) e ins insertions)
    (while (and list (not (setq e (car-safe list)))) (setq list (cdr list)))
    (when (and (not (memq this-command '(evil-open-above evil-open-below)))
               (listp list)
               (or (not (eq e (car evil-last-undo-entry)))
                   (not (equal e (cdr evil-last-undo-entry)))))
      (setq evil-last-undo-entry
            (cons e (or (cond ((consp e) (cons (car e) (cdr e)))
                              ((listp e) (copy-sequence e))) e)))
      (while (and (setq e (car-safe list)) (not ins))
        (when (and (consp e) (integerp (car e)) (integerp (cdr e)))
          (setq ins e))
        (setq list (cdr list)))
      (when ins
        (when (eq last-command evil-last-insertion-command)
          (setq insertions (cdr evil-current-insertions)))
        (add-to-list 'insertions ins)
        (let ((range (evil-concat-ranges insertions)))
          (unless range (setq range ins insertions (list ins)))
          (setq evil-current-insertions
                (cons (buffer-substring-no-properties (car range) (cdr range))
                      insertions)))
        (setq evil-last-insertion-command this-command)))
    ins))
(put 'evil-record-last-insertion 'permanent-local-hook t)
(defun evil-start-record-last-insertion ()
  (setq evil-current-insertions (cons nil nil))
  (add-hook 'post-command-hook #'evil-record-last-insertion nil t))
(defun evil-stop-record-last-insertion ()
  (setq evil-last-insertion (car evil-current-insertions))
  (remove-hook 'post-command-hook #'evil-record-last-insertion t))
(add-hook 'evil-insert-state-entry-hook #'evil-start-record-last-insertion)
(add-hook 'evil-insert-state-exit-hook #'evil-stop-record-last-insertion)

;;; track deletion

(defadvice evil-delete
  (before evil-track-deletion (beg end &optional ty reg handler) activate)
  (let ((str (buffer-substring-no-properties beg end)))
    (unless (string-match-p "[\r\n]" str)
      (setq evil-last-small-deletion str))))

;;; register functions

(defalias 'evil-core-get-register (symbol-function 'evil-get-register))

(defun evil-get-register (register &optional noerror)
  "Return contents of REGISTER.
Signal an error if empty, unless NOERROR is non-nil.

Supported registers are:
  \"  the unnamed register
  *  the clipboard contents
  +  the clipboard contents
  %  the current file name
  #  the alternate file name
  /  the last search pattern
  :  the last command line
  .  the last inserted text
  -  the last small (less than a line) delete
  =  the expression register"
  (cond
   ((not (characterp register)) (evil-core-get-register register noerror))
   ((= register ?%) ; the current file name
    (or (buffer-file-name) (unless noerror (error "No file name"))))
   ((= register ?#) ; the alternate file name
    (or (with-current-buffer (other-buffer) (buffer-file-name))
        (unless noerror (error "No file name"))))
   ((= register ?/) ; the last search pattern
    (or (car-safe evil-ex-search-history)
        (unless noerror (error "No previous regular expression"))))
   ((= register ?:) ; the last comand line
    (or (car-safe evil-ex-history)
        (unless noerror (error "No previous command line"))))
   ((= register ?.) ; the last inserted text
    evil-last-insertion)
   ((= register ?-) ; the last small (less than a line) delete
    evil-last-small-deletion)
   ((= register ?=) ; the expression register
    (let ((enable-recursive-minibuffers t))
      (eval (car (read-from-string (read-string "="))))))
   ((= register ?_) ; black hole: always empty
    "")
   (t (evil-core-get-register register noerror))))

(defalias 'evil-orig-get-register (symbol-function 'evil-get-register))

(defun evil-get-spec-register (register &optional noerror)
  "Return contents of REGISTER.
Signal an error if empty, unless NOERROR is non-nil.

Support some registers listed below in addition to
`evil-get-register'.
    the file name under the cursor
    the expanded file name under the cursor
    the word under the cursor
    the WORD under the cursor"
  (cond
   ((or (= register  6)  ; ^F the filename under the cursor
        (= register 16)) ; ^P the expanded filename under the cursor
    (let ((file (thing-at-point 'filename)))
      (or (and file (= register 16) (expand-file-name file)) file)))
   ((or (= register 23)  ; ^W the word under the cursor
        (= register 1))  ; ^A the WORD under the cursor
    (let* ((word (if (= register 1) #'evil-move-WORD #'evil-move-word))
           (range (evil-inner-object-range nil nil nil nil word)))
      (buffer-substring-no-properties (nth 0 range) (nth 1 range))))
   (t (evil-orig-get-register register noerror))))

(defun evil-ex-paste-from-register (&optional register)
  "Paste from REGISTER in command line."
  (interactive)
  (flet ((evil-get-register (register &optional noerror)
           (with-current-buffer evil-ex-current-buffer
             (evil-get-spec-register register noerror))))
    (if (called-interactively-p 'any)
        (call-interactively #'evil-paste-from-register)
      (evil-paste-from-register register))))

(provide 'evil-more-registers)
;;; evil-more-registers.el ends here
