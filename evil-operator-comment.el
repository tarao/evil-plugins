(require 'evil)

(defgroup evil-operator-comment nil
  "Comment/uncomment operator for Evil"
  :prefix "evil-operator-comment-"
  :group 'evil)

(defcustom evil-operator-comment-key (kbd "C")
  "A key for comment/uncomment operator"
  :type `,(if (get 'key-sequence 'widget-type)
              'key-sequence
            'sexp)
  :group 'evil-operator-comment)

(evil-define-operator evil-comment-or-uncomment-region (beg end type)
  "Comment out text from BEG to END with TYPE."
  (interactive "<R>")
  (if (eq type 'block)
      (evil-apply-on-block #'comment-or-uncomment-region beg end)
    (comment-or-uncomment-region beg end))
  ;; place cursor on beginning of line
  (when (and (evil-called-interactively-p)
             (eq type 'line))
    (evil-first-non-blank)))

;;;###autoload
(define-minor-mode evil-operator-comment-mode
  "Buffer local minor mode of comment/uncomment operator for Evil."
  :lighter ""
  :keymap (make-sparse-keymap)
  :group 'evil-operator-comment
  (evil-normalize-keymaps))

(defun evil-operator-comment-mode-install () (evil-operator-comment-mode 1))

;;;###autoload
(define-globalized-minor-mode global-evil-operator-comment-mode
  evil-operator-comment-mode evil-operator-comment-mode-install
  "Global minor mode of comment/uncomment operator for Evil.")

(evil-define-key 'normal evil-operator-comment-mode-map
                 evil-operator-comment-key 'evil-comment-or-uncomment-region)
(evil-define-key 'visual evil-operator-comment-mode-map
                 evil-operator-comment-key 'evil-comment-or-uncomment-region)

(provide 'evil-operator-comment)
