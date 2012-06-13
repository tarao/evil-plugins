;;; evil-little-word.el --- Emulate camelcasemotion.vim

(require 'evil)

(defun maybe-define-category (cat doc &optional table)
  (unless (category-docstring cat table) (define-category cat doc table)))

(let (uc lc defs (table (standard-category-table)))
  (map-char-table
   #'(lambda (key value)
       (when (natnump value)
         (let (from to)
           (if (consp key)
               (setq from (car key) to (cdr key))
             (setq from (setq to key)))
           (while (<= from to)
             (cond ((/= from (downcase from))
                    (add-to-list 'uc from))
                   ((/= from (upcase from))
                    (add-to-list 'lc from)))
             (setq from (1+ from))))))
   (standard-case-table))
  (setq defs `(("Uppercase" ?U ,uc)
               ("Lowercase" ?u ,lc)
               ("Underscore" ?_ (?_))))
  (dolist (elt defs)
    (maybe-define-category (cadr elt) (car elt) table)
    (dolist (ch (car (cddr elt)))
      (modify-category-entry ch (cadr elt) table))))

(defgroup evil-little-word nil
  "CamelCase and snake_case word movement support."
  :prefix "evil-little-word-"
  :group 'evil)

(defcustom evil-little-word-separating-categories
  '((?u . ?U) (?_ . ?u) (?_ . ?U))
  "List of pair (cons) of categories to determine word boundary
for little word movement. See the documentation of
`word-separating-categories'. Use `describe-categories' to see
the list of categories."
  :type '((character . character))
  :group 'evil-little-word)

(defcustom evil-little-word-combining-categories
  '()
  "List of pair (cons) of categories to determine word boundary
for little word movement. See the documentation of
`word-combining-categories'. Use `describe-categories' to see the
list of categories."
  :type '((character . character))
  :group 'evil-little-word)

(defcustom evil-little-word-cjk nil
  "Force little word movement to be sensitive to CJK word
boundary."
  :type 'boolean
  :group 'evil-cjk
  :group 'evil-little-word)

(defmacro evil-with-little-word (&rest body)
  (declare (indent defun)
           (debug t))
  `(let ((sep (append (if (and evil-cjk-emacs-word-boundary
                               (not evil-little-word-cjk))
                          word-separating-categories
                        evil-cjk-word-separating-categories)
                      evil-little-word-separating-categories))
         (cmb (append (if (and evil-cjk-emacs-word-boundary
                               (not evil-little-word-cjk))
                          word-combining-categories
                        evil-cjk-word-combining-categories)
                      evil-little-word-combining-categories)))
     (let ((evil-cjk-emacs-word-boundary t) ; turn off CJK word boundary
           (word-separating-categories sep)
           (word-combining-categories cmb))
       ,@body)))

(evil-define-union-move evil-move-little-word (count)
  "Move by little words."
  (evil-with-little-word (evil-move-word count)))

(evil-define-motion evil-forward-little-word-begin (count)
  "Move the cursor to the beginning of the COUNT-th next little word."
  :type exclusive
  (evil-with-little-word (evil-forward-word-begin count)))

(evil-define-motion evil-forward-little-word-end (count)
  "Move the cursor to the end of the COUNT-th next little word."
  :type inclusive
  (evil-with-little-word (evil-forward-word-end count)))

(evil-define-motion evil-backward-little-word-begin (count)
  "Move the cursor to the beginning of the COUNT-th previous little word."
  :type exclusive
  (evil-with-little-word (evil-backward-word-begin count)))

(evil-define-motion evil-backward-little-word-end (count)
  "Move the cursor to the end of the COUNT-th previous little word."
  :type inclusive
  (evil-with-little-word (evil-backward-word-end count)))

(evil-define-text-object evil-a-little-word (count &optional beg end type)
  "Select a little word."
  (evil-an-object-range count beg end type #'evil-move-little-word))

(evil-define-text-object evil-inner-little-word (count &optional beg end type)
  "Select inner little word."
  (evil-inner-object-range count beg end type #'evil-move-little-word))

(define-key evil-motion-state-map (kbd "glw") 'evil-forward-little-word-begin)
(define-key evil-motion-state-map (kbd "glb") 'evil-backward-little-word-begin)
(define-key evil-motion-state-map (kbd "glW") 'evil-forward-little-word-end)
(define-key evil-motion-state-map (kbd "glB") 'evil-backward-little-word-end)
(define-key evil-outer-text-objects-map (kbd "lw") 'evil-a-little-word)
(define-key evil-inner-text-objects-map (kbd "lw") 'evil-inner-little-word)

(provide 'evil-little-word)
