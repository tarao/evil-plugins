(require 'evil)

(defgroup evil-cjk nil
  "CJK patch for Evil"
  :prefix "evil-cjk-"
  :group 'evil)

(defcustom evil-cjk-emacs-word-boundary nil
  "Determine word boundary exactly the same way as Emacs does."
  :type 'boolean
  :group 'evil-cjk)

(defcustom evil-cjk-word-separating-categories
  '(;; Kanji
    (?C . ?H) (?C . ?K) (?C . ?k) (?C . ?A) (?C . ?G)
    ;; Hiragana
    (?H . ?C) (?H . ?K) (?H . ?k) (?H . ?A) (?H . ?G)
    ;; Katakana
    (?K . ?C) (?K . ?H) (?K . ?k) (?K . ?A) (?K . ?G)
    ;; half-width Katakana
    (?k . ?C) (?k . ?H) (?k . ?K) ; (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?C) (?A . ?H) (?A . ?K) ; (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?C) (?G . ?H) (?G . ?K) ; (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `evil-cjk-word-boundary-p'. See the documentation of
`word-separating-categories'."
  :type '((character . character))
  :group 'evil-cjk)

(defcustom evil-cjk-word-combining-categories
  '(;; default value in word-combining-categories
    (nil . ?^) (?^ . nil)
    ;; Roman
    (?r . ?k) (?r . ?A) (?r . ?G)
    ;; half-width Katakana
    (?k . ?r) (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?r) (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?r) (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `evil-cjk-word-boundary-p'. See the documentation of
`word-combining-categories'."
  :type '((character . character))
  :group 'evil-cjk)

;; These are taken from word_boundary_p function in category.c
(defun categoryp (x) (and (integerp x) (>= x #x20) (<= x #x7e)))
(defun word-boundary-p (ch1 ch2)
  "Return t if there is a word boundary between two
word-consistuent characters CH1 and CH2 if they appear in this
order, else return nil."
  (let (tail result)
    (if (eq (aref char-script-table ch1) (aref char-script-table ch2))
        (setq tail   word-separating-categories
              result nil)
      (setq tail   word-combining-categories
            result t))
    (let ((cat1 (char-category-set ch1))
          (cat2 (char-category-set ch2)))
      (when (and cat1 cat2)
        (while (consp tail)
          (let ((elt (car tail)))
            (when (and (consp elt)
                       (or (null (car elt))
                           (and (categoryp (car elt))
                                (aref cat1 (car elt))
                                (not (aref cat2 (car elt)))))
                       (or (null (cdr elt))
                           (and (categoryp (cdr elt))
                                (not (aref cat1 (cdr elt)))
                                (aref cat2 (cdr elt)))))
              (setq result (not result))
              (setq tail nil)))
          (setq tail (cdr tail))))
      result)))

(defun evil-cjk-word-boundary-p (ch1 ch2)
  "Return t if there is a word boundary between two
word-consistuent characters CH1 and CH2 if they appear in this
order, else return nil. This function acts exactly the same as
`word-boundary-p' if `evil-cjk-emacs-word-boundary' is non-nil."
  (if evil-cjk-emacs-word-boundary
      (word-boundary-p ch1 ch2)
    (let ((word-separating-categories evil-cjk-word-separating-categories)
          (word-combining-categories evil-cjk-word-combining-categories))
      (word-boundary-p ch1 ch2))))

(defun evil-move-word-cjk (count)
  "Move by words being sensitive to CJK word boundary."
  (let ((regexp (format "[%s]" evil-word)))
    (evil-motion-loop (var count)
      (cond
       ((< var 0)
        (re-search-backward regexp nil t)
        ;; instead of (skip-chars-backward evil-word), move backward until
        ;; a word boundary is found
        (while (and (not (bobp))
                    (not (evil-cjk-word-boundary-p
                          (char-before (point)) (char-after (point)))))
          (backward-char)))
       (t
        (re-search-forward regexp nil t)
        ;; instead of (skip-chars-forward evil-word), move forward until
        ;; a word boundary is found
        (while (and (not (eobp))
                    (not (evil-cjk-word-boundary-p
                          (char-before (point)) (char-after (point)))))
          (forward-char)))))))

(defalias 'evil-move-word-non-cjk (symbol-function 'evil-move-word)
  "Move by words not being sensitive to CJK word boundary.")

(evil-define-union-move evil-move-word (count)
  "Move by words."
  (evil-move-word-cjk count)
  (evil-move-word-non-cjk count))

(provide 'evil-cjk)
