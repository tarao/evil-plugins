(require 'evil)

(defgroup evil-textobj-between nil
  "Text object between for Evil"
  :prefix "evil-textobj-between-"
  :group 'evil)

(defcustom evil-textobj-between-i-key "f"
  "Keys for evil-inner-between"
  :type 'string
  :group 'evil-textobj-between)
(defcustom evil-textobj-between-a-key "f"
  "Keys for evil-a-between"
  :type 'string
  :group 'evil-textobj-between)

(defun evil-between-range (count &optional inclusive)
  (condition-case ()
      (let* ((ch (read-char))
             (regexp (format "[%s]" (string ch))))
        (evil-regexp-range count regexp regexp (not inclusive)))))

(evil-define-text-object evil-a-between (count)
  "Select range between a character by which the command is followed."
  :extend-selection t
  (evil-between-range count t))
(evil-define-text-object evil-inner-between (count)
  "Select inner range between a character by which the command is followed."
  :extend-selection nil
  (evil-between-range count))

(define-key evil-outer-text-objects-map evil-textobj-between-a-key
  'evil-a-between)
(define-key evil-inner-text-objects-map evil-textobj-between-i-key
  'evil-inner-between)

(provide 'evil-textobj-between)
