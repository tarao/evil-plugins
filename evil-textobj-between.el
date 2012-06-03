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

(defun evil-between-range (count beg end type &optional inclusive)
  (condition-case ()
      (let ((ch (read-char)) beg-inc end-inc)
        (save-excursion
          (goto-char end)
          (backward-char)
          (evil-find-char count ch)
          (setq end-inc (1+ (point)))
          (goto-char beg)
          (evil-find-char-backward count ch)
          (setq beg-inc (point)))
        (if inclusive
            (evil-range beg-inc end-inc)
          (evil-range
           (if (= (1+ beg-inc) beg) beg-inc (1+ beg-inc))
           (if (= (1- end-inc) end) end-inc (1- end-inc)))))))

(evil-define-text-object evil-a-between (count &optional beg end type)
  "Select range between a character by which the command is followed."
  :extend-selection t
  (evil-between-range count beg end type t))
(evil-define-text-object evil-inner-between (count &optional beg end type)
  "Select inner range between a character by which the command is followed."
  :extend-selection nil
  (evil-between-range count beg end type))

(define-key evil-outer-text-objects-map evil-textobj-between-a-key
  'evil-a-between)
(define-key evil-inner-text-objects-map evil-textobj-between-i-key
  'evil-inner-between)

(provide 'evil-textobj-between)
