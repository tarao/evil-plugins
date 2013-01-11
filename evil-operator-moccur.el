;;; evil-operator-moccur.el --- Operator for Evil to use color-moccur

;; Author: INA Lintaro <tarao.gnn at gmail.com>
;; URL: http://github.com/tarao/evil-plugins
;; Version: 0.1
;; Keywords: evil, plugin

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'color-moccur)
(require 'evil)

(defgroup evil-operator-moccur nil
  "Moccur operator for Evil"
  :prefix "evil-operator-moccur-"
  :group 'evil)

(defcustom evil-operator-moccur-grep-find-key (kbd "M")
  "A key for moccur-grep-find operator"
  :type `,(if (get 'key-sequence 'widget-type)
              'key-sequence
            'sexp)
  :group 'evil-operator-moccur)

(defcustom evil-operator-moccur-use-current-directory nil
  "Uses current directory for grep and does not ask interactively."
  :type 'boolean
  :group 'evil-operator-moccur)

(evil-define-operator evil-moccur-grep-find-region (beg end &optional dir)
  "Moccur on text from BEG to END."
  :move-point nil
  (interactive "<r>")
  (unless dir
    (setq dir (or (and (not evil-operator-moccur-use-current-directory)
                       (moccur-grep-read-directory))
                  (file-name-directory (buffer-file-name)))))
  (moccur-grep-find dir (list (buffer-substring-no-properties beg end))))

;;;###autoload
(define-minor-mode evil-operator-moccur-mode
  "Buffer local minor mode of moccur operator for Evil."
  :lighter ""
  :keymap (make-sparse-keymap)
  :group 'evil-operator-moccur
  (evil-normalize-keymaps))

(defun evil-operator-moccur-mode-install () (evil-operator-moccur-mode 1))

;;;###autoload
(define-globalized-minor-mode global-evil-operator-moccur-mode
  evil-operator-moccur-mode evil-operator-moccur-mode-install
  "Global minor mode of moccur operator for Evil.")

(evil-define-key 'normal evil-operator-moccur-mode-map
                 evil-operator-moccur-grep-find-key
                 'evil-moccur-grep-find-region)
(evil-define-key 'visual evil-operator-moccur-mode-map
                 evil-operator-moccur-grep-find-key
                 'evil-moccur-grep-find-region)

(provide 'evil-operator-moccur)
;;; evil-operator-moccur.el ends here
