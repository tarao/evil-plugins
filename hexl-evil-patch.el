;;; hexl-evil-patch.el --- Patch to use Evil in hexl-mode

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

(setq hexl-mode-hook
      '(hexl-follow-line
        hexl-activate-ruler
        (lambda ()
          (defadvice hexl-self-insert-command
            (around ad-evil-hexl first (&optional num) activate)
            (let ((c last-command-event)
                  (address (hexl-current-address t)))
              (if (not num) (setq num 1))
              (while (> num 0)
                (let ((hex-position
                       (+ (* (/ address 16) 68)
                          11
                          (* 2 (% address 16))
                          (/ (% address 16) 2)))
                      (ascii-position
                       (+ (* (/ address 16) 68) 52 (% address 16))))
                  (if (= (point) ascii-position) (hexl-insert-char c 1)
                    (let ((pt (point)) hex next)
                      (cond ((= pt (1+ hex-position))
                                        ; ... 0a1b 2c3d ...
                                        ;        ^
                             (setq hex (concat
                                        (buffer-substring (1- pt) pt)
                                        (char-to-string c)))
                             (setq next t))
                            ((= pt hex-position)
                                        ; ... 0a1b 2c3d ...
                                        ;       ^
                             (setq hex (concat
                                        (char-to-string c)
                                        (buffer-substring (1+ pt)
                                                          (+ pt 2))))))
                      (if (stringp hex)
                          (let ((ch (hexl-hex-string-to-integer hex)))
                            (if (or (> ch 255) (< ch 0))
                                (error "Hex number out of range"))
                            (setq pt (point))
                            (delete-char 1)
                            (insert c)
                            (goto-char ascii-position)
                            (delete-char 1)
                            (insert (hexl-printable-character ch))
                            (goto-char (1+ pt))
                            (if (and next
                                     (or (eq address hexl-max-address)
                                         (setq address (1+ address))))
                                (hexl-goto-address address)))))))
                (setq num (1- num))))))
        (lambda ()
          (progn
            (setq hexl-iso nil)))))

(provide 'hexl-evil-patch)
;;; hexl-evil-patch.el ends here
