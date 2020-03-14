;;; skk-nicola-dcomp.el --- skk-dcomp support for NICOLA-DDSKK

;; Copyright (C) 2001 Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>

;; Author: Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese, mule, input method

;; This file is part of Daredevil SKK.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'skk-macs))

;;;###autoload
(add-hook 'skk-mode-hook
          #'(lambda ()
              (when (and (featurep 'skk-dcomp)
                         (featurep 'skk-nicola))
                (require 'skk-nicola-dcomp))))

(defadvice skk-nicola-self-insert-lshift-1 (around skk-nicola-dcomp activate)
  (cond
   ((or (not skk-dcomp-activate)
        skk-hint-inhibit-dcomp
        (eq skk-henkan-mode 'active))
    ad-do-it)
   (t
    (let (pos)
      (cond
       ((or (eq skk-henkan-mode 'active)
            (skk-get-prefix skk-current-rule-tree)
            (not skk-comp-stack))
        (skk-set-marker skk-dcomp-start-point nil)
        (skk-set-marker skk-dcomp-end-point nil))
       ((skk-dcomp-marked-p)
        (skk-dcomp-face-off)
        (unless (member (this-command-keys) skk-dcomp-keep-completion-keys)
          ;;
          (if (eq this-command 'skk-nicola-self-insert-rshift)
              (setq pos (point))
            (ignore-errors
              (delete-region skk-dcomp-start-point skk-dcomp-end-point))))))
      ad-do-it
      ;;
      (when (and (eq this-command 'skk-nicola-self-insert-rshift)
                 (eq skk-henkan-mode 'on))
        (when (and (markerp skk-dcomp-start-point)
                   (marker-position skk-dcomp-start-point)
                   pos
                   (< (marker-position skk-dcomp-start-point) pos))
          (delete-region skk-dcomp-start-point pos))
        (when (and (markerp skk-dcomp-end-point)
                   (marker-position skk-dcomp-end-point)
                   (< (point) (marker-position skk-dcomp-end-point)))
          (delete-region skk-dcomp-end-point (point))))
      ;;
      (skk-dcomp-do-completion (point))))))

(provide 'skk-nicola-dcomp)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; skk-nicola-dcomp.el ends here
