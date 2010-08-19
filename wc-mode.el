;;wc-mode --- Count number of words in a buffer
;;
;; Author: Benjamin Beckwith
;; Created: 2010-6-19
;; Version: 1.0
;; Last-Updated: 2010-6-19
;; URL:
;; Keywords: 
;; Compatability:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Read the following for how to use the 'how-many' function
;; http://www.neverfriday.com/sweetfriday/2008/06/emacs-tip-word-counting-with-a.html
;; The following site had a good idea on how to produce number of chars
;; http://xahlee.org/emacs/elisp_count-region.html
;; Inspired by http://750words.com
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 1.0 Keystrokes for all goals added.
;;     Hooks variable added.
;;     In-code documentation updated.
;; 0.9 Added keymap for basic mode features/functions
;; 0.8 Added modeline format customization
;;     Added other customizations
;; 0.7 Added stats for lines and characters
;; 0.6 Mode line goal code added
;; 0.5 Initial version focused on word-count
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defgroup wc nil
  "Customization group for `wc-mode'."
  :group 'wp)

(defcustom wc-modeline-format "WC[%W%w/%tw]"
  "The format string for the modeline.
The detailed information for this minor mode can be shown in many
ways in the modeline. The formatting strings recognized in this
format are as follows.

  %W  Original word count (before changes)
  %L  Original line count
  %C  Original character count
  %w  Change in words
  %l  Change in lines
  %c  Change in characters
  %gc Character change goal
  %gl Line change goal
  %gw Word change goal
  %tw Total words in buffer
  %tl Total lines in buffer
  %tc Total characters in buffer

The default modeline, WC[%W%w], will display the orignial number
of words followed by the change in words. It will looks something
like WC[742+360] in the modeline.
"
  :group 'wc)

(defcustom wc-mode-hook nil
  "Hook to run when entering wc-mode."
  :type 'hook
  :group 'wc)

(defface wc-goal-face
  '((t (:inherit highlight)))
  "Face for modeline when goal is reached"
  :group 'wc)

(defvar wc-mode-map 
  (let ((map (make-sparse-keymap "Wordcount")))
    (define-key map (kbd "C-c C-w w") 'wc-set-word-goal)
    (define-key map (kbd "C-c C-w l") 'wc-set-line-goal)
    (define-key map (kbd "C-c C-w a") 'wc-set-char-goal)
    (define-key map (kbd "C-c C-w c") 'wc-count)
    (define-key map (kbd "C-c C-w q") 'wc-mode)
    map)
  "Keymap for wc-mode")

(defvar wc-orig-words 0 "Original count of words in the buffer")
(defvar wc-orig-lines 0 "Original count of words in the buffer")
(defvar wc-orig-chars 0 "Original count of words in the buffer")

(defvar wc-words-delta 0 "Change in word count")
(defvar wc-lines-delta 0 "Change in line count")
(defvar wc-chars-delta 0 "Change in char count")

(defvar wc-word-goal nil "Goal for number of words added")
(defvar wc-line-goal nil "Goal for number of lines added")
(defvar wc-char-goal nil "Goal for numger of chars added")

(defvar wc-modeline-format-alist
  '(("%W" . (number-to-string wc-orig-words))
    ("%L" . (number-to-string wc-orig-lines))
    ("%C" . (number-to-string wc-orig-chars))
    ("%w" . (wc-prepend-sign wc-words-delta))
    ("%l" . (wc-prepend-sign wc-lines-delta))
    ("%c" . (wc-prepend-sign wc-chars-delta))
    ("%gc" . (wc-prepend-sign wc-char-goal))
    ("%gl" . (wc-prepend-sign wc-line-goal))
    ("%gw" . (wc-prepend-sign wc-word-goal))
    ("%tc" . (number-to-string (+ wc-orig-chars wc-chars-delta)))
    ("%tl" . (number-to-string (+ wc-orig-lines wc-lines-delta)))
    ("%tw" . (number-to-string (+ wc-orig-words wc-words-delta))))
  "Format and value pair
Format will be evaluated in `wc-generate-modeline'")

(defun wc-format-modeline-string (fmt)
  "Format the modeline string according to specification and return result"
  (let ((case-fold-search nil))
    (dolist (pair wc-modeline-format-alist fmt)
      (when (string-match (car pair) fmt)
	(setq fmt (replace-match (eval (cdr pair)) t t fmt))))))

(defun wc-prepend-sign (val)
  "Add a sign to the beginning of a value.
Also cheat here a bit and add nil-value processing."
  (if val
      (format "%s%d"
	      (if (< val 0)
		  "-" "+")
	      (abs val))
    "none"))

(defun wc-set-local-variables (counts)
  "Setup buffer-local variables with the initial line, word and char counts."
  (set (make-local-variable 'wc-orig-lines) (nth 0 counts))
  (set (make-local-variable 'wc-orig-words) (nth 1 counts))
  (set (make-local-variable 'wc-orig-chars) (nth 2 counts))
  (set (make-local-variable 'wc-words-delta) 0)
  (set (make-local-variable 'wc-lines-delta) 0)
  (set (make-local-variable 'wc-chars-delta) 0))

(defun wc-set-word-goal (goal)
  "Set a goal for adding or removing words in the buffer"
  (interactive "nHow many words:")
  (setq wc-word-goal goal)
  (message "Goal set at %d words" goal))

(defun wc-set-line-goal (goal)
  "Set a goal for adding or removing lines in the buffer"
  (interactive "nHow many lines:")
  (setq wc-line-goal goal)
  (message "Goal set at %d lines" goal))

(defun wc-set-char-goal (goal)
  "Set a goal for adding or removing chars in the buffer"
  (interactive "nHow many characters:")
  (setq wc-char-goal goal)
  (message "Goal set at %d characters" goal))

(defun wc-goal-reached ()
  "Returns t when the goal change is reached."
  (or
   (if wc-line-goal
       (if (< wc-line-goal 0)
	   (< wc-lines-delta wc-line-goal)
	 (> wc-lines-delta wc-line-goal)))
   (if wc-word-goal
       (if (< wc-word-goal 0)
	   (< wc-words-delta wc-word-goal)
	 (> wc-words-delta wc-word-goal)))
   (if wc-char-goal
       (if (< wc-char-goal 0)
	   (< wc-chars-delta wc-char-goal)
	 (> wc-chars-delta wc-char-goal)))))


(defun wc-count (&optional rstart rend field)
  "Count the words present in the region following point.
This function follows most of the rules present in the `how-many'
function. If INTERACTIVE is omitted or nil, just return the word
count, do not print it.  Otherwise, if INTERACTIVE is t, the
function behaves according to interactive behavior.

START and END specify the region to operate on.

When called interactively, this function first checks to see if
it is in Transient Mark mode.  If that is the case, then the
function operates over the marked region.  Otherwise, it will
operate over the entire buffer.
"
  (interactive)
  (if rstart
      (setq rend (max rstart rend))
    (if (and (interactive-p) transient-mark-mode mark-active)
	(setq rstart (region-beginning)
	      rend (region-end))
      (setq rstart (point-min)
	    rend (point-max))))
  (let ((wcount (how-many "\\w+" rstart rend))
	(lcount (how-many "\\n" rstart rend))
	(ccount (- rend rstart)))
    (when (interactive-p) (message "%d line%s, %d word%s, %d char%s"
				   lcount
				   (if (= lcount 1) "" "s")
				   wcount
				   (if (= wcount 1) "" "s")
				   ccount
				   (if (= ccount 1) "" "s")
				   ))
    (if field
	(nth field (list lcount wcount ccount))
      (list lcount wcount ccount))))


(defalias 'wc 'wc-count
  "Alias function `wc-count' to the more legible `wc'.")

(defun wc-generate-modeline ()
  (let ((modeline (wc-format-modeline-string wc-modeline-format)))
    (when (wc-goal-reached)
      (put-text-property 0 (length modeline) 'face 'wc-goal-face modeline))
    (list " " modeline)))

(defun wc-mode-update ()
  "Return a string to update the modeline appropriately"
  (let* ((stats (wc-count (point-min) (point-max))))
    (setq wc-lines-delta (- (nth 0 stats) wc-orig-lines))
    (setq wc-words-delta (- (nth 1 stats) wc-orig-words))
    (setq wc-chars-delta (- (nth 2 stats) wc-orig-chars))
    (wc-generate-modeline)))

(define-minor-mode wc-mode
  "Toggle wc mode With no argument, this command toggles the
mode.  Non-null prefix argument turns on the mode.  Null prefix
argument turns off the mode.

When Wc mode is enabled on a buffer, it counts the current words
in the buffer and keeps track of a differential of added or
subtracted words.

A goal of number of words added/subtracted can be set while using
this mode. Upon completion of the goal, the modeline text will
highlight indicating that the goal has been reached.

Commands:
\\{wc-mode-map}

Entry to this mode calls the value of `wc-mode-hook' if that
value is non-nil."
  ;; initial value (off)
  :init-value nil
  ;; The indicator for the mode line
  :lighter (:eval (wc-mode-update))
  ;; The customization group
  :group 'wc
  ;; The local keymap to use
  :keymap wc-mode-map
  ;; The mode body code
  (if wc-mode
      (progn
	(wc-set-local-variables (wc-count (point-min) (point-max)))
	(run-mode-hooks 'wc-mode-hooks))))

(provide 'wc-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; wc-mode.el ends here
