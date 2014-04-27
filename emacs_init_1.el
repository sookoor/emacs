(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )
(transient-mark-mode 1) ; highlight text selection
(delete-selection-mode 1) ; delete seleted text when typing
(global-font-lock-mode 1) ; turn on syntax coloring
(show-paren-mode 1) ; turn on paren match highlighting
(setq show-paren-style 'expression) ; highlight entire bracket expression
(column-number-mode 1)
(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #autosave# files
(recentf-mode 1) ; keep a list of recently opened files
(setq line-move-visual nil) ; use t for true, nil for false

(defun toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0) ) )

(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if (eq line-spacing nil)
      (setq-default line-spacing 0.5) ; add 0.5 height between lines
    (setq-default line-spacing nil)   ; no extra heigh between lines
    )
  (redraw-display))

(global-hl-line-mode 1) ; turn on highlighting current line

(setq pop-up-frames t) ; t for true, nil for false

;; easy keys to split window. Key based on ErgoEmacs keybinding
(global-set-key (kbd "M-3") 'delete-other-windows) ; expand current pane
(global-set-key (kbd "M-4") 'split-window-vertically) ; split pane top/bottom
(global-set-key (kbd "M-2") 'delete-window) ; close current pane
(global-set-key (kbd "M-s") 'other-window) ; cursor to other pane

;; make cursor movement keys under right hand's home-row.
(global-set-key (kbd "M-j") 'backward-char) ; was indent-new-comment-line
(global-set-key (kbd "M-l") 'forward-char)  ; was downcase-word
(global-set-key (kbd "M-i") 'previous-line) ; was tab-to-tab-stop
(global-set-key (kbd "M-k") 'next-line) ; was kill-sentence

(global-set-key (kbd "M-SPC") 'set-mark-command) ; was just-one-space
(global-set-key (kbd "M-a") 'execute-extended-command) ; was backward-sentence

(defun xah-cut-line-or-region ()
  "Cut the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2)) ) )

(defun xah-copy-line-or-region ()
  "Copy current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2)) ) )

(global-set-key (kbd "<f2>") 'xah-cut-line-or-region) ; cut
(global-set-key (kbd "<f3>") 'xah-copy-line-or-region) ; copy
(global-set-key (kbd "<f4>") 'yank) ; paste

(global-set-key (kbd "M-9") 'kill-whole-line)

(load "~/.emacs.d/my_alias")

(global-set-key (kbd "<f8>") 'execute-extended-command)
(global-set-key (kbd "<apps>") 'execute-extended-command) ; Microsoft Windows, menu/apps key
(global-set-key (kbd "<menu>") 'execute-extended-command) ; Linux, menu/apps key

;; automatically show completions for execute-extended-command
(icomplete-mode 1)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(defcustom ξ-font-list nil "A list of fonts for `cycle-font' to cycle from." :group 'font)

(set-default 'ξ-font-list 
(cond
 ((string-equal system-type "windows-nt") ; Windows
'(
                         "Courier New-10"
                         "DejaVu Sans Mono-9"
                         "Lucida Console-10"
                         "Segoe UI Symbol-12"

                         "DejaVu Sans-10"
                         "Lucida Sans Unicode-10"
                         "Arial Unicode MS-10"
                         )
  )
 ((string-equal system-type "gnu/linux")
 '(
                         "DejaVu Sans Mono-9"
                         "DejaVu Sans-9"
                         "Symbola-13"
                         )
 )
 ((string-equal system-type "darwin") ; Mac
  '(
                         "DejaVu Sans Mono-9"
                         "DejaVu Sans-9"
                         "Symbola-13"
                         ) ) )
)

(defun cycle-font (ξ-n)
  "Change font in current frame.
Each time this is called, font cycles thru a predefined list of fonts in the variable `ξ-font-list' .
If ξ-n is 1, cycle forward.
If ξ-n is -1, cycle backward."
  (interactive "p")
  ;; this function sets a property “state”. It is a integer. Possible values are any index to the fontList.
  (let (fontToUse stateBefore stateAfter )
    (setq stateBefore (if (get 'cycle-font 'state) (get 'cycle-font 'state) 0))
    (setq stateAfter (% (+ stateBefore (length ξ-font-list) ξ-n) (length ξ-font-list)))

    (setq fontToUse (nth stateAfter ξ-font-list))
    (set-frame-parameter nil 'font fontToUse)
    (redraw-frame (selected-frame))
    (message "Current font is: %s" fontToUse )

    (put 'cycle-font 'state stateAfter) ) )

(defun cycle-font-forward ()
  "Switch to the next font, in the current frame.
See `cycle-font'."
  (interactive)
  (cycle-font 1)
  )

(defun cycle-font-backward ()
  "Switch to the previous font, in the current frame.
See `cycle-font'."
  (interactive)
  (cycle-font -1)
  )

(defun toggle-line-spacing ()
"Toggle line spacing between no extra space to extra half line height."
(interactive)
(if (eq line-spacing nil)
    (setq-default line-spacing 0.5) ; add 0.5 height between lines
  (setq-default line-spacing nil)   ; no extra heigh between lines
  ))

; open my Unicode template with F8 key
(global-set-key (kbd "<f8>")
  (lambda () (interactive) (find-file "~/emacs.d/my_unicode_template.txt")))

(global-set-key (kbd "<f11>") "★") ; make F11 key insert a star

(global-set-key (kbd "<f9> a") "α") ; F9 followed by a
(global-set-key (kbd "<f9> b") "β")

(define-abbrev-table 'global-abbrev-table '(
    ("alpha" "α")
    ("beta" "β")
    ("gamma" "γ")
    ("theta" "θ")
    ("inf" "∞")

    ("ar1" "→")
    ("ar2" "⇒")
    ))

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key (kbd "<f9>") 'calendar)
