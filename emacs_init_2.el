(abbrev-mode 1) ; turn on abbrev mode

;; set Unicode data file location. (used by what-cursor-position and describe-char)
(let ((x "~/emacs.d/UnicodeData.txt"))
  (when (file-exists-p x)
    (setq describe-char-unicodedata-file x)))

(setq-default indent-tabs-mode nil) ; emacs 23.1, 24.2, default to t

(setq tab-width 4)   ; emacs 23.1, 24.2, default to 8

(global-set-key (kbd "M-a") 'backward-char) ; Alt+a
(global-set-key (kbd "<f7> <f8>") 'calc)    ; F7 F8
(global-set-key (kbd "<app> c") 'calc)  ; Windows, menu/apps key syntax is <apps>

;; unset a key

(global-unset-key (kbd "C-b"))
;; or
(global-set-key (kbd "C-b") nil)

(global-set-key (kbd "C-:") 'backward-char) ; Ctrl+Shift+; or Ctrl+:
(global-set-key (kbd "C-\"") 'backward-char) ; Ctrl+Shift+' or Ctrl+"
; note: the question mark “?” cannot be used in shortcut.

(global-set-key (kbd "M-S-<f1>") 'backward-char)   ; Meta+Shift+F1
(global-set-key (kbd "C-S-<kp-2>") 'backward-char) ; Ctrl+Shift+“numberic pad 2”
(global-set-key (kbd "C-M-<up>") 'backward-char)   ; Ctrl+Meta+↑

(set-frame-font "DejaVu Sans Mono-10")  ; set font for current window
(set-frame-font "DejaVu Sans Mono-10" nil t) ; set font for all windows

;; set font for all windows
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))

;; set font for current window
(set-frame-parameter nil 'font "DejaVu Sans Mono-10")

;; set font
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    nil )
  )
 ((string-equal system-type "darwin")   ; Mac OS X
  (progn
    (add-to-list 'default-frame-alist '(font . "Monaco-14"))
    )
  )
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")) )
  )
 )

(text-scale-increase 2) ; 2 steps larger
(text-scale-increase 0) ; default size
(text-scale-decrease 2) ; 2 steps smaller

;; set background of current window (emacs's “frame”) to pale tinge
(set-background-color "honeydew")

;; set all windows (emacs's “frames”) to some defaults
(setq initial-frame-alist '((width . 100) (height . 54)))
(setq default-frame-alist
      '((menu-bar-lines . 1)
        (left-fringe)
        (right-fringe)
        (tool-bar-lines . 0)
        (width . 100)
        (height . 52)
        ))

;; set all windows (emacs's “frame”) to use font DejaVu Sans Mono
(set-frame-parameter nil 'font "DejaVu Sans Mono-10")

(load-theme 'misterioso)

(package-initialize)
(setq color-theme-is-global t)

(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(defun ergoemacs-forward-open-bracket (&optional number)
  "Move cursor to the next occurrence of left bracket or quotation mark.

With prefix NUMBER, move forward to the next NUMBER left bracket or quotation mark.

With a negative prefix NUMBER, move backward to the previous NUMBER left bracket or quotation mark."
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-backward-open-bracket (- 0 number))
    (forward-char 1)
    (search-forward-regexp
     (eval-when-compile
       (regexp-opt
        '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«"))) nil t number)
    (backward-char 1)))

(defun ergoemacs-backward-open-bracket (&optional number)
  "Move cursor to the previous occurrence of left bracket or quotation mark.
With prefix argument NUMBER, move backward NUMBER open brackets.
With a negative prefix NUMBER, move forward NUMBER open brackets."
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-forward-open-bracket (- 0 number))
    (search-backward-regexp
   (eval-when-compile
     (regexp-opt
      '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«"))) nil t number)))

(defun ergoemacs-forward-close-bracket (&optional number)
  "Move cursor to the next occurrence of right bracket or quotation mark.
With a prefix argument NUMBER, move forward NUMBER closed bracket.
With a negative prefix argument NUMBER, move backward NUMBER closed brackets."
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-backward-close-bracket (- 0 number))
    (search-forward-regexp
     (eval-when-compile
       (regexp-opt '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»"))) nil t number)))

(defun ergoemacs-backward-close-bracket (&optional number)
  "Move cursor to the previous occurrence of right bracket or quotation mark.
With a prefix argument NUMBER, move backward NUMBER closed brackets.
With a negative prefix argument NUMBER, move forward NUMBER closed brackets."
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-forward-close-bracket (- 0 number))
    (backward-char 1)
    (search-backward-regexp
     (eval-when-compile
       (regexp-opt '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»"))) nil t number)
    (forward-char 1)))

(global-set-key (kbd "<home>") 'ergoemacs-backward-open-bracket)
(global-set-key (kbd "<end>") 'ergoemacs-forward-close-bracket)

(defun ergoemacs-forward-quote (&optional number)
  "Move cursor to the next occurrence of ASCII quotation mark, single or double.

With prefix NUMBER, move forward to the next NUMBER quotation mark.

With a negative prefix NUMBER, move backward to the previous NUMBER quotation mark."
  (interactive "p")
  (if (and number (> 0 number))
      (ergoemacs-forward-quote (- 0 number))
    (search-forward-regexp (eval-when-compile (regexp-opt '("\"" "'"))) nil t number)
    ))

(defun ergoemacs-backward-quote (&optional number)
  "Move cursor to the previous occurrence of ASCII quotation mark, single or double.
With prefix argument NUMBER, move backward NUMBER quotation mark.
With a negative prefix NUMBER, move forward NUMBER quotation mark."
  (interactive "p")
  (if (and number (> 0 number)) (ergoemacs-backward-quote (- 0 number))
    (search-backward-regexp (eval-when-compile (regexp-opt '("\"" "'"))) nil t number)))

(defalias 'qrr 'query-replace-regexp)

(global-set-key [f5] 'call-last-kbd-macro)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(defalias 'qrr 'query-replace-regexp)

(global-set-key (kbd "M-r") 'isearch-forward-regexp) ; Alt+r
(global-set-key (kbd "M-s") 'isearch-backward-regexp) ; Alt+s

(defalias 'qrr 'query-replace-regexp)

(global-set-key [f5] 'call-last-kbd-macro)

(global-set-key (kbd "M-p") 'fill-paragraph) ; Alt+p
