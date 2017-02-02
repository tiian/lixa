;;;
;;; Coding style for LIXA:
;;; 1. copy this file to $HOME/.emacs
;;; 2. use emacs text editor
;;;



(global-font-lock-mode t)

(setq ispell-program-name "/usr/bin/ispell")
;;; This defeats the incorrect regexp for skipping TIB entries.
(setq ispell-check-tib t)

;;; Font used for frame
(set-default-font "-Sony-Fixed-Medium-R-Normal--16-120-100-100-C-80-ISO8859-1")

(setq c-basic-offset 4)



;;;----------------------------------------------------------------------------
;;; Sets global key

(global-set-key [f1]      'manual-entry)
(global-set-key [C-f1]    'ispell-word)
(global-set-key [S-f1]    'info-lookup-symbol)
(global-set-key [f2]      'save-buffer)
(global-set-key [S-f2]    'write-file)
(global-set-key [S-f4]    'kill-this-buffer)
(global-set-key [f5]      'insert-file)
;;(global-set-key [S-f12]   'info)
(global-set-key  [f12]    'indent-region)
(global-set-key  [S-f12]  'font-lock-fontify-buffer)
(global-set-key [delete]  'delete-char)
(global-set-key [home]    'beginning-of-line)
(global-set-key [end]     'end-of-line)
(global-set-key [C-prior] 'enlarge-window)
(global-set-key [C-next]  'shrink-window)
(global-set-key  [kp-subtract]  'other-window)
(global-set-key [f7]      'dos-to-unix)
(global-set-key [f8]      'compile)
(global-set-key [S-f8]    'gdb)
(global-set-key [f9]      'match-paren)



;;;----------------------------------------------------------------------------
;;; Customizations for all of c-mode, c++-mode, java-mode, and objc-mode

(defconst my-c-style
  '(
    (c-set-offset 'member-init-intro '++)
    (c-tab-always-indent        . t)
    (c-comment-only-line-offset . 0)
    (c-hanging-braces-alist     . ((substatement-open before after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-basic-offset             . 4)
    (c-offsets-alist            . ((arglist-close . c-lineup-arglist)
                                   (substatement-open     . +)
                                   (statement-block-intro . +)
                                   (case-label            . 4)
                                   (statement-case-intro  . 4)
                                   (access-label          . 0)
                                   (block-open            . -)
                                   (inline-open           . 0)
                                   (inline-close          . 0)
                                   (knr-argdecl-intro     . -)))
    (c-echo-syntactic-information-p . t)
    )
  "My C Programming Style")
     

(defun my-c-mode-common-hook ()
  (require 'hideif)
  (font-lock-add-keywords
   'c-mode
   '(
     ("\\<\\(FIXME:\\)" 1 font-lock-warning-face t)
     ("\\<\\(NULL\\)" 1 font-lock-builtin-face t)
     ("\\<\\(TRUE\\)" 1 font-lock-constant-face t)
     ("\\<\\(FALSE\\)" 1 font-lock-constant-face t)
     ("\\<\\(THROW\\)" 1 font-lock-constant-face t)
     ("\\<\\(TRY\\)" 1 font-lock-constant-face t)
     ("\\<\\(CATCH\\)" 1 font-lock-constant-face t)
     ))
  (hide-ifdef-mode t)
  ;; add my personal style and set it for the current buffer
  (c-add-style "PERSONAL" my-c-style t)
  ;; other customizations
  (setq tab-width 4
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  ;; we like auto-newline and hungry-delete
  ;; (c-toggle-auto-hungry-state 1)
  ;; keybindings for all supported languages.  We can put these in
  ;; c-mode-base-map because c-mode-map, c++-mode-map, objc-mode-map,
  ;; java-mode-map, and idl-mode-map inherit from it.
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)
  (define-key c-mode-base-map [f9]   'compile)
  (define-key c-mode-base-map [f10]  'next-error)
  (define-key c-mode-base-map [f11]  'hide-ifdefs)
  (define-key c-mode-base-map [S-f11]  'hif-show-all)
  (define-key c-mode-base-map [f12]  'indent-region)
  )
     
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


(put 'upcase-region 'disabled nil)
