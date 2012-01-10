;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit starter-kit-bindings starter-kit-eshell
                                  starter-kit-js starter-kit-lisp
								  yaml-mode sass-mode
                                  rainbow-delimiters rainbow-mode
                                  fill-column-indicator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages that I might have to install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp-slime-nav, find-file-in-project, idle-highlight-mode,
;; ido-ubiquitous, magit, paredit, smex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/vendor")
(add-to-list 'load-path "~/.emacs.d/vendor/nxhtml")
(add-to-list 'load-path "~/.emacs.d/themes")

(setq custom-file "~/.emacs.d/custom.el")
(if (file-readable-p custom-file)
    (load custom-file))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun safe-load (theme)
  (if (find theme (custom-available-themes))
      (load-theme theme t)))

(safe-load 'zenburn)
(safe-load 'Consolas)

(winner-mode t)

(setq fill-column 80)

(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IRC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'bitlbee)

(setq erc-auth "~/.emacs.d/.erc-auth")
(if (file-readable-p erc-auth)
    (load erc-auth))

(defun bitlbee-identify ()
  "If we're on the bitlbee server, send the identify command to the &bitlbee channel."
  (when (and (string= "localhost" erc-session-server)
             (string= "&bitlbee" (buffer-name)))
    (erc-message "PRIVMSG" (format "%s identify %s" (erc-default-target)
                                   bitlbee-password))))

(add-hook 'erc-join-hook 'bitlbee-identify)

(global-set-key (kbd "\C-c bee") (lambda () (interactive)
                                   (bitlbee-start)
                                   (erc :server "localhost" :port 6667
                                        :nick "jory")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun js-mode-options ()
  "These are *my* js-mode options."
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'js-mode-options)


(defun org-mode-options ()
  "These are *my* org-mode options."
  (setq fill-column 80)
  (longlines-mode -1)
  (auto-fill-mode))

(add-hook 'org-mode-hook 'org-mode-options)

(defun html-mode-options ()
  "These are *my* html-mode options."
  (setq fill-column 80)
  (setq tab-width 4)
  (longlines-mode -1))

(add-hook 'html-mode-hook 'html-mode-options)

(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'longlines-mode)

(add-hook 'before-save-hook 'whitespace-cleanup)

(set-default 'tab-width 4)
(set-default 'indent-tabs-mode nil)
(setq longlines-auto-wrap t)
(column-number-mode)

(load "~/.emacs.d/vendor/nxhtml/autostart.el")
(setq mumamo-chunk-coloring 42)
(add-to-list 'auto-mode-alist '("\\.html$" . html-mumamo-mode))

;; HACK: Needed to suppress all the Compile-log junk. Should be removed.
(setq warning-minimum-level :error)

(add-to-list 'auto-mode-alist '("\\Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

(add-hook 'css-mode-hook 'rainbow-mode)

(add-hook 'sass-mode-hook 'rainbow-mode)
(add-hook 'sass-mode-hook 'rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defuns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-most-horizontal-space ()
  "Usually you want to leave at least one space"
  (interactive)
  (delete-horizontal-space)
  (insert " "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global-set-keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x m") 'smex)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "\C-c cap") 'org-capture)
(global-set-key (kbd "M-\\") 'delete-most-horizontal-space)

(defun python-server ()
  "Run a python http server in the current directory."
  (interactive
   (async-shell-command "python -m http.server")))

(global-set-key (kbd "C-c p") 'python-server)
(put 'ido-exit-minibuffer 'disabled nil)

(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-command-key-is-meta t)
  (setq mac-command-modifier 'meta)

  ;; OS X's ls command doesn't support the --dired flag, so use the
  ;; built-in one instead. This is the default behaviour on Windows.
  ;;     See: dired-use-ls-dired
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp))

(set-frame-height (selected-frame) 80)
(set-frame-width (selected-frame) 176)
