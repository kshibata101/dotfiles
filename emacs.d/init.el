;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacsの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; 基本設定
;;

;; functionキーをmetaキーに
(setq mac-function-modifier 'meta)

;; 文字の色を設定します。
(add-to-list 'default-frame-alist '(foreground-color . "green"))

;; 背景色を設定します
(add-to-list 'default-frame-alist '(background-color . "black"))

;; 透明度を設定します
(add-to-list 'default-frame-alist '(alpha . 85))

;; 対応する括弧を表示
(show-paren-mode 1)

;; スタートアップ画面非表示
(setq inhibit-startup-message t)

;; バックアップファイル*.~作成無効化
(setq make-backup-files nil)

;; バックアップファイル.#*無効化
(setq auto-save-default nil)

;; バックアップファイル作成無効
(setq backup-inhibited t)

;; GUIならメニューバーを隠す
;; turn off toolbar
(if window-system
    (tool-bar-mode 0))

;; 行末の空白を非表示
(remove-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default show-trailing-whitespace nil)
;(setq-default delete-trailing-whitespace nil)

;; バッファ移動
(windmove-default-keybindings)
(setq windmove-wrap-around t)

;; 同バッファ内でのバッファきりかえ　
(global-set-key "\M-n" 'next-buffer)
(global-set-key "\M-p" 'previous-buffer)

;; ファイル名検索時の大文字小文字区別無視
(setq completion-ignore-case t)

;; バッファ再読み込み
;; http://shibayu36.hatenablog.com/entry/2012/12/29/001418
(global-auto-revert-mode 1)

;; 同名ファイルを賢く開く
;; http://shibayu36.hatenablog.com/entry/2012/12/29/001418
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; 現在行ハイライト
;; http://shibayu36.hatenablog.com/entry/2012/12/29/001418
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background  "#98FB98"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)

;; Emacs 23より前のバージョンを利用している人用
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

;;
;; コマンド設定
;;

;; delete
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\M-h" 'help-command)

;; buffer
(global-set-key "\C-x\C-b" 'bs-show)

(global-set-key "\C-x\C-k" 'kill-this-buffer)

;; kill-ring
(global-set-key (kbd "M-y") 'anything-show-kill-ring)

;; split window horizontally
(global-set-key (kbd "S-t") 'split-window-horizontally)

;; Return key to newline and indent
(global-set-key (kbd "RET") 'newline-and-indent)

;; shell-mode
;; shell-modeで矢印キーの上下でコマンド履歴を表示
(setq shell-mode-hook
      (function (lambda ()
                  (define-key shell-mode-map [up] 'comint-previous-input)
                  (define-key shell-mode-map [down] 'comint-next-input))))

;; cua-mode
(cua-mode t) 
(setq cua-enable-cua-keys nil) ; CUAキーバインドを無効にする

;;
;; load-path
;;
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "elisp" "auto-install" "public_repos" "site-lisp" "snippet")


;; 単語の削除
(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (kbd "M-d") 'delete-word)
;(global-set-key (kbd "M-h") 'backward-delete-word) ; => 挙動が微妙

(global-set-key (kbd "RET") 'newline-and-indent)

;; 行頭とインデントの頭
;; http://d.hatena.ne.jp/kitokitoki/20100131/p4
(defun my-move-beginning-of-line ()
  (interactive)
  (if (bolp)
      (back-to-indentation)    
      (beginning-of-line)))

(global-set-key "\C-a" 'my-move-beginning-of-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacsの設定（外部ソース利用）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; emacs-nav
;; Type M-x nav to start navigating.
;(add-to-list 'load-path "/directory/containing/nav/")
(require 'nav)
(global-set-key "\C-x\C-d" 'nav-toggle)

;; install-elisp
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/install-elisp/")

; auto-install 関係
(require 'auto-install)
(setq auto-install-directory "~/.emacs.d/auto-install/")
;(auto-install-update-emacswiki-package-name t)
(auto-install-compatibility-setup)             ; 互換性確保

;; auto-complete.el
;; http://d.hatena.ne.jp/Watson/20100315/1268632079
(require 'auto-complete)

;; 辞書設定
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(global-auto-complete-mode t)

(setq ac-auto-start 4)
(setq ac-use-menu-map t)
;; デフォルトで設定済み
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
(define-key ac-completing-map (kbd "TAB") 'ac-complete)
(setq ac-dwim t)

;; linum-mode
(global-linum-mode t)
(set-face-attribute 'linum nil
                    :foreground "#888"
                    :height 1.0)
(setq linum-format "%4d")

;; undo-tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; undohistの設定
(when (require 'undohist nil t)
  (undohist-initialize))

;; recentf-ext
(require 'recentf-ext)

;; wgrep
(require 'wgrep nil t)
;(setq wgrep-enable-key "r")

;; maxframe
;; https://github.com/rmm5t/maxframe.el
(require 'maxframe)
(global-set-key (kbd "M-RET") 'maximize-frame)
(global-set-key (kbd "C-M-<return>") 'restore-frame)
(add-hook 'window-setup-hook 'maximize-frame t)
;(add-hook 'before-make-frame-hook 'maximize-frame t)

;; powerline
(require 'powerline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anything
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;;anything-c-source-R-helpの設定
;;
(setq anything-c-source-R-help
'((name . "R objects / help")
(init . (lambda ()
;; this grabs the process name associated with the buffer
(setq anything-c-ess-local-process-name ess-local-process-name)))
(candidates . (lambda ()
(condition-case nil
(ess-get-object-list anything-c-ess-local-process-name)
(error nil))))
(action
("help" . ess-display-help-on-object)
("head (10)" . (lambda(obj-name)
(ess-execute (concat "head(" obj-name ", n = 10)n") nil (concat "R head: " obj-name))))
("head (100)" . (lambda(obj-name)
(ess-execute (concat "head(" obj-name ", n = 100)n") nil (concat "R head: " obj-name))))
("tail" . (lambda(obj-name)
(ess-execute (concat "tail(" obj-name ", n = 10)n") nil (concat "R tail: " obj-name))))
("str" . (lambda(obj-name)
(ess-execute (concat "str(" obj-name ")n") nil (concat "R str: " obj-name))))
("summary" . (lambda(obj-name)
(ess-execute (concat "summary(" obj-name ")n") nil (concat "R summary: " obj-name))))
("view source" . (lambda(obj-name)
(ess-execute (concat "print(" obj-name ")n") nil (concat "R object: " obj-name))))
("dput" . (lambda(obj-name)
(ess-execute (concat "dput(" obj-name ")n") nil (concat "R dput: " obj-name)))))
(volatile)))

;;
;; anything-c-source-R-localの設定
;;
(setq anything-c-source-R-local
'((name . "R local objects")
(init . (lambda ()
;; this grabs the process name associated with the buffer
(setq anything-c-ess-local-process-name ess-local-process-name)
;; this grabs the buffer for later use
(setq anything-c-ess-buffer (current-buffer))))
(candidates . (lambda ()
(let (buf)
(condition-case nil
(with-temp-buffer
(progn
(setq buf (current-buffer))
(with-current-buffer anything-c-ess-buffer
(ess-command "print(ls.str(), max.level=0)n" buf))
(split-string (buffer-string) "n" t)))
(error nil)))))
(display-to-real . (lambda (obj-name) (car (split-string obj-name " : " t))))
(action
("str" . (lambda(obj-name)
(ess-execute (concat "str(" obj-name ")n") nil (concat "R str: " obj-name))))
("summary" . (lambda(obj-name)
(ess-execute (concat "summary(" obj-name ")n") nil (concat "R summary: " obj-name))))
("head (10)" . (lambda(obj-name)
(ess-execute (concat "head(" obj-name ", n = 10)n") nil (concat "R head: " obj-name))))
("head (100)" . (lambda(obj-name)
(ess-execute (concat "head(" obj-name ", n = 100)n") nil (concat "R head: " obj-name))))
("tail" . (lambda(obj-name)
(ess-execute (concat "tail(" obj-name ", n = 10)n") nil (concat "R tail: " obj-name))))
("print" . (lambda(obj-name)
(ess-execute (concat "print(" obj-name ")n") nil (concat "R object: " obj-name))))
("dput" . (lambda(obj-name)
(ess-execute (concat "dput(" obj-name ")n") nil (concat "R dput: " obj-name)))))
(volatile)))

; anything関係
;(require 'anything)
;(defvar org-directory "")
(require 'anything-startup)

(require 'anything-config)
(setq anything-sources (list anything-c-source-buffers
                             anything-c-source-emacs-commands
                             anything-c-source-bookmarks
                             anything-c-source-recentf
                             anything-c-source-file-name-history
                             anything-c-source-man-pages
                             anything-c-source-occur
                             anything-c-source-R-help
                             anything-c-source-locate))
(define-key anything-map (kbd "C-p") 'anything-previous-line)
(define-key anything-map (kbd "C-n") 'anything-next-line)
(define-key anything-map (kbd "C-v") 'anything-next-source)
(define-key anything-map (kbd "M-v") 'anything-previous-source)
(global-set-key (kbd "C-;") 'anything)
(global-set-key (kbd "C-:") 'anything-filelist+):

;; anything filelist
(setq anything-c-filelist-file-name "/tmp/all.filelist")
(setq anything-grep-candidates-fast-directory-regexp "^/tmp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; moccur
;; http://d.hatena.ne.jp/IMAKADO/20080724/1216882563
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; color-moccur.elの設定
(require 'color-moccur)

;; 複数の検索語や、特定のフェイスのみマッチ等の機能を有効にする
;; 詳細は http://www.bookshelf.jp/soft/meadow_50.html#SEC751
(setq moccur-split-word t)
;; migemoがrequireできる環境ならmigemoを使う
(when (require 'migemo nil t) ;第三引数がnon-nilだとloadできなかった場合にエラーではなくnilを返す
  (setq moccur-use-migemo t))

;;; anything-c-moccurの設定
(require 'anything-c-moccur)
;; カスタマイズ可能変数の設定(M-x customize-group anything-c-moccur でも設定可能)
(setq anything-c-moccur-anything-idle-delay 0.2 ;`anything-idle-delay'
      anything-c-moccur-higligt-info-line-flag t ; `anything-c-moccur-dmoccur'などのコマンドでバッファの情報をハイライトする
      anything-c-moccur-enable-auto-look-flag t ; 現在選択中の候補の位置を他のwindowに表示する
      anything-c-moccur-enable-initial-pattern t) ; `anything-c-moccur-occur-by-moccur'の起動時にポイントの位置の単語を初期パターンにする

;;; キーバインドの割当(好みに合わせて設定してください)
(global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur) ;バッファ内検索
(global-set-key (kbd "C-M-o") 'anything-c-moccur-dmoccur) ;ディレクトリ
(add-hook 'dired-mode-hook ;dired
          '(lambda ()
             (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur)))

;; ctags + anything
(when (require 'anything nil t)
  (require 'anything-exuberant-ctags)
  )
(define-key global-map (kbd "C-@") 'anything-exuberant-ctags-select-from-here)

(require 'anything-yaetags)
(add-to-list 'anything-sources 'anything-c-source-yaetags-select)
(define-key global-map (kbd "M-@") 'anything-yaetags-find-tag)


;; junk file
;; http://shibayu36.hatenablog.com/entry/2012/12/29/001418
(require 'open-junk-file)
(setq open-junk-file-format "~/Documents/junk/%Y-%m-%d-%H%M%S.")

;;;
;;; org-mode
;;;
;; org-modeの初期化
(require 'org-install)
;; キーバインドの設定
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cr" 'org-remember)
;; 拡張子がorgのファイルを開いた時，自動的にorg-modeにする
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; org-modeでの強調表示を可能にする
(add-hook 'org-mode-hook 'turn-on-font-lock)
;; 見出しの余分な*を消す
(setq org-hide-leading-stars t)
;; org-default-notes-fileのディレクトリ
(setq org-directory "~/org/")
;; org-default-notes-fileのファイル名
(setq org-default-notes-file "notes.org")

(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [(control tab)] nil)))

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;;
;; tabbar
;; (install-elisp "http://www.emacswiki.org/emacs/download/tabbar.el")
;; http://d.hatena.ne.jp/plasticster/20110825/1314271209
;; ______________________________________________________________________

(require 'tabbar)
(tabbar-mode 1)

;; タブ上でマウスホイール操作無効
(tabbar-mwheel-mode -1)

;; グループ化しない
(setq tabbar-buffer-groups-function nil)

;; 左に表示されるボタンを無効化
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))

;; タブの長さ
(setq tabbar-separator '(1.5))

;; 外観変更
(set-face-attribute
 'tabbar-default nil
 :family "Comic Sans MS"
 :background "black"
 :foreground "gray72"
 :height 1.0)
(set-face-attribute
 'tabbar-unselected nil
 :background "black"
 :foreground "grey72"
 :box nil)
(set-face-attribute
 'tabbar-selected nil
 :background "black"
 :foreground "yellow"
 :box nil)
(set-face-attribute
 'tabbar-button nil
 :box nil)
(set-face-attribute
 'tabbar-separator nil
 :height 1.5)

;;タブに表示させるバッファの設定
(defvar my-tabbar-displayed-buffers
  '("*scratch*")
  "*Regexps matches buffer names always included tabs.")

(defun my-tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space or an asterisk.
The current buffer and buffers matches `my-tabbar-displayed-buffers'
are always included."
  (let* ((hides (list ?\  ?\*))
         (re (regexp-opt my-tabbar-displayed-buffers))
         (cur-buf (current-buffer))
         (tabs (delq nil
                     (mapcar (lambda (buf)
                               (let ((name (buffer-name buf)))
                                 (when (or (string-match re name)
                                           (not (memq (aref name 0) hides)))
                                   buf)))
                             (buffer-list)))))
    ;; Always include the current buffer.
    (if (memq cur-buf tabs)
        tabs
      (cons cur-buf tabs))))

(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

;; Chrome ライクなタブ切り替えのキーバインド
(global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-S-tab>") 'tabbar-backward-tab)
(global-set-key (kbd "<C-right>") 'tabbar-forward-tab)
(global-set-key (kbd "<C-left>") 'tabbar-backward-tab)

;; タブ上をマウス中クリックで kill-buffer
(defun my-tabbar-buffer-help-on-tab (tab)
  "Return the help string shown when mouse is onto TAB."
  (if tabbar--buffer-show-groups
      (let* ((tabset (tabbar-tab-tabset tab))
             (tab (tabbar-selected-tab tabset)))
        (format "mouse-1: switch to buffer %S in group [%s]"
                (buffer-name (tabbar-tab-value tab)) tabset))
    (format "\
mouse-1: switch to buffer %S\n\
mouse-2: kill this buffer\n\
mouse-3: delete other windows"
            (buffer-name (tabbar-tab-value tab)))))

(defun my-tabbar-buffer-select-tab (event tab)
  "On mouse EVENT, select TAB."
  (let ((mouse-button (event-basic-type event))
        (buffer (tabbar-tab-value tab)))
    (cond
     ((eq mouse-button 'mouse-2)
      (with-current-buffer buffer
        (kill-buffer)))
     ((eq mouse-button 'mouse-3)
      (delete-other-windows))
     (t
      (switch-to-buffer buffer)))
    ;; Don't show groups.
    (tabbar-buffer-show-groups nil)))

(setq tabbar-help-on-tab-function 'my-tabbar-buffer-help-on-tab)
(setq tabbar-select-tab-function 'my-tabbar-buffer-select-tab)

;; e2wm.el
;; (require 'e2wm)
;; (global-set-key (kbd "M-+") 'e2wm:start-management)
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

(require 'direx)
(push '(direx:direx-mode :position left :width 40 :dedicated t)
      popwin:special-display-config)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
(setq direx:leaf-icon "  "
      direx:open-icon "▾ "
      direx:closed-icon "▸ ")

(setq special-display-function 'popwin:special-display-popup-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; summaryeの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'se/make-summary-buffer "summarye" nil t)
(global-set-key (kbd "C-x f") 'se/make-summary-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymakeの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; htmlの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; nxml-modeの場合
;; (add-to-list 'auto-mode-alist '(".\\.[sx]?html?\\(\\.[a-zA-Z_]+\\)?\\'" . nxml-mode))
;; (add-hook 'nxml-mode-hook
;;           (lambda ()
;;             ;;(setq auto-fill-mode -1)
;;             ;; スラッシュの入力で終了タグを自動補完
;;             (setq nxml-slash-auto-complete-flag t)
;;             ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cssの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; <Emacs実践入門>
(defun css-mode-hooks () 
  "css-mode hooks"
  ;; インデントをCスタイルにする
  (setq cssm-indent-function 'cssm-c-style-indenter)
  ;; インデント幅を2にする
  (setq cssm-indent-level 2)
  ;; インデントにタブ文字を使わない
  (setq-default indent-tabs-mode nil)
  ;; 閉じ括弧の前に改行を挿入する
  (setq cssm-newline-before-closing-bracket t))

(add-hook 'css-mode-hook 'css-mode-hooks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; javascriptの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Emacs実践入門
;(add-hook 'js2-mode-hook 'js-indent-hook)

;;; JS用Flymakeの初期化関数の定義
(defun flymake-jsl-init ()
  (list "jsl" (list "-process" (flymake-init-create-temp-buffer-copy
                                'flymake-create-temp-inplace))))
;; Javascript編集でFlymakeを起動する
(add-to-list 'flymake-allowed-file-name-masks
             '("\\.js\\'" flymake-jsl-init))

(add-to-list 'flymake-err-line-patterns
 '("^\\(.+\\)(\\([0-9]+\)): \\(.*warning\\|SyntaxError\\): \\(.*\\)" 1 2 nil 4))
;;; 

;; json.el
(require 'json)

;; nodejs
(add-to-list 'load-path "~/.emacs.d/site-lisp/nodejs-mode/")
(require 'nodejs-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rubyの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ruby-mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                     interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda () (inf-ruby-keys)))

;; ruby-electric
(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))

;; rubydb
(autoload 'rubydb "rubydb3x"
  "run rubydb on program file in buffer *gud-file*.
the directory containing file becomes the initial working directory
and source-file directory for your debugger." t)

;; Interactively Do Things (highly recommended, but not strictly required)
;(require 'ido)
;(ido-mode t)

;; Rinari
 (require 'rinari)

;;; rhtml-mode
 (require 'rhtml-mode)
 (add-hook 'rhtml-mode-hook
     (lambda () (rinari-launch)))

;; rails
;; (defun try-complete-abbrev (old)
;;   (if (expand-abbrev) t nil))

;; (setq hippie-expand-try-functions-list
;;       '(try-complete-abbrev
;;         try-complete-file-name
;;         try-expand-dabbrev))
;; (setq rails-use-mongrel t)
;; (require 'cl)
;; (require 'rails)

;; ;; 対応するファイルへの切り替え(C-c C-p)
;; (define-key rails-minor-mode-map "\C-c\C-p" 'rails-lib:run-primary-switch)
;; ;; 行き先を選べるファイル切り替え(C-c C-n)
;; (define-key rails-minor-mode-map "\C-c\C-n" 'rails-lib:run-secondary-switch)

;; (setq auto-mode-alist  (cons '("\\.rhtml$" . html-mode) auto-mode-alist))

;; ruby-block
(require 'ruby-block)
(ruby-block-mode t)
;; ミニバッファに表示し, かつ, オーバレイする.
(setq ruby-block-highlight-toggle t)

;; Ruby用Flymakeの設定
(defun flymake-ruby-init () 
  (list "ruby" (list "-c" (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.rb\\'" flymake-ruby-init))

(add-to-list 'flymake-err-line-patterns
             '("\\(.*\\):(\\([0-9]+\\)): \\(.*\\)" 1 2 nil 3))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHPの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; flymake
;; http://mugijiru.seesaa.net/article/326967860.html
(defun flymake-php-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "php" (list "-l" local-file))))
(push '(".+\\.php$" flymake-php-init) flymake-allowed-file-name-masks)
(push '("(Parse|Fatal) error: (.*) in (.*) on line ([0-9]+)" 3 4 nil 2) flymake-err-line-patterns)

;; 自動挿入
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/insert/")
(define-auto-insert "\\.html$" "html-template.html")
(define-auto-insert "\\.php$" "php-template.php")
;; (define-auto-insert "\\.php$" "php-html-template.php")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rの設定
;; http://sheephead.homelinux.org/2009/10/26/1673/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; essの設定
(require 'ess-site)

;;
;; YaTeX
;; http://oku.edu.mie-u.ac.jp/~okumura/texwiki/?YaTeX#qe563613
;;
(add-to-list 'load-path "~/.emacs.d/site-lisp/yatex")
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-use-LaTeX2e t)
(setq YaTeX-use-AMS-LaTeX t)
(setq YaTeX-dvipdf-command "/usr/texbin/dvipdfmx")
(setq YaTeX-dvi2-command-ext-alist
      '(("[agx]dvi\\|PictPrinter\\|Mxdvi" . ".dvi")
        ("gv" . ".ps")
        ("Preview\\|TeXShop\\|TeXworks\\|Skim\\|mupdf\\|xpdf\\|Adobe" . ".pdf")))

;; tex-commandの設定は以下のサイトを参照した
;; http://henry.animeo.jp/wp/?p=1553
(setq tex-command "~/Library/TeXShop/bin/platex2pdf-utf8")

(setq bibtex-command (cond ((string-match "uplatex" tex-command) "/usr/texbin/upbibtex")
                           ((string-match "platex" tex-command) "/usr/texbin/pbibtex")
                           ((string-match "lualatex\\|xelatex" tex-command) "/usr/texbin/bibtexu")
                           (t "/usr/texbin/bibtex")))
(setq makeindex-command (cond ((string-match "uplatex" tex-command) "/usr/texbin/mendex")
                              ((string-match "platex" tex-command) "/usr/texbin/mendex")
                              ((string-match "lualatex\\|xelatex" tex-command) "/usr/texbin/texindy")
                              (t "/usr/texbin/makeindex")))
(setq dvi2-command (cond ((string-match "pdf\\|lua\\|xe" tex-command) "/usr/bin/open -a Preview")
                         (t "/usr/bin/open -a PictPrinter")))
(setq dviprint-command-format (cond ((string-match "pdf\\|lua\\|xe" tex-command) "/usr/bin/open -a \"Adobe Reader\" %s")
                                    (t "/usr/bin/open -a \"Adobe Reader\" `echo %s | sed -e \"s/\\.[^.]*$/\\.pdf/\"`")))

(defun skim-forward-search ()
  (interactive)
  (let* ((ctf (buffer-name))
         (mtf)
         (pf)
         (ln (format "%d" (line-number-at-pos)))
         (cmd "/Applications/Skim.app/Contents/SharedSupport/displayline")
         (args))
    (if (YaTeX-main-file-p)
        (setq mtf (buffer-name))
      (progn
        (if (equal YaTeX-parent-file nil)
            (save-excursion
              (YaTeX-visit-main t)))
        (setq mtf YaTeX-parent-file)))
    (setq pf (concat (car (split-string mtf "\\.")) ".pdf"))
    (setq args (concat ln " " pf " " ctf))
    (message (concat cmd " " args))
    (process-kill-without-query
     (start-process-shell-command "displayline" nil cmd args))))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map (kbd "C-c s") 'skim-forward-search)))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode -1)))

;;
;; RefTeX with YaTeX
;;
;(add-hook 'yatex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook
          '(lambda ()
             (reftex-mode 1)
             (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
             (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))

;; load-pathにyasnippetのパスを通す
(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets" ;; 作成するスニペットはここに入る
        "~/.emacs.d/public_repos/yasnippet/snippets" ;; 最初から入っていたスニペット(省略可能)
        ))
(yas-global-mode 1)

;; 単語展開キーバインド (ver8.0から明記しないと機能しない)
;; (setqだとtermなどで干渉問題ありでした)
;; もちろんTAB以外でもOK 例えば "C-;"とか
;(custom-set-variables '(yas-trigger-key "TAB"))

;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

;; anything interface
(eval-after-load "anything-config"
  '(progn
     (defun my-yas/prompt (prompt choices &optional display-fn)
       (let* ((names (loop for choice in choices
                           collect (or (and display-fn (funcall display-fn choice))
                                       choice)))
              (selected (anything-other-buffer
                         `(((name . ,(format "%s" prompt))
                            (candidates . names)
                            (action . (("Insert snippet" . (lambda (arg) arg))))))
                         "*anything yas/prompt*")))
         (if selected
             (let ((n (position selected names :test 'equal)))
               (nth n choices))
           (signal 'quit "user quit!"))))
     (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))))

;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(safe-local-variable-values (quote ((whitespace-style face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark) (ruby-compilation-executable . "ruby") (ruby-compilation-executable . "ruby1.8") (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby")))))
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.

;;  )

;;
;; Shell 設定
;;

;; より下に記述した物が PATH の先頭に追加されます
(dolist (dir (list
              "/sbin"
              "/usr/sbin"
              "/bin"
              "/usr/bin"
              "/opt/local/bin"
              "/sw/bin"
              "/usr/local/bin"
              (expand-file-name "~/bin")
              (expand-file-name "~/.emacs.d/bin")
              ))
;; PATH と exec-path に同じ物を追加します
(when (and (file-exists-p dir) (not (member dir exec-path)))
  (setenv "PATH" (concat dir ":" (getenv "PATH")))
  (setq exec-path (append (list dir) exec-path))))

(setenv "MANPATH" (concat "/usr/local/man:/usr/share/man:/Developer/usr/share/man:/sw/share/man" (getenv "MANPATH")))

;; shell の存在を確認
(defun skt:shell ()
  (or (executable-find "zsh")
      (executable-find "bash")
      ;; (executable-find "f_zsh") ;; Emacs + Cygwin を利用する人は Zsh の代りにこれにしてください
      ;; (executable-find "f_bash") ;; Emacs + Cygwin を利用する人は Bash の代りにこれにしてください
      ;; (executable-find "cmdproxy")
      (error "can't find 'shell' command in PATH!!")))

;; Shell 名の設定
(setq shell-file-name (skt:shell))
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)

(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)

;; lsなどの色が出る処理におけるエスケープ文字表出対策
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(global-set-key (kbd "C-c s") '(lambda ()
                                (interactive)
                                (term shell-file-name)))

;; Emacs が保持する terminfo を利用する
(setq system-uses-terminfo nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mysqlの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sql-mysqlでsql-interactive-modeが起動
(setq sql-user "root"
      sql-server "localhost"
      sql-product 'mysql)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gitの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (executable-find "git")
  (require 'egg nil t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-modeの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
      
;;; インデント数
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset   2)
  (setq web-mode-css-indent-offset      4)
  (setq web-mode-code-indent-offset     4))
(add-hook 'web-mode-hook 'web-mode-hook)

