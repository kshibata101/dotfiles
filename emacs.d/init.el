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
;(add-to-list 'default-frame-alist '(alpha .8))

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

;; ビープ消す
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; GUIならメニューバーを隠す
;; turn off toolbar
(if window-system
    (tool-bar-mode 0))

;; 行末の空白を非表示
(remove-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default show-trailing-whitespace nil)
;(setq-default delete-trailing-whitespace nil)

;; 空白や長すぎる行を視覚化する。
(require 'whitespace)
(setq whitespace-style '(face           ; faceで可視化
                         ;trailing       ; 行末
                         tabs           ; タブ
                         ;spaces
                         ;empty          ; 先頭/末尾の空行
                         tab-mark
                         ))
(global-whitespace-mode 1)


;; バッファ移動
(windmove-default-keybindings)
(setq windmove-wrap-around t)
(define-key global-map (kbd "M-k") 'windmove-up)
(define-key global-map (kbd "M-j") 'windmove-down)
(define-key global-map (kbd "M-l") 'windmove-right)
(define-key global-map (kbd "M-h") 'windmove-left)

;; 日本語環境
(set-language-environment "Japanese")

;; 日本語
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Hiragino Mincho Pro")) ;; font
;;  (font-spec :family "Hiragino Kaku Gothic ProN")) ;; font

;; utf-8
;(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

;; 同バッファ内でのバッファきりかえ　
;(global-set-key "\M-n" 'next-buffer)
;(global-set-key "\M-p" 'previous-buffer)

;; ファイル名検索時の大文字小文字区別無視
(setq completion-ignore-case t)

;; バッファ再読み込み
;; http://shibayu36.hatenablog.com/entry/2012/12/29/001418
(global-auto-revert-mode 1)

;; 現在の関数名をモードラインに表示
(which-function-mode 1)

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

;; Mavericksでデフォルトディレクトリがルートになってしまう問題への対処
;; http://qiita.com/h12o@github/items/f4f16a3d1c7162f3cb51
;(setq inhibit-splash-screen t)
(defun cd-to-homedir-all-buffers ()
  "Change every current directory of all buffers to the home directory."
  (mapc
   (lambda (buf) (set-buffer buf) (cd (expand-file-name "~"))) (buffer-list)))
(add-hook 'after-init-hook 'cd-to-homedir-all-buffers)

;; ウィンドウ3分割
;; http://d.hatena.ne.jp/yascentur/20110621/1308585547
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))
(global-set-key "\C-x6" '(lambda ()
                           (interactive)
                           (split-window-vertically-n 3)))
(global-set-key "\C-x7" '(lambda ()
                           (interactive)
                           (split-window-horizontally-n 3)))

;; ファイル名補完時に大文字小文字を区別しない
(setq completion-ignore-case t)

;; インデントをタブを使わないでスペースを使う設定
;; http://d.hatena.ne.jp/stealthinu/20061128/p6
(setq-default tab-width 4 indent-tabs-mode nil)


;; load environment value
;; (load-file (expand-file-name "~/.emacs.d/shellenv.el"))
;; (dolist (path (reverse (split-string (getenv "PATH") ":")))
;;   (add-to-list 'exec-path path))

(setq-default mode-line-format
              '("-"
                mode-line-mule-info
                mode-line-modified
                " "
                mode-line-buffer-identification
                " "
                global-mode-string
                " %[("
                mode-name
                mode-line-process
                minor-mode-alist
                "%n" ")%]"
                (which-func-mode ("" which-func-format "-"))
                "-%-"
                )
              )

;; yes no to y n
;; https://github.com/uwabami/dot.emacs.d/blob/master/config/basic_config.org
(fset 'yes-or-no-p 'y-or-n-p)

;; 空になったファイルを削除
;; https://github.com/uwabami/dot.emacs.d/blob/master/config/basic_config.org
(if (not (memq 'delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'delete-file-if-no-contents after-save-hook)))
(defun delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))

;; isearch
;; リージョン選択後にC-s, C-rした場合、選択した文字列を検索してくれる
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

;;
;; コマンド設定
;;

;; delete
(global-set-key "\C-h" 'delete-backward-char)
(global-set-key "\C-?" 'help-command)
(define-key isearch-mode-map "\C-h" 'isearch-delete-char)

;; buffer
(global-set-key "\C-x\C-b" 'bs-show)

(global-set-key "\C-x\C-k" 'kill-this-buffer)

;; kill-ring
;; (global-set-key (kbd "M-y") 'anything-show-kill-ring)

;; split window horizontally
(global-set-key (kbd "S-t") 'split-window-horizontally)

;; Return key to newline and indent
(global-set-key (kbd "RET") 'newline-and-indent)

;; comment-out
(global-set-key (kbd "C-/") 'comment-dwim)

;; query-replace
(global-set-key (kbd "C-x C-q") 'query-replace)

;; shell-mode
;; shell-modeで矢印キーの上下でコマンド履歴を表示
;; (setq shell-mode-hook
;;       (function (lambda ()
;;                   (define-key shell-mode-map [up] 'comint-previous-input)
;;                   (define-key shell-mode-map [down] 'comint-next-input))))

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
(add-to-load-path "elisp" "auto-install" "public_repos" "snippets")

;; 単語の削除
(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (kbd "M-d") 'delete-word)

;(global-set-key (kbd "RET") 'newline-and-indent)

;; 行頭とインデントの頭
;; http://gifnksm.hatenablog.jp/entry/20100131/1264956220
(defun beginning-of-indented-line (current-point)
  "インデント文字を飛ばした行頭に戻る。ただし、ポイントから行頭までの間にインデント文字しかない場合は、行頭に戻る。"
  (interactive "d")
  (if (string-match
       "^[ \t]+$"
       (save-excursion
         (buffer-substring-no-properties
          (progn (beginning-of-line) (point))
          current-point)))
      (beginning-of-line)
    (back-to-indentation)))

(defun beginning-of-visual-indented-line (current-point)
  "インデント文字を飛ばした行頭に戻る。ただし、ポイントから行頭までの間にインデント文 字しかない場合は、行頭に戻る。"
  (interactive "d")
  (let ((vhead-pos (save-excursion (progn (beginning-of-visual-line) (point))))
        (head-pos (save-excursion (progn (beginning-of-line) (point)))))
    (cond
     ;; 物理行の1行目にいる場合
     ((eq vhead-pos head-pos)
      (if (string-match
           "^[ \t]+$"
           (buffer-substring-no-properties vhead-pos current-point))
          (beginning-of-visual-line)
        (back-to-indentation)))
     ;; 物理行の2行目以降の先頭にいる場合
     ((eq vhead-pos current-point)
      (backward-char)
      (beginning-of-visual-indented-line (point)))
     ;; 物理行の2行目以降の途中にいる場合
     (t (beginning-of-visual-line)))))

(global-set-key "\C-a" 'beginning-of-visual-indented-line)

;; 行頭とインデントの頭
;; http://d.hatena.ne.jp/kitokitoki/20100131/p4
;; (defun my-move-beginning-of-line ()
;;   (interactive)
;;   (if (bolp)
;;       (back-to-indentation)    
;;       (beginning-of-line)))

;; (global-set-key "\C-a" 'my-move-beginning-of-line)

;; package 
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacsの設定（外部ソース利用）
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(require 'drill-instructor)
(setq drill-instructor-global t)

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
(global-auto-complete-mode t)

;; 辞書設定
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(global-auto-complete-mode t)

(setq ac-auto-start 3)
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

;; 現在編集中の関数を表示する
(require 'which-func)
(which-func-mode t)

;; imenu-tree
;(require 'imenu-tree)
;(global-set-key (kbd "C-x f") 'imenu-tree)

;; undo-tree
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; undohistの設定
(when (require 'undohist nil t)
  (undohist-initialize))

;; recentf-ext
;(require 'recentf-ext)
(when (require 'recentf-ext nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  ;(setq recentf-auto-cleanup 10)
  (setq recentf-auto-save-timer
        (run-with-idle-timer 30 t 'recentf-save-list))
  (recentf-mode 1))

;; ブロックの折畳みと展開
;; http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/fold-dwim.el
(when (require 'fold-dwim nil t)
  (require 'hideshow nil t)
  ;; 機能を利用するメジャーモード一覧
  (let ((hook))
    (dolist (hook
             '(emacs-lisp-mode-hook
               c-mode-common-hook
               python-mode-hook
               php-mode-hook
               ruby-mode-hook
               js2-mode-hook
               js3-mode-hook
               css-mode-hook
               apples-mode-hook))
      (add-hook hook 'hs-minor-mode))))

;; wgrep
(require 'wgrep nil t)
(setq wgrep-enable-key "r")

;; grep-edit
(require 'grep-edit)

(global-set-key (kbd "C-x C-r") 'rgrep)
(global-set-key (kbd "C-x C-l") 'lgrep)

;; maxframe
;; https://github.com/rmm5t/maxframe.el
(require 'maxframe)
(global-set-key (kbd "M-RET") 'maximize-frame)
(global-set-key (kbd "C-M-<return>") 'restore-frame)
(add-hook 'window-setup-hook 'maximize-frame t)
;(add-hook 'before-make-frame-hook 'maximize-frame t)

;; saveplace
;; https://github.com/uwabami/dot.emacs.d/blob/master/config/basic_config.org
;; (require 'saveplace)
;; (setq-default save-place t)
;; (setq save-place-file
;;       (convert-standard-filename
;;        (concat my:user-emacs-temporary-directory "emacs-places")))

;; ;; powerline
;; (require 'powerline)

;; multiple selection and edition
(require 'iedit)

;; thing
;; region selection with a word in.
(require 'thing-opt)
(define-thing-commands)
(global-set-key (kbd "C-$") 'mark-word*)
(global-set-key (kbd "C-\"") 'mark-string)
(global-set-key (kbd "C-(") 'mark-up-list)

;; kill-ringを使いやすく
(require 'browse-kill-ring)

;; buffer move
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; hi-lock
(global-hi-lock-mode 1)

;; gtags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GNU GLOBAL(gtags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'gtags)
;; (require 'anything-gtags)
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
      '(lambda ()
         (local-set-key (kbd "C-c C-t") 'gtags-find-tag)
         (local-set-key (kbd "C-c C-r") 'gtags-find-rtag)
         (local-set-key (kbd "C-c C-s") 'gtags-find-symbol)
         (local-set-key (kbd "C-c C-p") 'gtags-pop-stack)
         ))
(setq gtags-path-style 'relative)

;; uniquify
;; 同名ファイルを賢く開く
;; http://shibayu36.hatenablog.com/entry/2012/12/29/001418
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; imenu
(require 'imenu-tree)
(global-set-key (kbd "C-x f") 'imenu-tree)
(setq imenu-tree-auto-update t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anything
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;
;; ;;anything-c-source-R-helpの設定
;; ;;
;; (setq anything-c-source-R-help
;; '((name . "R objects / help")
;; (init . (lambda ()
;; ;; this grabs the process name associated with the buffer
;; (setq anything-c-ess-local-process-name ess-local-process-name)))
;; (candidates . (lambda ()
;; (condition-case nil
;; (ess-get-object-list anything-c-ess-local-process-name)
;; (error nil))))
;; (action
;; ("help" . ess-display-help-on-object)
;; ("head (10)" . (lambda(obj-name)
;; (ess-execute (concat "head(" obj-name ", n = 10)n") nil (concat "R head: " obj-name))))
;; ("head (100)" . (lambda(obj-name)
;; (ess-execute (concat "head(" obj-name ", n = 100)n") nil (concat "R head: " obj-name))))
;; ("tail" . (lambda(obj-name)
;; (ess-execute (concat "tail(" obj-name ", n = 10)n") nil (concat "R tail: " obj-name))))
;; ("str" . (lambda(obj-name)
;; (ess-execute (concat "str(" obj-name ")n") nil (concat "R str: " obj-name))))
;; ("summary" . (lambda(obj-name)
;; (ess-execute (concat "summary(" obj-name ")n") nil (concat "R summary: " obj-name))))
;; ("view source" . (lambda(obj-name)
;; (ess-execute (concat "print(" obj-name ")n") nil (concat "R object: " obj-name))))
;; ("dput" . (lambda(obj-name)
;; (ess-execute (concat "dput(" obj-name ")n") nil (concat "R dput: " obj-name)))))
;; (volatile)))

;; ;;
;; ;; anything-c-source-R-localの設定
;; ;;
;; (setq anything-c-source-R-local
;; '((name . "R local objects")
;; (init . (lambda ()
;; ;; this grabs the process name associated with the buffer
;; (setq anything-c-ess-local-process-name ess-local-process-name)
;; ;; this grabs the buffer for later use
;; (setq anything-c-ess-buffer (current-buffer))))
;; (candidates . (lambda ()
;; (let (buf)
;; (condition-case nil
;; (with-temp-buffer
;; (progn
;; (setq buf (current-buffer))
;; (with-current-buffer anything-c-ess-buffer
;; (ess-command "print(ls.str(), max.level=0)n" buf))
;; (split-string (buffer-string) "n" t)))
;; (error nil)))))
;; (display-to-real . (lambda (obj-name) (car (split-string obj-name " : " t))))
;; (action
;; ("str" . (lambda(obj-name)
;; (ess-execute (concat "str(" obj-name ")n") nil (concat "R str: " obj-name))))
;; ("summary" . (lambda(obj-name)
;; (ess-execute (concat "summary(" obj-name ")n") nil (concat "R summary: " obj-name))))
;; ("head (10)" . (lambda(obj-name)
;; (ess-execute (concat "head(" obj-name ", n = 10)n") nil (concat "R head: " obj-name))))
;; ("head (100)" . (lambda(obj-name)
;; (ess-execute (concat "head(" obj-name ", n = 100)n") nil (concat "R head: " obj-name))))
;; ("tail" . (lambda(obj-name)
;; (ess-execute (concat "tail(" obj-name ", n = 10)n") nil (concat "R tail: " obj-name))))
;; ("print" . (lambda(obj-name)
;; (ess-execute (concat "print(" obj-name ")n") nil (concat "R object: " obj-name))))
;; ("dput" . (lambda(obj-name)
;; (ess-execute (concat "dput(" obj-name ")n") nil (concat "R dput: " obj-name)))))
;; (volatile)))

;; ; anything関係
;; (require 'anything)
;; ;(defvar org-directory "")
;; (require 'anything-startup)

;; (require 'anything-config)
;; (setq anything-sources (list anything-c-source-buffers
;;                              anything-c-source-emacs-commands
;;                              anything-c-source-bookmarks
;;                              anything-c-source-recentf
;;                              anything-c-source-file-name-history
;;                              anything-c-source-man-pages
;;                              anything-c-source-occur
;;                              anything-c-source-R-help
;;                              anything-c-source-locate))
;; (define-key anything-map (kbd "C-p") 'anything-previous-line)
;; (define-key anything-map (kbd "C-n") 'anything-next-line)
;; (define-key anything-map (kbd "C-v") 'anything-next-source)
;; (define-key anything-map (kbd "M-v") 'anything-previous-source)
;; ;(global-set-key (kbd "C-;") 'anything)
;; (global-set-key (kbd "C-:") 'anything-filelist+):

;; ;; anything filelist
;; (setq anything-c-filelist-file-name "/tmp/all.filelist")
;; (setq anything-grep-candidates-fast-directory-regexp "^/tmp")

;; (define-key global-map (kbd "C-x C-i") 'anything-imenu)

;; ESS
;; R util
(autoload 'ess-site "ess-site" nil t)

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

;; ;;; anything-c-moccurの設定
;; (require 'anything-c-moccur)
;; ;; カスタマイズ可能変数の設定(M-x customize-group anything-c-moccur でも設定可能)
;; (setq anything-c-moccur-anything-idle-delay 0.2 ;`anything-idle-delay'
;;       anything-c-moccur-higligt-info-line-flag t ; `anything-c-moccur-dmoccur'などのコマンドでバッファの情報をハイライトする
;;       anything-c-moccur-enable-auto-look-flag t ; 現在選択中の候補の位置を他のwindowに表示する
;;       anything-c-moccur-enable-initial-pattern t) ; `anything-c-moccur-occur-by-moccur'の起動時にポイントの位置の単語を初期パターンにする

;; ;;; キーバインドの割当(好みに合わせて設定してください)
;; (global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur) ;バッファ内検索
;; (global-set-key (kbd "C-M-o") 'anything-c-moccur-dmoccur) ;ディレクトリ
;; (add-hook 'dired-mode-hook ;dired
;;           '(lambda ()
;;              (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur)))

;; ;; ctags + anything
;; (when (require 'anything nil t)
;;   (require 'anything-exuberant-ctags)
;;   )
;; (define-key global-map (kbd "C-@") 'anything-exuberant-ctags-select-from-here)

;; (require 'anything-yaetags)
;; (add-to-list 'anything-sources 'anything-c-source-yaetags-select)
;; (define-key global-map (kbd "M-@") 'anything-yaetags-find-tag)


;; junk file
;; http://shibayu36.hatenablog.com/entry/2012/12/29/001418
(require 'open-junk-file)
(setq open-junk-file-format "~/Documents/junk/%Y-%m-%d-%H%M%S.")

;;;
;;; org-mode
;;;
;; org-modeの初期化
;; (require 'org-install)
;; ;; キーバインドの設定
;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (define-key global-map "\C-cr" 'org-remember)
;; ;; 拡張子がorgのファイルを開いた時，自動的にorg-modeにする
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; ;; org-modeでの強調表示を可能にする
;; (add-hook 'org-mode-hook 'turn-on-font-lock)
;; ;; 見出しの余分な*を消す
;; (setq org-hide-leading-stars t)
;; ;; org-default-notes-fileのディレクトリ
;; (setq org-directory "~/org/")
;; ;; org-default-notes-fileのファイル名
;; (setq org-default-notes-file "notes.org")

;; (add-hook 'org-mode-hook
;;           '(lambda ()
;;              (define-key org-mode-map [(control tab)] nil)))

;; ;; Make windmove work in org-mode:
;; (add-hook 'org-shiftup-final-hook 'windmove-up)
;; (add-hook 'org-shiftleft-final-hook 'windmove-left)
;; (add-hook 'org-shiftdown-final-hook 'windmove-down)
;; (add-hook 'org-shiftright-final-hook 'windmove-right)

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
(global-set-key (kbd "C-t") 'tabbar-forward-tab)
(global-set-key (kbd "C-q") 'tabbar-backward-tab)

;; ;; タブ上をマウス中クリックで kill-buffer
;; (defun my-tabbar-buffer-help-on-tab (tab)
;;   "Return the help string shown when mouse is onto TAB."
;;   (if tabbar--buffer-show-groups
;;       (let* ((tabset (tabbar-tab-tabset tab))
;;              (tab (tabbar-selected-tab tabset)))
;;         (format "mouse-1: switch to buffer %S in group [%s]"
;;                 (buffer-name (tabbar-tab-value tab)) tabset))
;;     (format "\
;; mouse-1: switch to buffer %S\n\
;; mouse-2: kill this buffer\n\
;; mouse-3: delete other windows"
;;             (buffer-name (tabbar-tab-value tab)))))

;; (defun my-tabbar-buffer-select-tab (event tab)
;;   "On mouse EVENT, select TAB."
;;   (let ((mouse-button (event-basic-type event))
;;         (buffer (tabbar-tab-value tab)))
;;     (cond
;;      ((eq mouse-button 'mouse-2)
;;       (with-current-buffer buffer
;;         (kill-buffer)))
;;      ((eq mouse-button 'mouse-3)
;;       (delete-other-windows))
;;      (t
;;       (switch-to-buffer buffer)))
;;     ;; Don't show groups.
;;     (tabbar-buffer-show-groups nil)))

;; (setq tabbar-help-on-tab-function 'my-tabbar-buffer-help-on-tab)
;; (setq tabbar-select-tab-function 'my-tabbar-buffer-select-tab)

;; popwin
(require 'popwin)
;(setq display-buffer-function 'popwin:display-buffer)
(setq special-display-function 'popwin:special-display-popup-window)
(setq special-display-buffer-names '("*compilation*" "*anything*" "*anything file list*" "*anything kill-ring*" " *undo-tree*" "*imenu-tree*" "*imenu*"))
(setq popwin:popup-window-height 0.4)
(setq anything-samewindow nil)
(push '("*compilation*" :height 20) popwin:special-display-config)
(push '("*anything*" :height 20) popwin:special-display-config)
(push '("*anything file list*" :height 20) popwin:special-display-config)
(push '("*anything kill-ring*" :height 20) popwin:special-display-config)
(push '("*anything yas/prompt*" :position bottom :height 20) popwin:special-display-config)
(push '(" *undo-tree*" :position auto) popwin:special-display-config)
(push '("*imenu-tree*" :position right) popwin:special-display-config)
(push '("*imenu*" :position right) popwin:special-display-config)

;; direx
(require 'direx)
(push '(direx:direx-mode :position left :width 40 :dedicated t)
      popwin:special-display-config)

(setq direx:leaf-icon "  "
      direx:open-icon "▾ "
      direx:closed-icon "▸ ")

;(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)

;; http://shibayu36.hatenablog.com/entry/2013/01/26/194741
(defun chomp (str)
  (replace-regexp-in-string "[\n\r]+$" "" str))

(defun git-project-p ()
  (string=
   (chomp
    (shell-command-to-string "git rev-parse --is-inside-work-tree"))
   "true"))

(defun git-root-directory ()
  (cond ((git-project-p)
         (chomp
          (shell-command-to-string "git rev-parse --show-toplevel")))
        (t
         "")))

(defun direx:jump-to-git-project-directory ()
  (interactive)
  (let* ((git-root-dir))
    (setq git-root-dir (git-root-directory))
    (unless (string= git-root-dir "")
      (direx:find-directory-noselect git-root-dir))
    (direx:jump-to-directory)))
;    (direx:jump-to-directory-other-window)))

(global-set-key (kbd "C-x C-j") 'direx:jump-to-git-project-directory)

(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymakeの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flymake-easy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key mode-specific-map "c" 'compile)
(define-key mode-specific-map "s" 'shell)

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

;; scss
(autoload 'scss-mode "scss-mode")
(setq scss-compile-at-save nil) ;; 自動コンパイルをオフにする
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

;; jade-mode
(require 'sws-mode)
(autoload 'jade-mode "jade-mode" nil t)
(autoload 'stylus-mode "stylus-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.styl$" . stylus-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; javascriptの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; js2-mode
;(autoload 'js2-mode "js2-mode" nil t)
;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; js3-mode
(autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
(custom-set-variables
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js3-indent-level 4)
 '(js3-auto-indent-p t)
 '(js3-indent-on-enter-key t)
 '(js3-enter-indents-newline t)
 '(js3-consistent-level-indent-inner-bracket t))

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

;; json.el
(autoload 'json "json" nil t)

;; coffee-script
(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;; nodejs-repl
(autoload 'nodejs-repl "nodejs-repl" "Run Node.js REPL" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; web-modeの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.handlebars\\'" . web-mode))

(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2) ;; for HTML
  (setq web-mode-css-indent-offset    2) ;; for CSS
  (setq web-mode-code-indent-offset   2) ;; for Js, PHP, etc
  (setq web-mode-indent-style   2)
  (setq web-mode-style-padding  2)
  (setq web-mode-script-padding 2)
  )
(add-hook 'web-mode-hook 'web-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rubyの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'ruby-mode "ruby-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.Gemfile$" . ruby-mode))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (setq tab-width 2)
             (setq ruby-indent-level tab-width)
             ;(setq ruby-deep-indent-paren-style nil)
             ;(define-key ruby-mode-map [return] 'ruby-reindent-then-newline-and-indent)
             ))

(when (require 'ruby-block nil t)
  (ruby-block-mode t)
  (setq ruby-block-highlight-toggle t))

(when (require 'ruby-electric nil t)
  (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
  (setq ruby-electric-expand-delimiters-list nil))

(when (require 'rcodetools nil t)
  (define-key ruby-mode-map (kbd "<C-j>") 'rct-complete-symbol))

; http://qiita.com/akisute3@github/items/f02ab0c38ad5e9ba385a
(require 'smart-compile)
(define-key ruby-mode-map (kbd "C-c c") 'smart-compile)
(define-key ruby-mode-map (kbd "C-c C-c") (kbd "C-c c C-m"))

; https://github.com/akisute3/anything-ruby-methods
;; (require 'anything-ruby-methods)
;; (define-key ruby-mode-map (kbd "C-c w") 'anything-ruby-methods)

;;
;; rails - rinari
;;

;; Interactively Do Things (highly recommended, but not strictly required)
(require 'ido)
(ido-mode t)
;; Rinari
(require 'rinari)
(global-rinari-mode)

;; <<< rinari commands
;; C-c ; f f	RAILS_ROOT/
;; C-c ; f c	app/controller/
;; C-c ; f m	app/models/
;; C-c ; f v	app/views/
;; C-c ; f h	app/helper/
;; C-c ; f i	db/migrate/
;; C-c ; f n	config/
;; C-c ; f e	config/environment/
;; C-c ; f j	pubic/javascript/
;; C-c ; f l	vendor/plugin/
;; C-c ; f o	log/
;; C-c ; f p	public/
;; C-c ; f s	script/
;; C-c ; f t	test/
;; C-c ; f w	lib/workers/
;; C-c ; f x	test/fixtures/
;; C-c ; f y	public/stylesheets/
;; C-c ; f r	spec/
;; C-c ; f z	spec/fixtures
;; >>>

;; yasnippetのロード
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"
                         "~/.emacs.d/public_repos/yasnippet/snippets"
                         "~/.emacs.d/public_repos/yasnippets-rails/rails-snippets"
                         "~/.emacs.d/public_repos/js-yasnippets"
                         ))
(yas-global-mode 1)
(custom-set-variables '(yas-trigger-key "TAB"))

;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)
(define-key yas-minor-mode-map (kbd "C-x i r") 'yas-reload-all)

;; ;; anything interface
;; (eval-after-load "anything-config"
;;   '(progn
;;      (defun my-yas/prompt (prompt choices &optional display-fn)
;;        (let* ((names (loop for choice in choices
;;                            collect (or (and display-fn (funcall display-fn choice))
;;                                        choice)))
;;               (selected (anything-other-buffer
;;                          `(((name . ,(format "%s" prompt))
;;                             (candidates . names)
;;                             (action . (("Insert snippet" . (lambda (arg) arg))))))
;;                          "*anything yas/prompt*")))
;;          (if selected
;;              (let ((n (position selected names :test 'equal)))
;;                (nth n choices))
;;            (signal 'quit "user quit!"))))
;;      (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))
;;      (define-key anything-command-map (kbd "y") 'yas/insert-snippet)))

;; haml-mode
(autoload 'haml-mode "haml-mode" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PHPの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'php-mode "php-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.php5$" . php-mode))

(add-hook 'php-mode-hook
          (lambda ()
            (defun ywb-php-lineup-arglist-intro (langelem)
              (save-excursion
                (goto-char (cdr langelem))
                (vector (+ (current-column) c-basic-offset))))
            (defun ywb-php-lineup-arglist-close (langelem)
              (save-excursion
                (goto-char (cdr langelem))
                (vector (current-column))))
            (c-set-style "stroustrup")    ; インデントは4文字分基本スタイル
            (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro) ; 配列のインデント関係
            (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close) ; 配列のインデント関係
            (c-set-offset 'arglist-cont-nonempty' 4) ; 配列のインデント関係
            (c-set-offset 'case-label' 0) ; case はインデントする
            (make-local-variable 'tab-width)
            (make-local-variable 'indent-tabs-mode)
            (setq tab-width 4)
            (setq indent-tabs-mode nil)
            ))

(add-hook  'php-mode-hook
           (lambda ()
             (when (require 'auto-complete nil t)
               (make-variable-buffer-local 'ac-sources)
               (add-to-list 'ac-sources 'ac-source-php-completion)
               ;; if you like patial match,
               ;; use `ac-source-php-completion-patial' instead of `ac-source-php-completion'.
               ;; (add-to-list 'ac-sources 'ac-source-php-completion-patial)
               (auto-complete-mode t))))

(add-hook 'php-mode-hook
         (lambda ()
             (require 'php-completion)
             (php-completion-mode t)
             (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)
             (when (require 'auto-complete nil t)
             (make-variable-buffer-local 'ac-sources)
             (add-to-list 'ac-sources 'ac-source-php-completion)
             (auto-complete-mode t))))

;; flymake
(require 'flymake-php)
(add-hook 'php-mode-hook 'flymake-php-load)

;; ;; flymake_phpcs
;; ;; If flymake_phpcs isn't found correctly, specify the full path
;; (setq flymake-phpcs-command "~/.emacs.d/public_repos/emacs-flymake-phpcs/bin/flymake_phpcs")

;; ;; Customize the coding standard checked by phpcs
;; (setq flymake-phpcs-standard
;;       "~/lib/PHP_CodeSniffer/PHP_CodeSniffer/CodeSniffer/Standards/PHPCS")

;; ;; Show the name of sniffs in warnings (eg show
;; ;; "Generic.CodeAnalysis.VariableAnalysis.UnusedVariable" in an unused
;; ;; variable warning)
;; (setq flymake-phpcs-show-rule t)

;; (require 'flymake-phpcs)

;; 自動挿入
(auto-insert-mode)
(setq auto-insert-directory "~/.emacs.d/insert/")
(define-auto-insert "\\.php$" "php-template.php")
(define-auto-insert "\\.html$" "html-template.html")
(define-auto-insert "\\.js$" "js-template.js")
;; (define-auto-insert "\\.php$" "php-html-template.php")

;; Show Git branch information to mode-line
;; this is heavy !!
;; (let ((cell (or (memq 'mode-line-position mode-line-format)
;;                 (memq 'mode-line-buffer-identification mode-line-format)))
;;       (newcdr '(:eval (my/update-git-branch-mode-line))))
;;   (unless (member newcdr mode-line-format)
;;     (setcdr cell (cons newcdr (cdr cell)))))

;; (defun my/update-git-branch-mode-line ()
;;   (let* ((branch (replace-regexp-in-string
;;                   "[\r\n]+\\'" ""
;;                   (shell-command-to-string "git symbolic-ref -q HEAD")))
;;          (mode-line-str (if (string-match "^refs/heads/" branch)
;;                             (format "[%s]" (substring branch 11))
;;                           "[Not Repo]")))
;;     (propertize mode-line-str
;;                 'face '((:foreground "Dark green" :weight bold)))))

;; rainbow-mode
;; http://qiita.com/ironsand/items/cf8c582da3ec20715677
(require 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)
(add-hook 'scss-mode-hook 'rainbow-mode)
(add-hook 'php-mode-hook 'rainbow-mode)
(add-hook 'html-mode-hook 'rainbow-mode)

;; ;(require 'nginx-mode)
;; ;(add-to-list 'auto-mode-alist '("/usr/local/etc/nginx/site-available/.*" . nginx-mode))
