;;; package-json.el --- Selection package  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/sebastiencs/package-json.el
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1") (dash "2.13"))
;; Version: 0.0.1

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.


;;; Commentary:
;;
;; A lightweight completion/selection system for Emacs
;;

;;; Code:

(require 'icons-in-terminal)
(require 'frame-local)
(require 'json)
(require 'dash)
(require 'subr-x)
(require 'markdown-mode)

(defvar-local package-json--deps nil)

(defun package-json--region (path)
  (let (region)
    (save-excursion
      (goto-char 1)
      (while (and (not (eobp)) (not region))
        (let ((json-path (json-path-to-position (point))))
          (cond ((equal (plist-get json-path :path) (list path))
                 (setq region (cons (plist-get json-path :match-start)
                                    (plist-get json-path :match-end))))
                ((plist-get json-path :match-end)
                 (goto-char (plist-get json-path :match-end))))
          (forward-line))))
    region))

(defun package-json--get-deps (path)
  (-when-let* ((region (package-json--region path)))
    (save-excursion
      (goto-char (car region))
      (let ((obj (json-read))
            deps)
        (goto-char (1+ (car region)))
        (while (< (point) (cdr region))
          (-when-let* ((pkg (ignore-errors (json-read)))
                       (version (alist-get (intern pkg) obj))
                       (line (line-number-at-pos)))
            (push (list :line line :pkg pkg :version version) deps))
          (forward-line))
        (nreverse deps)))))

(defvar url-http-end-of-headers)

(defun package-json--reset-ov (pkgs)
  (remove-overlays nil nil 'package-json t)
  (dolist (pkg pkgs)
    (save-excursion
      (goto-char 1)
      (forward-line (1- (plist-get pkg :line)))
      (goto-char (line-beginning-position))
      (let* ((start (point))
             (distance (skip-chars-forward "\t\r\n "))
             (ov (make-overlay start (+ start distance))))
        ;; (overlay-put ov 'display (concat " " (icons-in-terminal 'md_loop :foreground "#607D8B")))
        (overlay-put ov 'display (concat " " (icons-in-terminal 'md_refresh :foreground "#607D8B")))
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'after-string
                     (propertize " " 'display `(space :align-to (+ left-margin ,distance 1))))
        (overlay-put ov 'package-json t)))))

(defun package-json--update-ov (pkg data &optional on-pkg)
  (save-excursion
    (goto-char 1)
    (forward-line (1- (plist-get pkg :line)))
    (let* ((ov (--first (overlay-get it 'package-json) (overlays-in (line-beginning-position) (line-end-position))))
           (current (string-trim-left (plist-get pkg :version) "\\^"))
           (latest (-some->> (plist-get data :tags)
                             (gethash "latest")))
           ;;;(latest (alist-get 'latest (plist-get data :tags)))
           (icon (cond ((null latest) (icons-in-terminal 'fa_question_circle_o))
                       ;; ((ignore-errors (version< current latest)) (icons-in-terminal 'md_file_upload))
                       ((ignore-errors (version< current latest)) (icons-in-terminal 'md_update))
                       (t " "))))
      (if on-pkg
          (overlay-put ov 'display (concat " " (icons-in-terminal 'md_arrow_forward)))
        (overlay-put ov 'display (concat " " icon))))))

(defun package-json--make-data (object current-version)
  (when object
    (list :name (-some-> (gethash "name" object) (decode-coding-string 'utf-8 t))
          :tags (gethash "dist-tags" object)
          :new-available (-some--> (gethash "dist-tags" object)
                                   (gethash "latest" it)
                                   (and (ignore-errors (version< (string-trim-left current-version "\\^") it))
                                        it))
          :description (-some-> (gethash "description" object) (decode-coding-string 'utf-8 t))
          :homepage (-some-> (gethash "homepage" object) (decode-coding-string 'utf-8 t))
          :readme (-some-> (gethash "readme" object) (decode-coding-string 'utf-8 t)))
    ))

;; (defun package-json nil
;;   (let* ((current-buffer (current-buffer))
;;          (deps (package-json--get-deps "dependencies"))
;;          (devdeps (package-json--get-deps "devDependencies"))
;;          (all (-concat deps devdeps)))
;;     (package-json--reset-ov all)
;;     (redisplay)
;;     (dolist (pkg all)
;;       (package-json--update-ov pkg nil t)
;;       (redisplay)
;;       (-when-let* ((buffer (url-retrieve-synchronously (concat "https://registry.npmjs.org/" (plist-get pkg :pkg))
;;                                                        t)))
;;         (with-current-buffer buffer
;;           (goto-char (1+ url-http-end-of-headers))
;;           (let* ((obj (json-parse-buffer))
;;                  (data (package-json--make-data obj (plist-get pkg :version))))
;;             (plist-put pkg :data data)
;;             (with-current-buffer current-buffer
;;               (package-json--update-ov pkg data))
;;             (redisplay)
;;             (kill-buffer)))))
;;     (setq package-json--deps all)
;;     nil))

(defun package-json--fetch-and-update (pkg current-buffer)
  (url-retrieve (concat "https://registry.npmjs.org/" (plist-get pkg :pkg))
                (lambda (&rest _)
                  (goto-char (1+ url-http-end-of-headers))
                  ;; (let* ((obj (json-read))
                  (let* ((obj (json-parse-buffer))
                         (data (package-json--make-data obj (plist-get pkg :version))))
                    (when (plist-get data :name)
                      (plist-put pkg :data data))
                    (with-current-buffer current-buffer
                      (package-json--update-ov pkg data))
                    (redisplay)
                    (kill-buffer)))
                nil t))

(defun package-json nil
  (let* ((current-buffer (current-buffer))
         (deps (package-json--get-deps "dependencies"))
         (devdeps (package-json--get-deps "devDependencies"))
         (all (-concat deps devdeps)))
    (package-json--reset-ov all)
    (redisplay)
    (dolist (pkg all)
      (package-json--fetch-and-update pkg current-buffer))
    (setq package-json--deps all)
    nil))

(defun package-json--update nil
  (let* ((deps (package-json--get-deps "dependencies"))
         (devdeps (package-json--get-deps "devDependencies"))
         (all (-concat deps devdeps))
         new-list)
    (dolist (pkg all)
      (let ((new pkg))
        (-some--> (--first (equal (plist-get it :pkg) (plist-get pkg :pkg)) package-json--deps)
                  (and (equal (plist-get it :version) (plist-get pkg :version)) it)
                  (plist-put new :data (plist-get it :data)))
        (push new new-list)))
    (setq package-json--deps new-list)
    nil))

;; (defun package-json-thread nil
;;   (make-thread 'package-json "OTHER THREAD"))

(defmacro package-json--get (variable)
  (let ((var (intern (format "package-json-%s" variable))))
    `(frame-local-get ',var (frame-parent))))

(defmacro package-json--set (variable value)
  (let ((var (intern (format "package-json-%s" variable))))
    `(frame-local-set ',var ,value (frame-parent))))

(defun package-json--clean nil
  (-some-> (package-json--get frame) (make-frame-invisible))
  (dolist (pkg package-json--deps)

    (save-excursion
      (goto-char 1)
      (forward-line (1- (plist-get pkg :line)))
      (let* ((ov (--first (overlay-get it 'package-json) (overlays-in (line-beginning-position) (line-end-position)))))
        (overlay-put ov 'display (concat " "))))))

(defun package-json-open-readme-at-point nil
  "Open the readme of the package on the current line."
  (interactive)
  (-when-let* ((line (line-number-at-pos))
               (pkg (--first (equal (plist-get it :line) line) package-json--deps))
               (name (plist-get pkg :pkg))
               (data (plist-get pkg :data)))
    (-> (with-current-buffer (get-buffer-create (concat "*README-" name "*"))
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert (plist-get data :readme))
          (markdown-view-mode)
          (goto-char 1)
          (setq-local markdown-fontify-code-blocks-natively t)
          (setq-local markdown-hide-markup-in-view-modes t)
          (setq-local markdown-hide-markup t)
          (setq-local markdown-hide-urls t)
          (local-set-key (kbd "q") #'(lambda nil (interactive) (quit-restore-window nil 'kill)))
          (current-buffer))
        (pop-to-buffer))
    (-some-> (package-json--get frame)
             (make-frame-invisible))))

(require 'browse-url)

(defun package-json-browse-package-homepage-at-point nil
  "Open the package homepage in the browser."
  (interactive)
  (-when-let* ((line (line-number-at-pos))
               (pkg (--first (equal (plist-get it :line) line) package-json--deps))
               (data (plist-get pkg :data)))
    (browse-url (plist-get data :homepage))))

(defvar package-json-frame-parameters
  '((left . -1)
    (no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width  . 0)
    (width  . 0)
    (min-height  . 0)
    (height  . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (right-fringe . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (line-spacing . 0)
    (unsplittable . t)
    (undecorated . t)
    (top . -1)
    (mouse-wheel-frame . nil)
    (no-other-frame . t)
    (cursor-type . nil)
    (inhibit-double-buffering . t)
    (drag-internal-border . t)
    (no-special-glyphs . t))
  "Frame parameters used to create the frame.")

(defface package-json-url
  '((t :inherit link))
  "Face used on links."
  :group 'package-json)

(defun package-json--make-clickable (string)
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] 'browse-url-at-mouse)
    (put-text-property 0 (length string) 'keymap map string)
    (put-text-property 0 (length string) 'mouse-face
                       (list :inherit 'package-json-url
                             :box (list :line-width -1
                                        :color (face-foreground 'package-json-url)))
                       string)
    (add-face-text-property 0 (length string) 'link t string)
    string))

(defun package-json--make-buffer (data)
  (with-current-buffer (get-buffer-create "*package-json*")
    (erase-buffer)
    (setq mode-line-format nil
          header-line-format nil)
    (insert (string-join (-filter 'identity
                                  (list (plist-get data :description)
                                        (-some->> (plist-get data :new-available)
                                                  (concat "Latest version: "))
                                        (-> (plist-get data :homepage)
                                            (package-json--make-clickable))))
                         "\n\n"))
    (current-buffer)))

(defun package-json--post-command nil
  (let* ((line (line-number-at-pos))
         (pkg (--first (equal (plist-get it :line) line) package-json--deps))
         (data (plist-get pkg :data)))
    (if (not data)
        (-some->> (package-json--get frame)
                  (make-frame-invisible))
      (run-with-idle-timer
       0.1 nil
       (lambda nil
         (when (= line (line-number-at-pos))
           (let* ((buffer (package-json--make-buffer data))
                  (frame (package-json--make-frame buffer)))
             (package-json--move-frame frame))))))))

(defun package-json--make-frame (buffer)
  (-if-let* ((frame (package-json--get frame)))
      (progn (make-frame-visible frame)
             frame)
    (let* ((after-make-frame-functions nil)
           (before-make-frame-hook nil)
           (params (append package-json-frame-parameters
                           `((default-minibuffer-frame . ,(selected-frame))
                             (minibuffer . ,(minibuffer-window))
                             (background-color . "#272A36")
                             (internal-border-width . ,(frame-char-width))
                             (parent-frame . ,(selected-frame)))))
           (frame (make-frame params))
           (window (frame-selected-window frame)))
      (set-window-buffer window buffer)
      (set-window-dedicated-p window t)
      (redirect-frame-focus frame (frame-parent frame))
      (package-json--set frame frame)
      frame)))

(defun package-json--move-frame (frame)
  (-let* ((window (frame-root-window frame))
          ((width . height) (window-text-pixel-size window nil nil (* (frame-char-width) 60) 10000 t))
          (frame-resize-pixelwise t))
    (set-frame-size frame width height t)
    (set-frame-position frame (- (frame-pixel-width) width 20 (* (frame-char-width) 2)) 10)))

(defun package-json--update-data nil
  (dolist (pkg package-json--deps)
    (unless (plist-member pkg :data)
      (package-json--update-ov pkg nil t)
      (package-json--fetch-and-update pkg (current-buffer)))))

(defun package-json--after-changes (_start _end _len)
  (ignore-errors
    (package-json--update)
    (package-json--update-data)))

(define-minor-mode package-json-mode
  "Minor mode for package-json."
  :init-value nil
  :group package-json
  (cond
   (package-json-mode
    (add-hook 'post-command-hook 'package-json--post-command nil t)
    (add-hook 'after-change-functions 'package-json--after-changes nil t)
    (package-json)
    )
   (t
    (remove-hook 'post-command-hook 'package-json--post-command t)
    (remove-hook 'after-change-functions 'package-json--after-changes t)
    (remove-overlays nil nil 'package-json t))))

(provide 'package-json)
;;; package-json.el ends here
