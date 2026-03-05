;;; mo.el --- Emacs integration for mo (Markdown viewer) -*- lexical-binding: t -*-

;; Author: matsuyoshi30
;; URL: https://github.com/matsuyoshi30/mo.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Preview Markdown files using mo (https://github.com/k1LoW/mo) inside Emacs.
;;
;; mo starts a local server that renders Markdown with GitHub-flavored styling,
;; syntax highlighting, Mermaid diagrams, and live-reload on save.
;;
;; This package sends the current Markdown buffer to mo and displays the
;; preview in an xwidget-webkit buffer side by side.
;;
;; Usage:
;;   M-x mo-preview    — start mo and open preview beside the current buffer
;;   M-x mo-shutdown   — stop the running mo server
;;
;; Window behavior:
;;   - If only one window exists, splits horizontally and shows preview
;;   - If already split, creates a new tab-bar tab for the preview workspace

;;; Code:

(defgroup mo nil
  "Emacs integration for mo Markdown viewer."
  :group 'markdown
  :prefix "mo-")

(defcustom mo-executable "mo"
  "Path to the mo executable."
  :type 'string)

(defcustom mo-port 6275
  "Port for the mo server."
  :type 'integer)

(defcustom mo-target "default"
  "Default target group name for mo."
  :type 'string)

(defvar mo--process nil
  "The mo background process.")

(defvar mo--preview-xwidget-buffer nil
  "The xwidget-webkit buffer created by 'mo-preview'.")

(defvar mo--managed-tab nil
  "Name of the tab-bar tab created by mo, or nil if we split instead.")

(defvar-local mo--xwidget nil
  "The xwidget instance used for mo preview.")

(defvar-local mo--sync-heading nil
  "Last synced heading slug, to avoid redundant JS calls.")

(defun mo--preview-url ()
  "Return the localhost URL for the mo preview."
  (format "http://localhost:%d" mo-port))

(defun mo--server-running-p ()
  "Return non-nil if a mo server is responding on `mo-port'."
  (condition-case nil
      (let ((proc (make-network-process
                   :name "mo-check" :host "localhost" :service mo-port
                   :nowait nil)))
        (delete-process proc)
        t)
    (error nil)))

(defun mo--wait-for-server (&optional timeout)
  "Wait up to TIMEOUT seconds (default 5) for the mo server to respond."
  (let ((deadline (+ (float-time) (or timeout 5))))
    (while (and (< (float-time) deadline)
                (not (mo--server-running-p)))
      (sit-for 0.3))
    (mo--server-running-p)))

(defun mo--base-args ()
  "Return the common argument list (-p PORT [-t TARGET]) for mo commands."
  (let ((args (list "-p" (number-to-string mo-port))))
    (unless (string= mo-target "default")
      (setq args (append args (list "-t" mo-target))))
    args))

(defun mo--shutdown-args ()
  "Return the argument list for `mo --shutdown' matching the current config."
  (cons "--shutdown" (mo--base-args)))

(defun mo--send-file (file)
  "Send FILE to the running mo server (or start one)."
  (let* ((args (append (list "--no-open") (mo--base-args) (list file)))
         (proc (apply #'start-process "mo" "*mo-log*" mo-executable args)))
    (unless mo--process
      (setq mo--process proc))
    proc))

(defun mo--xwidget-available-p ()
  "Return non-nil if xwidget-webkit is usable."
  (and (require 'xwidget nil t)
       (fboundp 'xwidget-webkit-browse-url)))

(defun mo--open-xwidget-preview (source-buf)
  "Open the mo preview in an xwidget-webkit buffer.
SOURCE-BUF is the markdown buffer to sync scrolling with."
  (let ((url (mo--preview-url)))
    (if (mo--xwidget-available-p)
        (progn
          (xwidget-webkit-browse-url url t)
          ;; Track the actual xwidget buffer created by xwidget-webkit
          (setq mo--preview-xwidget-buffer (current-buffer))
          ;; Store the last created xwidget in the source buffer for scroll sync
          (let ((xw (car (last (xwidget-list-xwidgets)))))
            (when xw
              (with-current-buffer source-buf
                (setq mo--xwidget xw))))
          ;; When this xwidget buffer is killed, delete the window too
          (let ((buf (current-buffer)))
            (add-hook 'kill-buffer-hook
                      (lambda ()
                        (when-let* ((win (get-buffer-window buf)))
                          (unless (one-window-p)
                            (delete-window win))))
                      nil t)))
      (message "mo: xwidget-webkit not available, falling back to eww (CSS/JS won't render)")
      (eww url))))

;;; Scroll sync — heading-based

(defun mo--heading-slug (text)
  "Convert heading TEXT to a GitHub-style anchor slug.
Follows the github-slugger algorithm used by rehype-slug."
  (let ((slug (downcase (string-trim text))))
    ;; Remove inline markup like **bold**, *italic*, `code`, [link](url)
    (setq slug (replace-regexp-in-string "\\*\\*\\|\\*\\|`" "" slug))
    (setq slug (replace-regexp-in-string "\\[\\([^]]*\\)\\]([^)]*)" "\\1" slug))
    ;; Replace spaces and consecutive whitespace with hyphens
    (setq slug (replace-regexp-in-string "[ \t]+" "-" slug))
    ;; Remove characters that aren't alphanumeric, hyphens, or CJK
    (setq slug (replace-regexp-in-string
                "[^[:alnum:]\\-\u3000-\u9fff\uac00-\ud7af\uff00-\uffef]"
                "" slug))
    ;; Collapse multiple hyphens
    (setq slug (replace-regexp-in-string "-\\{2,\\}" "-" slug))
    ;; Trim leading/trailing hyphens
    (setq slug (replace-regexp-in-string "\\`-\\|-\\'" "" slug))
    slug))

(defvar-local mo--heading-cache nil
  "Cache of (POS . SLUG) pairs for all headings, rebuilt on buffer changes.")

(defvar-local mo--heading-cache-tick nil
  "Buffer `buffer-chars-modified-tick' when `mo--heading-cache' was last built.")

(defun mo--rebuild-heading-cache ()
  "Rebuild `mo--heading-cache' if the buffer has been modified since last build."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eql tick mo--heading-cache-tick)
      (setq mo--heading-cache-tick tick)
      (let ((entries nil)
            (slug-counts (make-hash-table :test #'equal)))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^#{1,6}[ \t]+\\(.+\\)" nil t)
            (let* ((pos (match-beginning 0))
                   (base-slug (mo--heading-slug (match-string 1)))
                   (n (gethash base-slug slug-counts 0))
                   (slug (if (> n 0) (format "%s-%d" base-slug n) base-slug)))
              (puthash base-slug (1+ n) slug-counts)
              (push (cons pos slug) entries))))
        (setq mo--heading-cache (nreverse entries))))))

(defun mo--nearest-heading-slug ()
  "Return the deduplicated slug of the nearest heading at or above point."
  (mo--rebuild-heading-cache)
  (let ((result nil)
        (p (line-end-position)))
    (catch 'done
      (dolist (entry mo--heading-cache)
        (if (<= (car entry) p)
            (setq result (cdr entry))
          (throw 'done nil))))
    result))

(defun mo--scroll-sync (&rest _args)
  "Sync the mo preview to the nearest heading in the markdown buffer."
  (when (and mo--xwidget
             (derived-mode-p 'markdown-mode))
    (let ((slug (mo--nearest-heading-slug)))
      (when (and slug (not (equal slug mo--sync-heading)))
        (setq mo--sync-heading slug)
        (xwidget-webkit-execute-script
         mo--xwidget
         (format "(() => {
  const el = document.getElementById(%s);
  if (el) el.scrollIntoView({ behavior: 'smooth', block: 'start' });
})()"
                 (json-serialize slug)))))))

(defun mo--enable-scroll-sync ()
  "Enable scroll sync in the current markdown buffer."
  (add-hook 'post-command-hook #'mo--scroll-sync nil t))

(defun mo--disable-scroll-sync ()
  "Disable scroll sync in the current markdown buffer."
  (remove-hook 'post-command-hook #'mo--scroll-sync t)
  (setq mo--xwidget nil)
  (setq mo--sync-heading nil)
  (setq mo--heading-cache nil)
  (setq mo--heading-cache-tick nil))

(defun mo--setup-window (source-buf)
  "Arrange windows for mo preview.
SOURCE-BUF is the markdown buffer.
If only one window, split horizontally.
If already split, create a new tab-bar tab."
  (unless (one-window-p)
    (let ((tab-name (format "mo:%s"
                            (file-name-nondirectory
                             (or (buffer-file-name) "preview")))))
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name)
      (setq mo--managed-tab tab-name)
      (delete-other-windows)))
  (split-window-right)
  (other-window 1)
  (mo--open-xwidget-preview source-buf))

;;;###autoload
(defun mo-preview ()
  "Preview the current Markdown buffer with mo.
Starts the mo server if needed, sends the file, and opens an
xwidget-webkit preview buffer side by side."
  (interactive)
  (unless (derived-mode-p 'markdown-mode)
    (user-error "Not a Markdown buffer"))
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))
  ;; Save before sending to mo so it picks up the latest content
  (save-buffer)
  (let ((file (buffer-file-name))
        (source-buf (current-buffer)))
    (mo--send-file file)
    (unless (mo--wait-for-server)
      (user-error "mo server did not start within 5 seconds"))
    (mo--setup-window source-buf)
    ;; Return to the source buffer and enable scroll sync
    (select-window (get-buffer-window source-buf))
    (mo--enable-scroll-sync)))

;;;###autoload
(defun mo-shutdown ()
  "Shut down the running mo server and clean up."
  (interactive)
  ;; Disable scroll sync in all markdown buffers
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when mo--xwidget
        (mo--disable-scroll-sync))))
  ;; Shutdown with correct port/target flags
  (apply #'call-process mo-executable nil nil nil (mo--shutdown-args))
  (when (and mo--process (process-live-p mo--process))
    (kill-process mo--process))
  (setq mo--process nil)
  ;; Kill the actual xwidget buffer
  (when (and mo--preview-xwidget-buffer
             (buffer-live-p mo--preview-xwidget-buffer))
    (kill-buffer mo--preview-xwidget-buffer))
  (setq mo--preview-xwidget-buffer nil)
  ;; Close the managed tab if we created one
  (when mo--managed-tab
    (when-let* ((tabs (mapcar (lambda (tab) (alist-get 'name tab))
                              (tab-bar-tabs)))
                (_found (member mo--managed-tab tabs)))
      (tab-bar-close-tab-by-name mo--managed-tab))
    (setq mo--managed-tab nil))
  (message "mo server stopped"))

(provide 'mo)
;;; mo.el ends here
