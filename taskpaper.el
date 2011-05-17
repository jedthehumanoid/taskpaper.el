(defvar taskpaper-mode-map nil "Keymap for taskpaper-mode")
(when (not taskpaper-mode-map)
  (setq taskpaper-mode-map (make-sparse-keymap))
  (define-key taskpaper-mode-map (kbd "<S-return>") 'taskpaper-focus-selected-project)
  (define-key taskpaper-mode-map (kbd "<S-backspace>") 'taskpaper-unfocus-project)
  (define-key taskpaper-mode-map (kbd "C-c d") 'taskpaper-toggle-done)
  (define-key taskpaper-mode-map (kbd "C-c l") 'taskpaper-chose-project)
  )

(setq tpKeywords
      '(
        (".*@done.*" . font-lock-comment-face)
        (".*:$" . font-lock-function-name-face)
        ("^ *[^- ].*[^:]$" . font-lock-comment-face)
        ("@.*" . font-lock-variable-name-face)
        
        )
      )

(defun taskpaper-functions()

  (defun taskpaper-chose-project()
    "Show a list of projects to chose from."
    (interactive)
    (let ((projects '()) (startpoint (point)) (x 0) (buffer (concat (buffer-name) ": Projects")))
      (goto-char 0)
      ;;Gather list of projects
      (while (re-search-forward ":$" nil t)
        (back-to-indentation)
        (setq projects (append projects (list (buffer-substring-no-properties (point) (- (line-end-position) 1)))))
        (end-of-line)
        )
      (goto-char startpoint)

      (when (get-buffer buffer) 
        (kill-buffer buffer))
      ;;Open popupwindow for selecting project
      (split-window-vertically (- (+ (length projects) 1)))
      (other-window 1)
      (get-buffer-create buffer)
      (switch-to-buffer buffer)
      (while (< x (length projects))
        (insert (elt projects x))
        (insert "\n")
        (setq x (+ x 1))
        )
      (goto-char 0)
      )
    (toggle-read-only t)
    (local-set-key (kbd "<return>") 'taskpaper-projectwindow-select)
    (local-set-key (kbd "<ESC> <ESC>") 'taskpaper-projectwindow-esc)  
    )

  (defun taskpaper-projectwindow-esc()
    "Exit projectwindow, not selecting a project"
    (interactive)
    (local-unset-key (kbd "<return"))
    (local-unset-key (kbd "<ESC> <ESC>"))
    (kill-buffer)
    (delete-window)
    (other-window (- 1))
    )

  (defun taskpaper-projectwindow-select()
    "Action to perform when project is selected in project window."
    (interactive)
    (local-unset-key (kbd "<return>"))
    (let ((buffer (substring (buffer-name) 0 -10)) project)
      (message "substring: %s" buffer)
      (setq project (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
      (kill-buffer)
      (delete-window)
      (other-window (- 1))
      (switch-to-buffer buffer)

      (goto-char 0)
      (re-search-forward project)
      (taskpaper-unfocus-project)
      (taskpaper-focus-selected-project)
      
      )
    )

  (defun taskpaper-focus-selected-project()
    "Hide everything not related to project under cursor."
    (interactive)
    (let ((startpoint (point)) start end)
      (end-of-line)
      (re-search-backward ":$")
      (beginning-of-line)
      (setq start (point))
      (re-search-forward ":$")
      (re-search-forward ":$")
      (beginning-of-line)
      (setq end (point))
      (add-text-properties 1 start '(invisible t))
      (add-text-properties end (point-max) '(invisible t))
      (goto-char startpoint)
      )
    )

  (defun taskpaper-unfocus-project()
    "Show all projects if focused on one."
    (interactive)
    (add-text-properties 1 (point-max) '(invisible nil))
    )

  (defun taskpaper-toggle-done()
    "Toggle done status on task, this sets @done-tag with date."
    (interactive)
    (let ((startpoint (point)) (line (line-number-at-pos)))
      (back-to-indentation)
      (re-search-forward "@done" nil 2)
      (if (= line (line-number-at-pos))
          (progn
            (end-of-line)
            (re-search-backward "@")
            (backward-char)
            (kill-line)
            )
        (progn
          (goto-char startpoint)
          (end-of-line)
          (insert " @done")
          )
        )
      (when (not (equal (point) (line-end-position)))
        (goto-char startpoint))
      )
    )
  )

(defun taskpaper-mode ()
  "Major mode for editing taskpaper styled files."
  (interactive)
  (kill-all-local-variables)

  (setq major-mode 'taskpaper-mode)
  (setq mode-name "Taskpaper") ; for display purposes in mode line
  (use-local-map taskpaper-mode-map)
  
  (taskpaper-functions)
  (setq font-lock-defaults '(tpKeywords))
  
  ;; Dont wrap lines
  (toggle-truncate-lines t)
  
  ;; ... other code here

  (run-hooks 'taskpaper-mode-hook))

(provide 'taskpaper-mode)