(defvar taskpaper-mode-map nil "Keymap for taskpaper-mode")
(when (not taskpaper-mode-map)
  (setq taskpaper-mode-map (make-sparse-keymap))
  (define-key taskpaper-mode-map (kbd "<S-return>") 'taskpaper-focus-project)
  (define-key taskpaper-mode-map (kbd "<S-backspace>") 'taskpaper-unfocus-project)
  (define-key taskpaper-mode-map (kbd "C-c C-d") 'taskpaper-toggle-done)
  )

(setq tpKeywords
 '(
   (".*@done.*" . font-lock-comment-face)
   (".*:$" . font-lock-function-name-face)
   ("^ *[^- ].*[^:]$" . font-lock-comment-face)
   ("@.*" . font-lock-variable-name-face)

  )
)

(defun taskpaper-focus-project()
  "Hide everything not related to current project."
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
    (re-search-forward "@done")
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
    (goto-char startpoint)
    )
  )


(defun taskpaper-mode ()
  "Major mode for editing taskpaper styled files."
  (interactive)
  (kill-all-local-variables)

  (setq major-mode 'taskpaper-mode)
  (setq mode-name "Taskpaper") ; for display purposes in mode line
  (use-local-map taskpaper-mode-map)
  
  ;(copy-face 'font-lock-comment-face 'font-lock-taskpaper-done-face)
  ;(set-face-attribute 'font-lock-taskpaper-done-face nil :strike-through t)

  (setq font-lock-defaults '(tpKeywords))
  
  ;; Dont wrap lines
  (toggle-truncate-lines t)
  
  ;; ... other code here

  (run-hooks 'taskpaper-mode-hook))

(provide 'taskpaper-mode)