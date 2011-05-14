(defvar taskpaper-mode-map nil "Keymap for taskpaper-mode")
(when (not taskpaper-mode-map)
  (setq taskpaper-mode-map (make-sparse-keymap))
  (define-key taskpaper-mode-map (kbd "<S-return>") 'taskpaper-focus-project)
  )

(setq tpKeywords
 '(
   (".*@done.*" . font-lock-comment-face)
   (".*:$" . font-lock-function-name-face)
   ("^ *[^- ].*[^:]$" . font-lock-comment-face)
   ("@.*" . font-lock-variable-name-face)

  )
)
(defun taskpaper-mode ()
  "Major mode for editing taskpaper styled files."
  (interactive)
  (kill-all-local-variables)

  (setq major-mode 'taskpaper-mode)
  (setq mode-name "Taskpaper") ; for display purposes in mode line
  (use-local-map taskpaper-mode-map)

  (setq font-lock-defaults '(tpKeywords))
  

  (toggle-truncate-lines t)
  
  ;; ... other code here

  (run-hooks 'taskpaper-mode-hook))

(provide 'taskpaper-mode)