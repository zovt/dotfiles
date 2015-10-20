;;; keybindings.el --- my keybinds
;;; Commentary:
;;; Code:

(defhydra hydra-code (global-map "C-c c")
  "Code"
  ("=" indent-whole-buffer)
  ("i" imenu))

(defhydra hydra-swoop (global-map "C-c s")
  "Swoop"
  ("s" helm-swoop)
  ("b" helm-swoop-back-to-last-point)
  ("S" helm-multi-swoop))

(defhydra hydra-editing (global-map "C-c e")
  "Editing"
  ("r" align-regexp)
  ("u" undo-tree-visualize))

(defhydra hydra-git (global-map "C-c g")
  "Git"
  ("s" magit-status)
  ("c" magit-commit)
  ("C" magit-amend)
  ("P" magit-push)
  ("f" magit-fetch)
  ("F" magit-pull)
  ("i" magit-init)
  ("d" magit-diff-working-tree))

(defhydra hydra-files (global-map "C-c f")
  "Files"
  ("f" find-file))

(defhydra hydra-buffers (global-map "C-c b")
  "Buffers"
  ("b" switch-to-buffer)
  ("C-k" desktop-clear))

(defhydra hydra-windows (global-map "C-c w")
  "Windows"
  ("J" other-window)
  ("K" other-window -1)
  ("d" delete-window)
  ("D" delete-other-windows)
  ("v" split-window-vertically)
  ("h" split-window-horizontally))

(defhydra hydra-projectile (global-map "C-c p")
  "
Projectile
^Files^                       ^Project^                   ^Other^
^-------------------------------------------------------------------------------
_f_: find file                _r_: replace in project     _!_: run shell command
_F_: find file in known       _k_: kill project buffers       ^in project root
^    projects                 _i_: invalidate project     _&_: same as ! but
_T_: find test files              ^cache                      ^async
_t_: switch between test and  _S_: save all project       _c_: compile project
^    implementation                buffers
_d_: find project dir         _b_: switch project
_D_: open project root in         ^buffers
^    Dired
_l_: find file in directory
^    (not project)
_a_: find matching file
_e_: recent files
"
  ("f" helm-projectile-find-file)
  ("F" helm-projectile-find-file-in-known-projects)
  ("T" projectile-find-test-file)
  ("d" helm-projectile-find-dir)
  ("l" projectile-find-file-in-directory)
  ("a" projectile-find-other-file)
  ("r" projectile-replace)
  ("k" projectile-kill-buffers)
  ("i" projectile-invalidate-cache)
  ("D" projectile-dired)
  ("e" helm-projectile-recentf)
  ("!" projectile-run-shell-command-in-root)
  ("&" projectile-run-async-shell-command-in-root)
  ("t" projectile-toggle-between-implementation-and-test)
  ("S" projectile-save-project-buffers)
  ("b" helm-projectile-switch-to-buffer)
  ("c" projectile-compile-project))
(define-key projectile-mode-map (kbd "C-c p") 'hydra-projectile/body)


;;; Go-specific keybinds
(defhydra hydra-go ()
  ("i" go-goto-imports)
  ("I" go-remove-unused-imports)
  ("d" godoc)
  ("f" gofmt))
(define-key go-mode-map (kbd "C-c G") 'hydra-go/body)

;;; Origami Mode
(defhydra hydra-origami (origami-mode-map "C-c TAB")
  ("TAB" origami-recursively-toggle-node)
  ("O" origami-open-node-recursively)
  ("c" origami-close-node)
  ("C" origami-close-node-recursively)
  ("t" origami-open-node)
  ("G" origami-toggle-all-nodes)
  ("f" origami-show-only-node)
  ("r" origami-reset))

;; Special non-hydra binds
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-c x") 'smex)

(provide 'keybindings)
;;; keybindings.el ends here
