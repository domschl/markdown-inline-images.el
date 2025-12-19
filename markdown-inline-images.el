;;; markdown-inline-images.el --- Inline images for markdown-mode  -*- lexical-binding: t; -*-

;; Author: Antigravity
;; Version: 0.1.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: markdown, images

;;; Commentary:

;; This package provides a minor mode `markdown-inline-images-mode` that
;; displays images in Markdown buffers as graphics. When the cursor
;; moves over the Markdown code for an image, the code is revealed.

;;; Code:

(defgroup markdown-inline-images nil
  "Inline images for markdown-mode."
  :group 'markdown
  :prefix "markdown-inline-images-")

(defvar-local markdown-inline-images--overlays nil
  "List of overlays managed by `markdown-inline-images-mode'.")

(defun markdown-inline-images--clear-overlays ()
  "Remove all overlays created by this mode."
  (mapc #'delete-overlay markdown-inline-images--overlays)
  (setq markdown-inline-images--overlays nil))

(defun markdown-inline-images--sync-state ()
  "Sync visibility of all images based on current point.
Reveals images on the current line, hides others."
  (when markdown-inline-images-mode
    (dolist (ov markdown-inline-images--overlays)
      (when (overlay-buffer ov) ;; check if overlay is still valid
        (let* ((start (overlay-start ov))
               (end (overlay-end ov))
               ;; Determine logical line range dynamically
               ;; We include the newline in the check so cursor at end of line (EOL) reveals it too.
               (line-start (save-excursion (goto-char start) (line-beginning-position)))
               (line-end (save-excursion (goto-char end) (1+ (line-end-position)))))
          (if (and (>= (point) line-start) (< (point) line-end))
              (overlay-put ov 'display nil) ;; Reveal source
            (overlay-put ov 'display (overlay-get ov 'markdown-image-data)))))))) ;; Show image

(defun markdown-inline-images--pre-line-move (arg &rest _args)
  "Advice to reveal images on target line before `line-move' happens.
This prevents visual-line navigation from getting stuck exploring tall images."
  (when markdown-inline-images-mode
    ;; 'arg' is the number of lines to move, default 1.
    (let ((n (or arg 1)))
      (save-excursion
        ;; We attempt to move purely specifically to find the target line.
        ;; Usage of forward-line (logical lines) moves past visuals instantly.
        (when (zerop (forward-line n))
          ;; We are now on the target line. Reveal known overlays here.
          (let ((target-line-start (line-beginning-position))
                (target-line-end (line-end-position)))
             (dolist (ov markdown-inline-images--overlays)
                (when (and (overlay-buffer ov)
                           (<= (overlay-start ov) target-line-end)
                           (>= (overlay-end ov) target-line-start))
                  ;; Temporarily hide the image display so line-move lands on text.
                  ;; The sync-state hook will confirm this state or revert it if we missed.
                  (overlay-put ov 'display nil)))))))))

(defun markdown-inline-images--refresh (&optional _beg _end _len)
  "Scan the buffer for markdown images and create overlays."
  (interactive)
  (when markdown-inline-images-mode
    (save-excursion
      (save-restriction
        (widen)
        (markdown-inline-images--clear-overlays)
        (goto-char (point-min))
        ;; Regex for ![alt](path)
        (while (re-search-forward "!\\[\\([^]]*\\)\\](\\([^)]+\\))" nil t)
          (let* ((start (match-beginning 0))
                 (end (match-end 0))
                 (path (match-string 2))
                 (full-path (expand-file-name path))
                 (img (ignore-errors
                        (when (file-exists-p full-path)
                          (create-image full-path nil nil :max-width 500)))))
            (when img
              (let ((ov (make-overlay start end)))
                (overlay-put ov 'markdown-inline-image t)
                (overlay-put ov 'markdown-image-data img)
                (overlay-put ov 'display img)
                (push ov markdown-inline-images--overlays)))))))))

;;;###autoload
(define-minor-mode markdown-inline-images-mode
  "Toggle display of inline images in Markdown."
  :lighter " Img"
  (if markdown-inline-images-mode
      (progn
        (add-hook 'post-command-hook #'markdown-inline-images--sync-state nil t)
        (add-hook 'after-change-functions #'markdown-inline-images--refresh nil t)
        (advice-add 'line-move :before #'markdown-inline-images--pre-line-move)
        (markdown-inline-images--refresh))
    (progn
      (remove-hook 'post-command-hook #'markdown-inline-images--sync-state t)
      (remove-hook 'after-change-functions #'markdown-inline-images--refresh t)
      (advice-remove 'line-move #'markdown-inline-images--pre-line-move)
      (markdown-inline-images--clear-overlays))))

(provide 'markdown-inline-images)

;;; markdown-inline-images.el ends here
