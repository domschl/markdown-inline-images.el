;;; markdown-inline-images.el --- Inline images for markdown-mode  -*- lexical-binding: t; -*-

;; Author: Antigravity
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: markdown, images

;;; Commentary:

;; This package provides a minor mode `markdown-inline-images-mode` that
;; displays images in Markdown buffers as graphics. When the cursor
;; moves over the Markdown code for an image, the code is revealed.

;;; Code:

(require 'cursor-sensor)

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

(defun markdown-inline-images--update-overlay (ov state)
  "Update the display property of OV based on cursor sensor STATE."
  (let ((img (overlay-get ov 'markdown-image-data)))
    (overlay-put ov 'display (if (eq state 'entered) nil img))))

(defun markdown-inline-images--refresh (&optional _beg _end _len)
  "Scan the buffer for markdown images and create overlays.
   Optional arguments BEG, END, and LEN are ignored (for hook compatibility)."
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
              (let* ((ov (make-overlay start end))
                     ;; Extend sensor range by 1 to catch cursor at end-of-line
                     (sensor-end (min (point-max) (1+ end)))
                     (sov (make-overlay start sensor-end)))
                (overlay-put ov 'markdown-inline-image t)
                (overlay-put ov 'markdown-image-data img)
                (overlay-put ov 'display img)
                ;; we use cursor-sensor-functions to toggle the display
                (overlay-put sov 'cursor-sensor-functions
                             (list (lambda (_window _prev-pos state)
                                     (markdown-inline-images--update-overlay ov state))))
                (push ov markdown-inline-images--overlays)
                (push sov markdown-inline-images--overlays)))))))))

;;;###autoload
(define-minor-mode markdown-inline-images-mode
  "Toggle display of inline images in Markdown."
  :lighter " Img"
  (if markdown-inline-images-mode
      (progn
        (cursor-sensor-mode 1)
        (add-hook 'after-change-functions #'markdown-inline-images--refresh nil t)
        (markdown-inline-images--refresh))
    (progn
      (cursor-sensor-mode -1)
      (remove-hook 'after-change-functions #'markdown-inline-images--refresh t)
      (markdown-inline-images--clear-overlays))))

(provide 'markdown-inline-images)

;;; markdown-inline-images.el ends here
