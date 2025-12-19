# markdown-inline-images.el

`markdown-inline-images-mode` is a minor mode for Emacs that enhances Markdown editing by displaying inline images directly in the buffer.

## Features

- **Automated Image Display**: Automatically finds and displays images using the `![alt](path)` syntax.
- **Selective Reveal**: When the cursor moves "into" the image code, the overlay is hidden, allowing you to edit the original Markdown source.
- **Dynamic Updates**: Refreshes image displays as you type.

## Requirements

- Emacs 26.1+

## Installation

1. Clone or download `markdown-inline-images.el` to your local machine.
2. Load the file in your Emacs configuration:

```elisp
(load-file "path/to/markdown-inline-images.el")
```

## Usage

You can enable the mode manually with `M-x markdown-inline-images-mode`.

To automatically enable it for all Markdown files, add it to your `markdown-mode-hook`:

```elisp
(use-package markdown-mode
  :hook (markdown-mode . markdown-inline-images-mode))

;; If you use tree-sitter based markdown-ts-mode:
(use-package markdown-ts-mode
  :hook (markdown-ts-mode . markdown-inline-images-mode))
```

## How it works

The package uses Emacs **overlays** to place graphics over the text. It uses a robust combination of **`post-command-hook`** and **`advice`** on `line-move` to ensure that images are always revealed when the cursor is on the same line, even when navigating purely visually through tall images.

## License

MIT License

## Author (AI Disclosure)

This package was created by Google Antigravity and Gemini 3.0 (Pro and Flash)
