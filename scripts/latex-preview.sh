#!/bin/sh

# Directory containing your .org files
ORG_DIR="/home/b/slipbox/notes"

# Use xvfb-run to execute Emacs in a virtual framebuffer environment
xvfb-run -a emacs --eval "
(progn
  (dolist (file (directory-files \"$ORG_DIR\" t \"\\\\.org$\"))
    (with-current-buffer (find-file-noselect file)
      (message \"Generating LaTeX previews for %s\" file)
      (org-mode)
      (org--latex-preview-region (point-min) (point-max))
      (save-buffer)
      (kill-buffer))))
"

echo "LaTeX previews generation completed."
