;;; bv-dark-theme.el --- BV dark theme -*- lexical-binding: t -*-

(require 'bv-themes)

(defconst bv-dark-palette
  '((foreground . "#dadada")  ; Platinum
    (background . "#1c1c1c")  ; Eerie black
    (strong     . "#eeeeee")  ; White smoke
    (faded      . "#6c6c6c")  ; Dim gray
    (subtle     . "#303030")  ; Jet (for backgrounds)
    (highlight  . "#303030")  ; Jet (for highlights)
    (salient    . "#5f87d7")  ; Cornflower blue
    (popout     . "#afaf87")  ; Olive
    (critical   . "#d7875f") ; Terracotta
    (mode-line-fg . foreground)) ; Mode-line text color
  "Color palette for BV dark theme.")

(bv-themes-theme bv-dark bv-dark-palette)

(provide 'bv-dark-theme)
;;; bv-dark-theme.el ends here