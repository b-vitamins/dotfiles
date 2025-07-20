;;; bv-light-theme.el --- BV light theme -*- lexical-binding: t -*-

(require 'bv-themes)

(defconst bv-light-palette
  '((foreground . "#3a3a3a")  ; Charcoal
    (background . "#ffffff")  ; Pure white
    (strong     . "#262626")  ; Dark charcoal
    (faded      . "#767676")  ; Medium gray
    (subtle     . "#eeeeee")  ; Light gray
    (highlight  . "#f7f7f7")  ; Off-white
    (salient    . "#5f87d7")  ; Cornflower blue
    (popout     . "#87afaf")  ; Sage green
    (critical   . "#d7875f") ; Terracotta
    (mode-line-fg . background)) ; Mode-line text color
  "Color palette for BV light theme.")

(bv-themes-theme bv-light bv-light-palette)

(provide 'bv-light-theme)
;;; bv-light-theme.el ends here