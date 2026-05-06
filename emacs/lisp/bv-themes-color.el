;;; bv-themes-color.el --- Perceptual color math for BV themes -*- lexical-binding: t -*-

;; Copyright (C) 2026 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>

;;; Commentary:

;; This module is the color-science core for the BV theme compiler.  All
;; ramps, blends, contrast checks, and simulation gates use explicit numeric
;; transforms instead of legacy RGB tweaking.

;;; Code:

(require 'cl-lib)

(defconst bv-themes-color--epsilon 0.000001
  "Small epsilon used by color calculations.")

(defun bv-themes-color--clamp (value &optional min max)
  "Clamp VALUE between MIN and MAX."
  (min (or max 1.0) (max (or min 0.0) value)))

(defun bv-themes-color--cbrt (value)
  "Return the real cube root of VALUE."
  (if (< value 0)
      (- (expt (- value) (/ 1.0 3.0)))
    (expt value (/ 1.0 3.0))))

(defun bv-themes-color--normalize-hex (color)
  "Return COLOR as a lowercase #rrggbb string."
  (unless (and (stringp color)
               (string-match-p "\\`#[[:xdigit:]]+\\'" color))
    (error "Invalid hex color: %S" color))
  (let ((hex (downcase (substring color 1))))
    (pcase (length hex)
      (3 (format "#%c%c%c%c%c%c"
                 (aref hex 0) (aref hex 0)
                 (aref hex 1) (aref hex 1)
                 (aref hex 2) (aref hex 2)))
      (6 (concat "#" hex))
      (8 (concat "#" (substring hex 0 6)))
      (12 (format "#%s%s%s"
                  (substring hex 0 2)
                  (substring hex 4 6)
                  (substring hex 8 10)))
      (_ (error "Unsupported hex color length: %S" color)))))

(defun bv-themes-color-hex-to-rgb (color)
  "Return COLOR as a list of sRGB channels in the range 0..1."
  (let ((hex (substring (bv-themes-color--normalize-hex color) 1)))
    (list (/ (string-to-number (substring hex 0 2) 16) 255.0)
          (/ (string-to-number (substring hex 2 4) 16) 255.0)
          (/ (string-to-number (substring hex 4 6) 16) 255.0))))

(defun bv-themes-color-rgb-to-hex (rgb)
  "Return RGB channels in the range 0..1 as #rrggbb."
  (cl-destructuring-bind (r g b) rgb
    (format "#%02x%02x%02x"
            (round (* 255 (bv-themes-color--clamp r)))
            (round (* 255 (bv-themes-color--clamp g)))
            (round (* 255 (bv-themes-color--clamp b))))))

(defun bv-themes-color--srgb-to-linear (channel)
  "Convert sRGB CHANNEL to linear light."
  (if (<= channel 0.04045)
      (/ channel 12.92)
    (expt (/ (+ channel 0.055) 1.055) 2.4)))

(defun bv-themes-color--linear-to-srgb (channel)
  "Convert linear-light CHANNEL to sRGB."
  (if (<= channel 0.0031308)
      (* 12.92 channel)
    (- (* 1.055 (expt channel (/ 1.0 2.4))) 0.055)))

(defun bv-themes-color--rgb-to-linear (rgb)
  "Convert RGB from sRGB to linear light."
  (mapcar #'bv-themes-color--srgb-to-linear rgb))

(defun bv-themes-color--linear-to-rgb (rgb)
  "Convert RGB from linear light to sRGB."
  (mapcar #'bv-themes-color--linear-to-srgb rgb))

(defun bv-themes-color--in-gamut-p (rgb)
  "Return non-nil if RGB is inside the displayable sRGB gamut."
  (cl-every (lambda (channel)
              (and (<= (- bv-themes-color--epsilon) channel)
                   (<= channel (+ 1 bv-themes-color--epsilon))))
            rgb))

(defun bv-themes-color-rgb-to-oklab (rgb)
  "Convert sRGB RGB channels in the range 0..1 to Oklab."
  (cl-destructuring-bind (r g b) (bv-themes-color--rgb-to-linear rgb)
    (let* ((l (+ (* 0.4122214708 r) (* 0.5363325363 g) (* 0.0514459929 b)))
           (m (+ (* 0.2119034982 r) (* 0.6806995451 g) (* 0.1073969566 b)))
           (s (+ (* 0.0883024619 r) (* 0.2817188376 g) (* 0.6299787005 b)))
           (l-root (bv-themes-color--cbrt l))
           (m-root (bv-themes-color--cbrt m))
           (s-root (bv-themes-color--cbrt s)))
      (list (+ (* 0.2104542553 l-root)
               (* 0.7936177850 m-root)
               (* -0.0040720468 s-root))
            (+ (* 1.9779984951 l-root)
               (* -2.4285922050 m-root)
               (* 0.4505937099 s-root))
            (+ (* 0.0259040371 l-root)
               (* 0.7827717662 m-root)
               (* -0.8086757660 s-root))))))

(defun bv-themes-color-oklab-to-rgb (oklab)
  "Convert OKLAB coordinates to sRGB channels in the range 0..1."
  (cl-destructuring-bind (l a b) oklab
    (let* ((l-root (+ l (* 0.3963377774 a) (* 0.2158037573 b)))
           (m-root (+ l (* -0.1055613458 a) (* -0.0638541728 b)))
           (s-root (+ l (* -0.0894841775 a) (* -1.2914855480 b)))
           (l-cube (* l-root l-root l-root))
           (m-cube (* m-root m-root m-root))
           (s-cube (* s-root s-root s-root)))
      (bv-themes-color--linear-to-rgb
       (list (+ (* 4.0767416621 l-cube)
                (* -3.3077115913 m-cube)
                (* 0.2309699292 s-cube))
             (+ (* -1.2684380046 l-cube)
                (* 2.6097574011 m-cube)
                (* -0.3413193965 s-cube))
             (+ (* -0.0041960863 l-cube)
                (* -0.7034186147 m-cube)
                (* 1.7076147010 s-cube)))))))

(defun bv-themes-color-oklab-to-oklch (oklab)
  "Convert OKLAB to OKLCH."
  (cl-destructuring-bind (l a b) oklab
    (let ((chroma (sqrt (+ (* a a) (* b b)))))
      (list l
            chroma
            (if (< chroma bv-themes-color--epsilon)
                0.0
              (mod (* 180.0 (/ (atan b a) float-pi)) 360.0))))))

(defun bv-themes-color-oklch-to-oklab (oklch)
  "Convert OKLCH to OKLAB."
  (cl-destructuring-bind (l chroma hue) oklch
    (let ((radians (* float-pi (/ (or hue 0.0) 180.0))))
      (list l
            (* chroma (cos radians))
            (* chroma (sin radians))))))

(defun bv-themes-color-hex-to-oklch (color)
  "Convert COLOR to OKLCH."
  (bv-themes-color-oklab-to-oklch
   (bv-themes-color-rgb-to-oklab
    (bv-themes-color-hex-to-rgb color))))

(defun bv-themes-color--oklch-in-gamut-p (oklch)
  "Return non-nil if OKLCH is displayable in sRGB."
  (bv-themes-color--in-gamut-p
   (bv-themes-color-oklab-to-rgb
    (bv-themes-color-oklch-to-oklab oklch))))

(defun bv-themes-color--gamut-map-oklch (oklch)
  "Map OKLCH into sRGB by preserving lightness and hue."
  (cl-destructuring-bind (l chroma hue) oklch
    (cond
     ((bv-themes-color--oklch-in-gamut-p oklch) oklch)
     ((<= chroma bv-themes-color--epsilon) (list l 0.0 hue))
     (t
      (let ((lo 0.0)
            (hi chroma)
            best)
        (dotimes (_ 24)
          (let* ((mid (/ (+ lo hi) 2.0))
                 (candidate (list l mid hue)))
            (if (bv-themes-color--oklch-in-gamut-p candidate)
                (setq lo mid
                      best candidate)
              (setq hi mid))))
        (or best (list l 0.0 hue)))))))

(defun bv-themes-color-oklch-to-hex (l chroma hue)
  "Return the displayable hex color for OKLCH L CHROMA HUE."
  (bv-themes-color-rgb-to-hex
   (bv-themes-color-oklab-to-rgb
    (bv-themes-color-oklch-to-oklab
     (bv-themes-color--gamut-map-oklch
      (list (bv-themes-color--clamp l 0.0 1.0)
            (max 0.0 chroma)
            (mod (or hue 0.0) 360.0)))))))

(defun bv-themes-color-oklch (l chroma hue)
  "Return a displayable hex color from OKLCH L CHROMA HUE."
  (bv-themes-color-oklch-to-hex l chroma hue))

(defun bv-themes-color-adjust-oklch (color &rest plist)
  "Adjust COLOR in OKLCH according to PLIST.
Supported keys are `:lightness', `:chroma', `:hue',
`:lightness-scale', and `:chroma-scale'."
  (cl-destructuring-bind (l chroma hue)
      (bv-themes-color-hex-to-oklch color)
    (bv-themes-color-oklch
     (+ l (or (plist-get plist :lightness) 0.0))
     (* (+ chroma (or (plist-get plist :chroma) 0.0))
        (or (plist-get plist :chroma-scale) 1.0))
     (+ hue (or (plist-get plist :hue) 0.0)))))

(defun bv-themes-color-mix-oklab (color-a color-b amount)
  "Mix COLOR-A into COLOR-B by AMOUNT in Oklab.
AMOUNT is in the range 0..1.  Zero returns COLOR-B; one returns COLOR-A."
  (let ((amount (bv-themes-color--clamp amount)))
    (cl-mapcar (lambda (a b)
                 (+ (* amount a) (* (- 1.0 amount) b)))
               (bv-themes-color-rgb-to-oklab
                (bv-themes-color-hex-to-rgb color-a))
               (bv-themes-color-rgb-to-oklab
                (bv-themes-color-hex-to-rgb color-b)))))

(defun bv-themes-color-mix (color-a color-b amount)
  "Return COLOR-A mixed into COLOR-B by AMOUNT in Oklab."
  (bv-themes-color-rgb-to-hex
   (bv-themes-color-oklab-to-rgb
    (bv-themes-color-mix-oklab color-a color-b amount))))

(defun bv-themes-color-ramp (color background steps &optional start end)
  "Return STEPS colors from COLOR mixed with BACKGROUND.
START and END specify the first and last mix amounts."
  (let ((start (or start 0.18))
        (end (or end 1.0)))
    (cl-loop for index from 0 below steps
             for amount = (if (= steps 1)
                              end
                            (+ start (* (/ (float index) (1- steps))
                                        (- end start))))
             collect (bv-themes-color-mix color background amount))))

(defun bv-themes-color-relative-luminance (color)
  "Return WCAG relative luminance for COLOR."
  (cl-destructuring-bind (r g b)
      (bv-themes-color--rgb-to-linear
       (bv-themes-color-hex-to-rgb color))
    (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b))))

(defun bv-themes-color-contrast-ratio (color-a color-b)
  "Return WCAG contrast ratio between COLOR-A and COLOR-B."
  (let ((l1 (bv-themes-color-relative-luminance color-a))
        (l2 (bv-themes-color-relative-luminance color-b)))
    (when (< l1 l2)
      (cl-rotatef l1 l2))
    (/ (+ l1 0.05) (+ l2 0.05))))

(defun bv-themes-color-oklab-distance (color-a color-b)
  "Return Euclidean Oklab distance between COLOR-A and COLOR-B."
  (cl-destructuring-bind (l1 a1 b1)
      (bv-themes-color-rgb-to-oklab
       (bv-themes-color-hex-to-rgb color-a))
    (cl-destructuring-bind (l2 a2 b2)
        (bv-themes-color-rgb-to-oklab
         (bv-themes-color-hex-to-rgb color-b))
      (sqrt (+ (expt (- l1 l2) 2)
               (expt (- a1 a2) 2)
               (expt (- b1 b2) 2))))))

(defun bv-themes-color--matrix-transform (rgb matrix)
  "Apply MATRIX to RGB."
  (cl-destructuring-bind (r g b) rgb
    (mapcar (lambda (row)
              (bv-themes-color--clamp
               (+ (* (nth 0 row) r)
                  (* (nth 1 row) g)
                  (* (nth 2 row) b))))
            matrix)))

(defun bv-themes-color-simulate (color mode)
  "Return COLOR simulated for color vision MODE.
MODE can be `grayscale', `protanopia', `deuteranopia', or `tritanopia'."
  (let ((rgb (bv-themes-color-hex-to-rgb color)))
    (pcase mode
      ('grayscale
       (let ((luma (bv-themes-color-relative-luminance color)))
         (bv-themes-color-rgb-to-hex (list luma luma luma))))
      ('protanopia
       (bv-themes-color-rgb-to-hex
        (bv-themes-color--matrix-transform
         rgb
         '((0.56667 0.43333 0.0)
           (0.55833 0.44167 0.0)
           (0.0 0.24167 0.75833)))))
      ('deuteranopia
       (bv-themes-color-rgb-to-hex
        (bv-themes-color--matrix-transform
         rgb
         '((0.625 0.375 0.0)
           (0.7 0.3 0.0)
           (0.0 0.3 0.7)))))
      ('tritanopia
       (bv-themes-color-rgb-to-hex
        (bv-themes-color--matrix-transform
         rgb
         '((0.95 0.05 0.0)
           (0.0 0.43333 0.56667)
           (0.0 0.475 0.525)))))
      (_ color))))

(provide 'bv-themes-color)
;;; bv-themes-color.el ends here
