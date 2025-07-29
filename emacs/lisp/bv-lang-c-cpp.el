;;; bv-lang-c-cpp.el --- C/C++ development configuration  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Ayan Das
;; Author: Ayan Das <bvits@riseup.net>
;; URL: https://github.com/b-vitamins/dotfiles/emacs

;;; Commentary:

;; C/C++ development environment with tree-sitter, ccls LSP, and comprehensive tooling.

;;; Code:

(require 'cc-mode)
(require 'eglot)

;; External variables
(defvar c-basic-offset)
(defvar gdb-many-windows)
(defvar gdb-show-main)
(defvar c-ts-mode-map)
(defvar c++-ts-mode-map)

;; External functions
(declare-function gdb "gdb-mi" (command-line))

;; Optional package loading
(autoload 'clang-format-buffer "clang-format" nil t)
(autoload 'clang-format-region "clang-format" nil t)
(autoload 'cmake-mode "cmake-mode" nil t)
(autoload 'cmakelang-format-buffer "cmakelang" nil t)

;; Remap to tree-sitter modes
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))

;; File associations
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode)) ; Assume .h files are C++
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-ts-mode))

;; C/C++ style settings
(setq-default c-basic-offset 4)
(setq c-default-style '((java-mode . "java")
                       (awk-mode . "awk")
                       (other . "linux")))

;; Configure ccls as the LSP server
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((c-ts-mode c++-ts-mode c-mode c++-mode) . ("ccls"))))

;; C/C++-specific eglot settings
(defun bv-c-cpp-eglot-config ()
  "Configure eglot for C/C++ development."
  (setq-local eglot-workspace-configuration
              '(:ccls (:index (:threads 4
                              :initialBlacklist [])
                      :completion (:include (:suffixWhitelist [".h" ".hpp" ".hh"]))))))

(add-hook 'c-ts-mode-hook #'bv-c-cpp-eglot-config)
(add-hook 'c++-ts-mode-hook #'bv-c-cpp-eglot-config)

;; Compilation settings
(defun bv-c-cpp-setup-compilation ()
  "Setup compilation for C/C++."
  (setq-local compile-command
              (cond
               ((file-exists-p "Makefile") "make -k ")
               ((file-exists-p "CMakeLists.txt") "cmake --build build")
               ((file-exists-p "build.ninja") "ninja -C build")
               ((file-exists-p "meson.build") "meson compile -C build")
               (t (concat (if (eq major-mode 'c++-ts-mode) "g++" "gcc")
                         " -Wall -Wextra -g -o "
                         (file-name-sans-extension buffer-file-name)
                         " " buffer-file-name)))))

(add-hook 'c-ts-mode-hook #'bv-c-cpp-setup-compilation)
(add-hook 'c++-ts-mode-hook #'bv-c-cpp-setup-compilation)

;; Formatting with clang-format
(defun bv-c-cpp-format-buffer ()
  "Format current buffer with `clang-format'."
  (interactive)
  (if (fboundp 'clang-format-buffer)
      (clang-format-buffer)
    (shell-command-on-region (point-min) (point-max)
                            "clang-format" nil t)))

(defun bv-c-cpp-format-on-save ()
  "Format buffer with `clang-format' before saving."
  (when (or (eq major-mode 'c-ts-mode)
            (eq major-mode 'c++-ts-mode))
    (bv-c-cpp-format-buffer)))

;; Optional: Enable format on save
;; (add-hook 'before-save-hook #'bv-c-cpp-format-on-save)

;; Debugging with GDB
(setq gdb-many-windows t)
(setq gdb-show-main t)

(defun bv-c-cpp-debug ()
  "Start debugging with GDB."
  (interactive)
  (let ((executable (file-name-sans-extension buffer-file-name)))
    (if (file-exists-p executable)
        (gdb (format "gdb -i=mi %s" executable))
      (message "Executable not found. Compile first."))))

;; Valgrind integration
(defun bv-c-cpp-valgrind ()
  "Run valgrind on current executable."
  (interactive)
  (let ((executable (file-name-sans-extension buffer-file-name)))
    (compile (format "valgrind --leak-check=full --show-leak-kinds=all %s" executable))))

;; Header/implementation switching
(defun bv-c-cpp-switch-header-impl ()
  "Switch between header and implementation file."
  (interactive)
  (let* ((name (file-name-sans-extension buffer-file-name))
         (ext (file-name-extension buffer-file-name))
         (other-exts (cond
                     ((member ext '("h" "hpp" "hh")) '("c" "cpp" "cc" "cxx"))
                     ((member ext '("c" "cpp" "cc" "cxx")) '("h" "hpp" "hh"))
                     (t nil))))
    (when other-exts
      (cl-loop for other-ext in other-exts
               for other-file = (concat name "." other-ext)
               when (file-exists-p other-file)
               return (find-file other-file)))))

;; Include guards
(defun bv-c-cpp-insert-include-guard ()
  "Insert include guard for header file."
  (interactive)
  (let* ((filename (upcase (file-name-nondirectory buffer-file-name)))
         (guard (replace-regexp-in-string "[.-]" "_" filename)))
    (goto-char (point-min))
    (insert (format "#ifndef %s\n#define %s\n\n" guard guard))
    (goto-char (point-max))
    (insert (format "\n#endif // %s\n" guard))))

;; CMake support
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

(defun bv-c-cpp-cmake-configure ()
  "Configure CMake project."
  (interactive)
  (compile "cmake -B build -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"))

(defun bv-c-cpp-cmake-build ()
  "Build CMake project."
  (interactive)
  (compile "cmake --build build"))

;; Keybindings
(defvar bv-c-cpp-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") #'bv-c-cpp-format-buffer)
    (define-key map (kbd "C-c C-o") #'bv-c-cpp-switch-header-impl)
    (define-key map (kbd "C-c C-g") #'bv-c-cpp-insert-include-guard)
    (define-key map (kbd "C-c C-d") #'bv-c-cpp-debug)
    (define-key map (kbd "C-c C-v") #'bv-c-cpp-valgrind)
    (define-key map (kbd "C-c m c") #'bv-c-cpp-cmake-configure)
    (define-key map (kbd "C-c m b") #'bv-c-cpp-cmake-build)
    map)
  "Keymap for C/C++ mode commands.")

(with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "C-c C-c") bv-c-cpp-mode-map))

(with-eval-after-load 'c-ts-mode
  (define-key c-ts-mode-map (kbd "C-c C-c") bv-c-cpp-mode-map)
  (define-key c++-ts-mode-map (kbd "C-c C-c") bv-c-cpp-mode-map))

(provide 'bv-lang-c-cpp)
;;; bv-lang-c-cpp.el ends here