;;; bv-lang-systems.el --- C/C++ systems programming -*- lexical-binding: t -*-

;;; Commentary:
;; Modern C/C++ development with eglot, clangd, CMake, and debugging support.

;;; Code:

;;;; Dependencies
(require 'bv-core)
(require 'bv-development)

;;;; Custom Variables
(defgroup bv-systems nil
  "C/C++ systems programming configuration."
  :group 'bv-languages)

(defcustom bv-systems-c-style "linux"
  "C/C++ coding style."
  :type '(choice (const "linux")
                 (const "gnu")
                 (const "k&r")
                 (const "bsd")
                 (const "stroustrup")
                 (const "ellemtel")
                 (const "cc-mode"))
  :group 'bv-systems)

(defcustom bv-systems-clang-format-on-save t
  "Run clang-format on save."
  :type 'boolean
  :group 'bv-systems)

(defcustom bv-systems-clangd-args
  '("--header-insertion=never"
    "--completion-style=detailed"
    "--clang-tidy"
    "--suggest-missing-includes")
  "Arguments to pass to clangd."
  :type '(repeat string)
  :group 'bv-systems)

(defcustom bv-systems-cmake-build-type "Debug"
  "Default CMake build type."
  :type '(choice (const "Debug")
                 (const "Release")
                 (const "RelWithDebInfo")
                 (const "MinSizeRel"))
  :group 'bv-systems)

(defcustom bv-systems-default-c-standard "c17"
  "Default C standard."
  :type '(choice (const "c89")
                 (const "c99")
                 (const "c11")
                 (const "c17")
                 (const "c23"))
  :group 'bv-systems)

(defcustom bv-systems-default-cpp-standard "c++20"
  "Default C++ standard."
  :type '(choice (const "c++98")
                 (const "c++11")
                 (const "c++14")
                 (const "c++17")
                 (const "c++20")
                 (const "c++23"))
  :group 'bv-systems)

;;;; Project Detection
(defvar bv-systems-project-indicators
  '("CMakeLists.txt"
    "meson.build"
    "Makefile"
    "compile_commands.json"
    ".clangd"
    ".clang-format"
    ".clang-tidy"
    "configure.ac"
    "BUILD.bazel")
  "Files indicating a C/C++ project root.")

(defun bv-systems-find-project-root ()
  "Find C/C++ project root."
  (locate-dominating-file
   default-directory
   (lambda (dir)
     (cl-some (lambda (indicator)
                (file-exists-p (expand-file-name indicator dir)))
              bv-systems-project-indicators))))

;;;; C/C++ Mode
(use-package cc-mode
  :ensure nil
  :mode (("\\.c\\'" . c-mode)
         ("\\.cc\\'" . c++-mode)
         ("\\.cpp\\'" . c++-mode)
         ("\\.cxx\\'" . c++-mode)
         ("\\.c\\+\\+\\'" . c++-mode)
         ("\\.h\\'" . c++-mode)
         ("\\.hpp\\'" . c++-mode)
         ("\\.hxx\\'" . c++-mode)
         ("\\.h\\+\\+\\'" . c++-mode)
         ("\\.ipp\\'" . c++-mode)
         ("\\.tpp\\'" . c++-mode))
  :hook ((c-mode . bv-systems-setup)
         (c++-mode . bv-systems-setup))
  :custom
  (c-basic-offset 4)
  :config
  ;; Modern C++ syntax support
  (c-add-style "modern-cpp"
               '("stroustrup"
                 (c-basic-offset . 4)
                 (c-offsets-alist
                  (innamespace . 0)
                  (inline-open . 0)
                  (inlambda . 0)
                  (access-label . -)
                  (case-label . +)
                  (statement-case-intro . +))))
  
  (defun bv-systems-setup ()
    "Setup C/C++ mode."
    (c-set-style (if (string= bv-systems-c-style "modern-cpp")
                     "modern-cpp"
                   bv-systems-c-style))
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local indent-tabs-mode nil)
    (setq-local show-trailing-whitespace t)
    
    ;; Enable modern C++ font-lock
    (when (derived-mode-p 'c++-mode)
      (setq-local c++-font-lock-extra-types
                  '("std::string" "std::vector" "std::map" "std::unique_ptr"
                    "std::shared_ptr" "std::optional" "std::variant"
                    "std::span" "std::string_view" "size_t" "ssize_t"
                    "uint8_t" "uint16_t" "uint32_t" "uint64_t"
                    "int8_t" "int16_t" "int32_t" "int64_t")))
    
    ;; Clang-format on save
    (when (and bv-systems-clang-format-on-save
               (executable-find "clang-format"))
      (add-hook 'before-save-hook 'bv-systems-clang-format-buffer nil t))))

;;;; Clang Format
(use-package clang-format
  :defer t
  :config
  (defun bv-systems-clang-format-buffer ()
    "Format buffer with clang-format if .clang-format exists."
    (when (and (derived-mode-p 'c-mode 'c++-mode)
               (locate-dominating-file default-directory ".clang-format"))
      (clang-format-buffer)))
  
  (defun bv-systems-setup-clang-format ()
    "Create a default .clang-format file."
    (interactive)
    (let ((root (or (bv-systems-find-project-root)
                    default-directory)))
      (with-temp-file (expand-file-name ".clang-format" root)
        (insert "BasedOnStyle: LLVM\n")
        (insert "IndentWidth: 4\n")
        (insert (format "Standard: %s\n" 
                        (if (derived-mode-p 'c++-mode)
                            bv-systems-default-cpp-standard
                          bv-systems-default-c-standard)))
        (insert "ColumnLimit: 100\n")
        (insert "AllowShortFunctionsOnASingleLine: Empty\n")
        (insert "BreakBeforeBraces: Attach\n")
        (insert "AlwaysBreakTemplateDeclarations: Yes\n")))))

;;;; CMake Support
(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'" . cmake-mode))
  :config
  (defun bv-systems-cmake-configure ()
    "Configure CMake project."
    (interactive)
    (let* ((root (or (bv-systems-find-project-root)
                     default-directory))
           (build-dir (expand-file-name 
                       (format "build-%s" (downcase bv-systems-cmake-build-type))
                       root)))
      (unless (file-directory-p build-dir)
        (make-directory build-dir t))
      (let ((default-directory build-dir))
        (compile (format "cmake -DCMAKE_BUILD_TYPE=%s -DCMAKE_EXPORT_COMPILE_COMMANDS=ON .."
                         bv-systems-cmake-build-type)))))
  
  (defun bv-systems-cmake-build ()
    "Build CMake project."
    (interactive)
    (let* ((root (or (bv-systems-find-project-root)
                     default-directory))
           (build-dir (expand-file-name 
                       (format "build-%s" (downcase bv-systems-cmake-build-type))
                       root)))
      (if (file-directory-p build-dir)
          (let ((default-directory build-dir))
            (compile "cmake --build . --parallel"))
        (message "Build directory not found. Run bv-systems-cmake-configure first."))))
  
  (defun bv-systems-cmake-clean ()
    "Clean CMake build."
    (interactive)
    (let* ((root (or (bv-systems-find-project-root)
                     default-directory))
           (build-dir (expand-file-name 
                       (format "build-%s" (downcase bv-systems-cmake-build-type))
                       root)))
      (when (file-directory-p build-dir)
        (let ((default-directory build-dir))
          (compile "cmake --build . --target clean"))))))

;;;; Eglot + Clangd Configuration
(with-eval-after-load 'eglot
  ;; Configure clangd
  (add-to-list 'eglot-server-programs
               `((c-mode c++-mode) . ("clangd" ,@bv-systems-clangd-args)))
  
  ;; Enhanced clangd configuration
  (defun bv-systems-eglot-config ()
    "Configure eglot for C/C++."
    (when (derived-mode-p 'c-mode 'c++-mode)
      ;; Ensure compile_commands.json is found
      (let ((compile-commands (bv-systems-find-compile-commands)))
        (when compile-commands
          (setq-local eglot-workspace-configuration
                      `(:clangd (:compilationDatabasePath 
                                 ,(file-name-directory compile-commands))))))))
  
  (add-hook 'eglot-managed-mode-hook 'bv-systems-eglot-config))

(defun bv-systems-find-compile-commands ()
  "Find compile_commands.json file."
  (when-let ((root (bv-systems-find-project-root)))
    (or (let ((build-dir (expand-file-name 
                          (format "build-%s" (downcase bv-systems-cmake-build-type))
                          root)))
          (when (file-exists-p (expand-file-name "compile_commands.json" build-dir))
            (expand-file-name "compile_commands.json" build-dir)))
        (when (file-exists-p (expand-file-name "compile_commands.json" root))
          (expand-file-name "compile_commands.json" root))
        (when (file-exists-p (expand-file-name "build/compile_commands.json" root))
          (expand-file-name "build/compile_commands.json" root)))))

;;;; Header/Source Navigation
(defun bv-systems-find-other-file ()
  "Switch between header and source file."
  (interactive)
  (let* ((file (file-name-nondirectory buffer-file-name))
         (base (file-name-sans-extension file))
         (ext (file-name-extension file))
         (other-exts (cond
                      ((member ext '("c" "cc" "cpp" "cxx" "c++"))
                       '("h" "hpp" "hxx" "h++"))
                      ((member ext '("h" "hpp" "hxx" "h++"))
                       '("c" "cc" "cpp" "cxx" "c++"))
                      (t nil))))
    (when other-exts
      (let ((other-file
             (cl-find-if
              'file-exists-p
              (cl-mapcan
               (lambda (other-ext)
                 (list
                  (expand-file-name (concat base "." other-ext))
                  (expand-file-name (concat base "." other-ext) "../include")
                  (expand-file-name (concat base "." other-ext) "../src")
                  (expand-file-name (concat base "." other-ext) "../../include")
                  (expand-file-name (concat base "." other-ext) "../../src")))
               other-exts))))
        (if other-file
            (find-file other-file)
          (message "Other file not found")))))

;;;; Include Path Management
(use-package cmake-ide
  :defer t
  :config
  (cmake-ide-setup))

(defun bv-systems-add-include-path ()
  "Add directory to include path."
  (interactive)
  (let ((dir (read-directory-name "Include directory: ")))
    (when dir
      (add-to-list 'cc-search-directories dir)
      (when (eglot-current-server)
        (eglot-signal-didChangeConfiguration (eglot-current-server))))))

;;;; Modern C++ Snippets
(bv-with-feature tempel
  (with-eval-after-load 'tempel
    (defvar bv-systems-cpp-templates
      '(c++-mode
        ;; Modern C++ patterns
        (class "class " (p "Name") " {\npublic:\n    " p> "\n};\n")
        (struct "struct " (p "Name") " {\n    " p> "\n};\n")
        (enum "enum class " (p "Name") " {\n    " p> "\n};\n")
        (ns "namespace " (p "name") " {\n" r> "\n} // namespace " (s name))
        (lambda "[](" p ") { " p " }")
        (auto "auto " (p "var") " = " p ";")
        (for "for (const auto& " (p "elem") " : " (p "container") ") {\n    " r> "\n}")
        (unique "std::unique_ptr<" (p "T") "> " (p "ptr") "(new " (s T) ");")
        (shared "std::shared_ptr<" (p "T") "> " (p "ptr") " = std::make_shared<" (s T) ">();")
        (vector "std::vector<" (p "T") "> " (p "vec") ";")
        (map "std::map<" (p "K") ", " (p "V") "> " (p "map") ";")
        (optional "std::optional<" (p "T") "> " (p "opt") ";")
        (variant "std::variant<" (p "Types...") "> " (p "var") ";")
        ;; Testing
        (test "TEST(" (p "TestCase") ", " (p "TestName") ") {\n    " r> "\n}")
        (gtest "#include <gtest/gtest.h>\n\n" p)
        ;; Headers
        (guard "#ifndef " (p (upcase (concat (file-name-base) "_H"))) n
               "#define " (s (upcase (concat (file-name-base) "_H"))) n n
               r n n
               "#endif // " (s (upcase (concat (file-name-base) "_H"))))
        (pragma "#pragma once\n")
        ;; Common includes
        (inc "#include <" (p "header") ">")
        (incl "#include \"" (p "header") "\""))
      "Modern C++ templates.")
    
    (add-to-list 'tempel-template-sources 'bv-systems-cpp-templates)))

;;;; Debugging with DAP
(bv-with-feature dape
  (with-eval-after-load 'dape
    ;; GDB configuration
    (add-to-list 'dape-configs
                 '(cpp-gdb
                   modes (c-mode c++-mode)
                   command "gdb"
                   command-args ("--interpreter=dap")
                   :request "launch"
                   :program (lambda ()
                              (let ((exe (bv-systems-find-executable)))
                                (read-file-name "Executable: " nil nil t exe)))
                   :args []
                   :cwd dape-cwd-fn
                   :stopAtBeginningOfMainSubprogram nil))
    
    ;; LLDB configuration
    (add-to-list 'dape-configs
                 '(cpp-lldb
                   modes (c-mode c++-mode)
                   command "lldb-vscode"
                   :request "launch"
                   :program (lambda ()
                              (let ((exe (bv-systems-find-executable)))
                                (read-file-name "Executable: " nil nil t exe)))
                   :args []
                   :cwd dape-cwd-fn
                   :stopOnEntry nil))))

(defun bv-systems-find-executable ()
  "Find likely executable in build directory."
  (when-let* ((root (bv-systems-find-project-root))
              (build-dir (expand-file-name 
                          (format "build-%s" (downcase bv-systems-cmake-build-type))
                          root)))
    (when (file-directory-p build-dir)
      (car (directory-files build-dir t "^[^.].*[^.]$" t))))))

;;;; Compilation
(defun bv-systems-compile ()
  "Smart compile for C/C++ projects."
  (interactive)
  (let ((root (bv-systems-find-project-root)))
    (cond
     ;; CMake project
     ((and root (file-exists-p (expand-file-name "CMakeLists.txt" root)))
      (bv-systems-cmake-build))
     ;; Makefile
     ((and root (file-exists-p (expand-file-name "Makefile" root)))
      (let ((default-directory root))
        (compile "make -j$(nproc)")))
     ;; Single file
     (t
      (compile (format "%s -std=%s -Wall -g -o %s %s"
                       (if (derived-mode-p 'c++-mode) "g++" "gcc")
                       (if (derived-mode-p 'c++-mode)
                           bv-systems-default-cpp-standard
                         bv-systems-default-c-standard)
                       (file-name-sans-extension buffer-file-name)
                       buffer-file-name))))))

;;;; Documentation
(defun bv-systems-cpp-reference ()
  "Open C++ reference documentation."
  (interactive)
  (browse-url "https://en.cppreference.com/"))

(defun bv-systems-search-cppreference (query)
  "Search cppreference for QUERY."
  (interactive "sSearch C++ reference for: ")
  (browse-url (format "https://en.cppreference.com/mwiki/index.php?search=%s" 
                      (url-encode-url query))))

;;;; Keybindings
(with-eval-after-load 'cc-mode
  (dolist (map (list c-mode-map c++-mode-map))
    (define-key map (kbd "C-c C-o") 'bv-systems-find-other-file)
    (define-key map (kbd "C-c C-c") 'bv-systems-compile)
    (define-key map (kbd "C-c c c") 'bv-systems-cmake-configure)
    (define-key map (kbd "C-c c b") 'bv-systems-cmake-build)
    (define-key map (kbd "C-c c C") 'bv-systems-cmake-clean)
    (define-key map (kbd "C-c d d") 'bv-systems-cpp-reference)
    (define-key map (kbd "C-c d s") 'bv-systems-search-cppreference)
    (define-key map (kbd "C-c i") 'bv-systems-add-include-path)
    (define-key map (kbd "C-c f") 'clang-format-buffer)
    (define-key map (kbd "C-c F") 'clang-format-region)))

;;;; Feature Definition
(defun bv-systems-load ()
  "Load C/C++ configuration."
  (add-to-list 'bv-enabled-features 'systems)
  
  ;; Check for required tools
  (unless (executable-find "clangd")
    (message "Warning: clangd not found. Install with your package manager."))
  
  (unless (executable-find "clang-format")
    (message "Note: clang-format not found. Formatting disabled."))
  
  (unless (executable-find "cmake")
    (message "Note: cmake not found. CMake features unavailable."))
  
  ;; Add to org-babel
  (bv-with-feature org
    (with-eval-after-load 'org
      (add-to-list 'org-babel-load-languages '(C . t)))))

(provide 'bv-lang-systems)
;;; bv-lang-systems.el ends here
