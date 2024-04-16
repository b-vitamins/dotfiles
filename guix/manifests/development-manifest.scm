;; -*- mode: scheme; -*-
;; development-manifest.scm
(use-modules (gnu)
             (gnu packages admin)
             (gnu packages base)
             (gnu packages commencement)
             (gnu packages compression)
             (gnu packages cmake)
             (gnu packages debug)
             (gnu packages gcc)
             (gnu packages python)
             (gnu packages racket)
             (gnu packages scheme)
             (gnu packages java)
             (gnu packages julia)
             (gnu packages node)
             (gnu packages ocaml)
             (gnu packages perl)
             (gnu packages lua)
             (gnu packages gtk)
             (gnu packages qt)
             (gnu packages graphics)
             (gnu packages gl)
             (gnu packages guile)
             (gnu packages haskell)
             (gnu packages idris)
             (myguix packages rust)
             (myguix packages rust-pqrs)
             (myguix packages llvm-pqrs))

(specifications->manifest '("gcc-toolchain" ;Comprehensive C and C++ development environment
                            "glibc" ;GNU C Library
                            "libgccjit" ;Just-In-Time compilation library for GCC
                            "llvm-with-bolt" ;LLVM with BOLT, optimized for performance
                            "jemalloc" ;Memory allocation library designed to improve performance
                            
                            "rust" ;Modern systems programming language focusing on safety and concurrency
                            "rust:rust-src" ;Source code for the Rust programming language
                            "rust:tools" ;Additional tools for Rust development
                            "rust:cargo" ;Rust's package manager and build system
                            "libtorch" ;The core library of PyTorch in C++
                            
                            "ghc" ;Next generation Haskell Compiler
                            "idris" ;Functional programming language with dependent types
                            
                            "python" ;Interpreted, high-level, general-purpose programming language
                            "guile" ;Official GNU extension language, Scheme dialect
                            "lua" ;Powerful, efficient, lightweight, embeddable scripting language
                            "perl" ;Highly capable, feature-rich programming language with over 30 years of development
                            "node" ;JavaScript runtime built on Chrome's V8 JavaScript engine
                            "racket" ;Scheme descendant designed for programming language development
                            "mit-scheme" ;MIT's implementation of the Scheme programming language
                            "ocaml" ;General-purpose programming language with an emphasis on expressiveness and safety
                            "openjdk" ;Free and open-source implementation of the Java Platform, Standard Edition
                            
                            "julia" ;High-level, high-performance dynamic programming language for technical computing
                            
                            ;; GUI Development
                            "gtk" ;GIMP Toolkit for creating graphical user interfaces
                            "qtbase" ;Cross-platform application framework for desktop and embedded development
                            "qt-creator" ;Cross-platform IDE for Qt applications
                            "kdevelop" ;Integrated development environment for multiple programming languages
                            "wxwidgets" ;C++ library for creating cross-platform applications
                            "fltk" ;Fast, light, cross-platform C++ GUI toolkit
                            "electron" ;Framework for creating native applications with web technologies
                            
                            ;; Graphics Programming
                            "glew" ;OpenGL Extension Wrangler Library
                            "glfw" ;Library for OpenGL, OpenGL ES and Vulkan application development
                            "sdl2" ;Cross-platform development library for providing low level access to audio, keyboard, mouse, joystick, and graphics hardware
                            "sfml" ;Simple and Fast Multimedia Library
                            "glade" ;RAD tool to enable quick & easy development of user interfaces for GTK+
                            "mesa" ;Open-source implementation of the OpenGL specification
                            "opencv" ;Open Source Computer Vision Library
                            
                            "gdb" ;GNU Debugger
                            "make" ;Utility that automatically builds executable programs and libraries from source code
                            "cmake" ;Family of tools designed to build, test, and package software
                            "valgrind" ;Instrumentation framework for building dynamic analysis tools
                            "perf" ;Performance monitoring tool for Linux
                            ))

