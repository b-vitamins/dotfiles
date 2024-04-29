;; -*- mode: scheme; -*-
;; cpp-manifest.scm
(use-modules (gnu packages machine-learning)
             (gnu packages maths))

(specifications->manifest '("kaldi" ;Speech recognition toolkit
                            "kaldi-gstreamer-server" ;Real-time full-duplex speech recognition server
                            "llama-cpp" ;Port of Facebook's LLaMA model in C/C++
                            "openblas" ;OpenBLAS is a BLAS library forked from the GotoBLAS2-1.13 BSD version.
                            ))
