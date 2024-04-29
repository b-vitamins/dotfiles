;; -*- mode: scheme; -*-
;; cpp-manifest.scm
(use-modules (gnu packages machine-learning))

(specifications->manifest '("kaldi" ;Speech recognition toolkit
                            "kaldi-for-vosk" ;Speech recognition toolkit
                            "kaldi-gstreamer-server" ;Real-time full-duplex speech recognition server
                            "llama-cpp" ;Port of Facebook's LLaMA model in C/C++
                            ))
