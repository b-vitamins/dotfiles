;; Optional Guix environment for the CuTe DSL skills; any equivalent
;; environment works (see references/environment.md). Upstream Guix does not
;; package the NVIDIA stack, so this needs a channel that provides these
;; packages. Usage: guix shell -m guix-manifest.scm -- <command>
(specifications->manifest '("python" "python-pytorch-cuda"
                            "python-einops"
                            "python-numpy@1"
                            "python-pytest"
                            "python-nvidia-cutlass-dsl"
                            "python-cuda-python"
                            "python-nvtx"
                            "python-cupti-python"
                            "python-ncu-report"
                            "python-nsight-python"
                            "cuda-toolkit"
                            "cutlass-headers"
                            "cutlass-tools"
                            "cpp-httplib"
                            "gcc-toolchain@14"
                            "ninja"
                            "cmake"
                            "pybind11"
                            "onednn"
                            "cudnn"
                            "unzip"
                            "python-ruff"
                            "node-pyright"))
