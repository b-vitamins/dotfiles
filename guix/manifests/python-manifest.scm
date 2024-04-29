;; -*- mode: scheme; -*-
;; python-manifest.scm
(use-modules (gnu packages python)
             (gnu packages python-xyz)
             (gnu packages python-web)
             (gnu packages python-science)
             (gnu packages machine-learning)
             (gnu packages xml)
             (gnu packages search)
             (myguix packages python-pqrs)
             (guix-science packages python)
             (guix-science packages machine-learning))

(specifications->manifest '("python" ;Core Python language, specific version
                            "python-numpy" ;Numerical computing with Python
                            "python-matplotlib" ;Plotting library for Python
                            "python-scipy" ;Scientific computing tools for Python
                            "python-jedi" ;Autocompletion library for Python
                            "python-jedi-language-server" ;Language server based on Jedi for IDE-like features
                            "python-flake8" ;Static code analysis tool for Python
                            "python-black" ;The uncompromising Python code formatter
                            "python-pandas" ;Data structures and data analysis tools
                            "python-requests" ;Elegant and simple HTTP library for Python
                            "python-sqlalchemy" ;SQL toolkit and Object-Relational Mapping (ORM) for Python
                            "python-pytest" ;Testing framework for Python
                            "python-flask" ;A lightweight WSGI web application framework
                            "python-scikit-learn" ;Machine learning in Python
                            "python-scikit-learn-extra" ;Set of tools for scikit-learn
                            "python-pytorch" ;Tensors and Dynamic neural networks in Python with strong GPU acceleration
                            "python-pytorch-lightning" ;Deep learning framework to train, deploy, and ship AI products
                            "python-torchmetrics" ;Machine learning metrics for PyTorch applications
                            "python-torchvision" ;Datasets, transforms and models specific to computer vision
                            "python-aiohttp" ;Async HTTP client/server framework
                            "python-httpx" ;A fully featured HTTP client for Python 3, which provides sync and async APIs
                            "python-websockets" ;A library for building WebSocket servers and clients in Python with a focus on correctness and simplicity
                            "python-rich" ;Rich text and beautiful formatting in the terminal
                            "python-django" ;The Web framework for perfectionists with deadlines
                            "python-grobid-client-python" ;Simple Python client for GROBID REST services for extracting, parsing, and annotating academic documents
                            "python-pyalex" ;Python interface to the OpenAlex database, providing easy data access for bibliographic and citation information
                            "python-beautifulsoup4" ;Python screen scraping library
                            "python-scrapy" ;High-level Web crawling and Web scraping framework
                            "python-pillow" ;Fork of Python Image Library (PIL)
                            "python-pillow-simd" ;Fork of Python Image Library (PIL) with SIMD support
                            "tensorflow" ;Machine learning framework
                            "python-tensorflow" ;Machine learning framework
                            "python-keras-preprocessing" ;Data preprocessing and augmentation for deep learning models
                            "python-keras" ;High-level deep learning framework
                            "python-jax" ;Differentiate, compile, and transform Numpy code
                            "python-jaxtyping" ;Type annotations and runtime checking for JAX arrays and others
                            "python-jaxlib" ;Differentiate, compile, and transform Numpy code
                            "python-orbax-checkpoint" ;Utility libraries for JAX users
                            "python-optax" ;Gradient processing and optimization library for JAX
                            "python-jmp" ;JMP is a mixed precision library for JAX
                            "python-opt-einsum" ;Optimizing numpy's einsum function
                            "python-sympy" ;Python library for symbolic mathematics
                            "python-nltk" ;Natural language toolkit
                            "python-spacy" ;Natural Language Processing (NLP) in Python
                            "python-bokeh" ;Interactive plots and applications in the browser from Python
                            "python-openai" ;Python client library for the OpenAI API
                            "python-pypdf2" ;Pure Python PDF toolkit
                            "python-pdfminer-six" ;PDF parser and analyzer
                            "python-lxml" ;Python bindings for libxml2
                            "python-xapian-bindings" ;Python bindings for the Xapian search engine library
                            "vosk-api" ;Speech recognition toolkit based on `kaldi'
                            "python-vosk" ;Speech recognition toolkit based on `kaldi'
                            "nerd-dictation" ;Offline speech-to-text for desktop Linux
                            ))
