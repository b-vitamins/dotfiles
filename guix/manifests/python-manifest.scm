;; -*- mode: scheme; -*-
;; python-manifest.scm
(use-modules (gnu packages python)
             (gnu packages python-xyz)
             (gnu packages python-web)
             (gnu packages machine-learning)
             (myguix packages python-pqrs))

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
                            "python-pytorch" ;Tensors and Dynamic neural networks in Python with strong GPU acceleration
                            "python-aiohttp" ;Async HTTP client/server framework
                            "python-httpx" ;A fully featured HTTP client for Python 3, which provides sync and async APIs
                            "python-websockets" ;A library for building WebSocket servers and clients in Python with a focus on correctness and simplicity
                            "python-rich" ;Rich text and beautiful formatting in the terminal
                            "python-django" ;The Web framework for perfectionists with deadlines
                            "python-grobid-client-python" ;Simple Python client for GROBID REST services for extracting, parsing, and annotating academic documents
                            "python-pyalex" ;Python interface to the OpenAlex database, providing easy data access for bibliographic and citation information
                            ))

