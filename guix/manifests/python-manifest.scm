(use-modules (gnu packages python)
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
                            ))
