(use-modules (gnu packages python))

;; Define the Python package manifest with a selection of commonly used modules
(specifications->manifest '("python@3.10.7" ;Core Python language, specific version
                            "python-numpy@1.23.2" ;Numerical computing with Python
                            "python-matplotlib@3.8.2" ;Plotting library for Python
                            "python-scipy@1.12.0" ;Scientific computing tools for Python
                            "python-jedi@0.18.2" ;Autocompletion library for Python
                            "python-jedi-language-server" ;Language server based on Jedi for IDE-like features
                            "python-flake8@4.0.1" ;Static code analysis tool for Python
                            "python-black@22.3.0" ;The uncompromising Python code formatter
                            "python-pandas" ;Data structures and data analysis tools
                            "python-requests" ;Elegant and simple HTTP library for Python
                            "python-sqlalchemy" ;SQL toolkit and Object-Relational Mapping (ORM) for Python
                            "python-pytest" ;Testing framework for Python
                            "python-flask" ;A lightweight WSGI web application framework
                            "python-boto3" ;Amazon Web Services (AWS) SDK for Python
                            "python-scikit-learn" ;Machine learning in Python
                            "python-virtualenv" ;Tool to create isolated Python environments
                            "python-ipython" ;Enhanced interactive Python shell
                            "python-notebook" ;Web-based notebook environment for interactive computing
                            ))
