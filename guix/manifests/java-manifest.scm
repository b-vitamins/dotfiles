;; -*- mode: scheme; -*-
;; java-manifest.scm
(use-modules (gnu packages java)
             (gnu packages java-xml))

(specifications->manifest '("java-xerces" ;Validating XML parser for JAVA with DOM level 3 support
                            "java-jaxp" ;Java XML parser and transformer APIs (DOM, SAX, JAXP, TrAX)
                            "java-jdom" ;Access, manipulate, and output XML data
                            "java-dom4j" ;Flexible XML framework for Java
                            "java-jaxen" ;XPath library
                            ))
