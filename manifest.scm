(use-modules (guix ci))

(specifications->manifest '("coreutils" "glib:bin"
                            "xdg-utils"
                            "dconf"
                            "gsettings-desktop-schemas"
                            "python"
                            "python-pyside-2-tools"
                            "xvfb-run"))
