;; -*- mode: scheme; -*-
;; production-manifest.scm
(use-modules (gnu packages graphics)
             (gnu packages gimp)
             (gnu packages photo)
             (gnu packages audio)
             (gnu packages video)
             (gnu packages image)
             (gnu packages music)
             (gnu packages game-development)
             (gnu packages kde)
             (gnu packages inkscape)
             (gnu packages scribus)
             (gnu packages animation))

(specifications->manifest '("blender" ;3D creation suite for modeling, animation, simulation and rendering
                            "kdenlive" ;KDE Non-Linear Video Editor for multi-track video editing
                            "gimp" ;GNU Image Manipulation Program for raster graphics editing
                            "audacity" ;Audio software for multi-track recording and editing
                            "inkscape" ;Vector graphics editor comparable to Adobe Illustrator
                            "darktable" ;Photography workflow application and raw developer
                            "scribus" ;Desktop publishing application
                            "ardour" ;Digital audio workstation for recording, mixing and editing audio
                            "lmms" ;Music production software, alternative to FL Studio
                            "hydrogen" ;Advanced drum machine for GNU/Linux
                            "shotcut" ;Free, open-source, cross-platform video editor
                            "obs" ;Software for video recording and live streaming
                            "synfigstudio" ;Vector-based 2D animation package (GUI)
                            "pitivi" ;Video editor designed to be intuitive and integrate well in the GNOME desktop
                            "rawtherapee" ;Powerful cross-platform raw photo processing program
                            "godot" ;Multi-platform 2D and 3D game engine
                            "musescore" ;Free music composition and notation software
                            ))

