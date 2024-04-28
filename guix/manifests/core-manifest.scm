;; -*- mode: scheme; -*-
;; core-manifest.scm
(use-modules (gnu)
             (gnu packages admin)
             (gnu packages base)
             (gnu packages compression)
             (gnu packages gnupg)
             (gnu packages ssh)
             (gnu packages textutils)
             (gnu packages version-control)
             (gnu packages web)
             (gnu packages xorg)
             (gnu packages hunspell)
             (gnu packages math))

(specifications->manifest '("gitg" ;Graphical interface for git
                            "git-crypt" ;Git encryption plugin
                            "htop" ;Interactive process viewer
                            "tree" ;Display directories as trees
                            "gnupg" ;Encryption and signing tools
                            "pinentry" ;GnuPG's interface to passphrase input
                            "password-store" ;Password management
                            "xdg-utils" ;Freedesktop.org scripts for desktop integration
                            "dconf" ;Low-level GNOME configuration system
                            "gnome-shell-extensions" ;Extend and modify GNOME shell behaviour
                            "gsettings-desktop-schemas" ;GNOME settings for various desktop components
                            "rsync" ;File synchronization tool
                            "rclone" ;Cloud storage management
                            "fuse" ;Support file systems implemented in user space
                            "wget" ;Network file retrieval
                            "curl" ;Data transfer utility
                            "stow" ;Symlink farm manager
                            "lshw" ;Hardware lister
                            "inxi" ;Full-featured system information script
                            "smartmontools" ;SMART monitoring tools for hard disks
                            "xkeyboard-config" ;Keyboard configuration database
                            "pandoc" ;Universal document converter
                            "glibc-locales" ;C libraries' locales
                            "vim" ;Text editor
                            "tar" ;Archiving utility
                            "gzip" ;Compression utility
                            "bzip2" ;High-quality data compressor
                            "xz" ;Lossless data compression software
                            "unzip" ;Extraction utility for .zip files
                            "screen" ;Terminal multiplexer
                            "tmux" ;Another terminal multiplexer
                            "findutils" ;Utilities to find files
                            "grep" ;Text search utility
                            "sed" ;Stream editor
                            "mlocate" ;Quickly index and find files
                            "httrack" ;Website copier
                            "tcpdump" ;Powerful command-line packet analyzer
                            "wireshark" ;Network protocol analyzer
                            "nmap" ;Network exploration tool and security scanner
                            "traceroute" ;Tracks the route packets taken from an IP network
                            "iproute2" ;Advanced IP routing and network device configuration tools
                            "net-tools" ;Networking tools
                            "whois" ;Client for the whois directory service
                            "sysstat" ;System performance tools for Linux
                            "procps" ;Utilities that provide information about processes using the /proc filesystem
                            "atop" ;Advanced system and process monitor
                            "units" ;Converts between different systems of units
                            "firejail" ;SUID sandbox program to reduce the risk of security breaches
                            "parted" ;Disk partitioning and partition resizing tool
                            "gparted" ;GNOME partition editor for manipulating disk partitions
                            "dosfstools" ;Utilities for making and checking MS-DOS FAT filesystems
                            "e2fsprogs" ;Utilities for the ext2, ext3, and ext4 filesystems
                            "btrfs-progs" ;Utilities for managing the XFS filesystem
                            "strace" ;Trace system calls and signals
                            "ltrace" ;Track library calls made by a program during its execution
                            "systemtap" ;Instrumentation system for systems running Linux
                            "autoconf" ;An extensible package of M4 macros that produce shell scripts to automatically configure software source code packages
                            "automake" ;Tool for automatically generating Makefile.in files compliant with the GNU Coding Standards
                            "libtool" ;Generic library support script
                            "pkg-config" ;System for managing library compile/link flags
                            "make" ;GNU make utility to maintain groups of programs
                            "hunspell" ;Spell checker and morphological analyzer
                            "hunspell-dict-en" ;Hunspell dictionary for English
                            "hunspell-dict-en-us" ;Hunspell dictionary for United States English
                            "hunspell-dict-en-gb" ;Hunspell dictionary for British English, with -ise endings
                            "hunspell-dict-en-gb-ize" ;Hunspell dictionary for British English, with -ise endings
                            "maxima" ;Numeric and symbolic expression manipulation
                            "wxmaxima" ;GUI for Maxima
                            "gnuplot" ;Command-line driven graphing utility
                            ))
