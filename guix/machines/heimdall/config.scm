(use-modules (gnu))
(use-service-modules cups
                     desktop
                     networking
                     ssh
                     xorg
                     sysctl
                     vpn)

(define %vpn-nftables-ruleset
  (plain-file "nftables.conf"
   "# A simple and safe firewall with OpenVPN support
table inet filter {
  chain input {
    type filter hook input priority 0; policy drop;

    # early drop of invalid connections
    ct state invalid drop

    # allow established/related connections
    ct state { established, related } accept

    # allow from loopback
    iif lo accept
    # drop connections to lo not coming from lo
    iif != lo ip daddr 127.0.0.1/8 drop
    iif != lo ip6 daddr ::1/128 drop

    # allow icmp
    ip protocol icmp accept
    ip6 nexthdr icmpv6 accept

    # allow ssh
    tcp dport ssh accept

    # allow OpenVPN connections on TCP port 443
    tcp dport 443 accept

    # allow traffic on the tun0 interface (VPN)
    iifname tun0 accept

    # reject everything else
    reject with icmpx type port-unreachable
  }

  chain forward {
    type filter hook forward priority 0; policy drop;

    # Allow traffic from VPN clients (tun0) to the internet (eth0)
    iifname tun0 oifname eth0 accept

    # Allow traffic from the internet (eth0) to VPN clients (tun0)
    iifname eth0 oifname tun0 accept

    # Allow forwarding of established and related connections
    ct state { established, related } accept
  }

  chain output {
    type filter hook output priority 0; policy accept;
  }
}

# NAT table for OpenVPN
table ip nat {
  chain postrouting {
    type nat hook postrouting priority 100; policy accept;

    # Masquerade traffic from the VPN (tun0) to the internet (eth0)
    ip saddr 10.43.0.0/24 oifname eth0 masquerade
  }
}
"))

(operating-system
  (locale "en_US.utf8")
  (timezone "Asia/Kolkata")
  (host-name "heimdall")

  (keyboard-layout (keyboard-layout "us" "altgr-intl"
                                    #:options '("ctrl:nocaps"
                                                "altwin:swap_alt_win")))
  ;; User accounts with a custom shell for the 'b' user
  (users (cons* (user-account
                  (name "b")
                  (comment "Ayan")
                  (group "users")
                  (home-directory "/home/b")
                  (shell (file-append (specification->package "zsh")
                                      "/bin/zsh"))
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  ;; System-wide packages
  (packages (append (list (specification->package "git")
                          (specification->package "wget")
                          (specification->package "curl")
                          (specification->package "emacs-no-x-toolkit")
                          (specification->package "htop")
                          (specification->package "nmap")
                          (specification->package "screen")
                          (specification->package "zstd")
                          (specification->package "coreutils")
                          (specification->package "font-dejavu")
                          (specification->package "font-iosevka-comfy")
                          (specification->package "font-google-noto")
                          (specification->package "font-google-noto-serif-cjk")
                          (specification->package "font-google-noto-sans-cjk")
                          (specification->package "fontconfig"))
                    %base-packages))

  ;; Below is the list of system services.  To search for available
  ;; services, run 'guix system search KEYWORD' in a terminal.
  (services
   (append (list

                 ;; To configure OpenSSH, pass an 'openssh-configuration'
                 ;; record as a second argument to 'service' below.
                 ;; OpenSSH for remote access
                 (service openssh-service-type
                          (openssh-configuration (authorized-keys `(("b" ,(local-file
                                                                           "../../../keys/ssh/ragnar.pub"))
                                                                    ("b" ,(local-file
                                                                           "../../../keys/ssh/leif.pub"))
                                                                    ("b" ,(local-file
                                                                           "../../../keys/ssh/bjorn.pub"))
                                                                    ("b" ,(local-file
                                                                           "../../../keys/ssh/freydis.pub"))))
                                                 (password-authentication? #f)
                                                 (port-number 2123)))
                 (service network-manager-service-type)
                 (service wpa-supplicant-service-type)
                 (service nftables-service-type
                          (nftables-configuration (ruleset
                                                   %vpn-nftables-ruleset)))

                 (service ntp-service-type)
                 (service gpm-service-type)
                 (service openvpn-server-service-type
                          (openvpn-server-configuration (proto 'tcp)
                                                        (port 443)
                                                        (ca
                                                         "/etc/openvpn/ca.crt")
                                                        (cert
                                                         "/etc/openvpn/server.crt")
                                                        (key
                                                         "/etc/openvpn/server.key")
                                                        (dh
                                                         "/etc/openvpn/dh2048.pem")
                                                        (server
                                                         "10.43.0.0 255.255.255.0")
                                                        (keepalive '(10 120))
                                                        (persist-key? #t)
                                                        (persist-tun? #t)
                                                        (client-to-client? #t)
                                                        (redirect-gateway? #t)
                                                        (max-clients 10)
                                                        (status
                                                         "/var/log/openvpn-status.log")
                                                        (verbosity 3))))
           (modify-services %base-services
             (sysctl-service-type config =>
                                  (sysctl-configuration
                                   (settings (append '(("net.ipv4.ip_forward" . "1"))
                                                     %default-sysctl-settings)))))))
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets (list "/dev/sda"))
                (keyboard-layout keyboard-layout)))
  (initrd-modules (append '("virtio_scsi") %base-initrd-modules))
  (swap-devices (list (swap-space
                        (target (uuid "d43a5d28-30e8-4fc9-a549-dc2acd8d804d")))))

  ;; The list of file systems that get "mounted".  The unique
  ;; file system identifiers there ("UUIDs") can be obtained
  ;; by running 'blkid' in a terminal.
  (file-systems (cons* (file-system
                         (mount-point "/boot/efi")
                         (device (uuid "4752-1080"
                                       'fat32))
                         (type "vfat"))
                       (file-system
                         (mount-point "/")
                         (device (uuid "f2598482-d63b-4d4d-b0cb-511242dc54c3"
                                       'ext4))
                         (type "ext4")) %base-file-systems)))

;; Instructions for setting up the /etc/openvpn directory:

;; 1. Ensure that the following files are placed in /etc/openvpn:
;;
;;    - ca.crt: CA certificate
;;    - ca.key: CA private key
;;    - ca.srl: CA serial file (can be generated or copied)
;;    - client.crt: Client certificate
;;    - client.csr: Client certificate signing request (CSR)
;;    - client.key: Client private key
;;    - dh2048.pem: Diffie-Hellman parameters
;;    - server.crt: Server certificate
;;    - server.csr: Server certificate signing request (CSR)
;;    - server.key: Server private key
;;
;; 2. The permissions and ownership for these files must be set as follows:
;;
;;    - ca.crt:
;;        Permissions: 644 (rw-r--r--)
;;        Owner: openvpn:openvpn
;;        Command: `sudo chown openvpn:openvpn /etc/openvpn/ca.crt && sudo chmod 644 /etc/openvpn/ca.crt`
;;
;;    - ca.key:
;;        Permissions: 600 (rw-------)
;;        Owner: openvpn:openvpn
;;        Command: `sudo chown openvpn:openvpn /etc/openvpn/ca.key && sudo chmod 600 /etc/openvpn/ca.key`
;;
;;    - ca.srl:
;;        Permissions: 644 (rw-r--r--)
;;        Owner: root:root
;;        Command: `sudo chown root:root /etc/openvpn/ca.srl && sudo chmod 644 /etc/openvpn/ca.srl`
;;
;;    - client.crt:
;;        Permissions: 644 (rw-r--r--)
;;        Owner: openvpn:openvpn
;;        Command: `sudo chown openvpn:openvpn /etc/openvpn/client.crt && sudo chmod 644 /etc/openvpn/client.crt`
;;
;;    - client.csr:
;;        Permissions: 644 (rw-r--r--)
;;        Owner: root:root
;;        Command: `sudo chown root:root /etc/openvpn/client.csr && sudo chmod 644 /etc/openvpn/client.csr`
;;
;;    - client.key:
;;        Permissions: 600 (rw-------)
;;        Owner: openvpn:openvpn
;;        Command: `sudo chown openvpn:openvpn /etc/openvpn/client.key && sudo chmod 600 /etc/openvpn/client.key`
;;
;;    - dh2048.pem:
;;        Permissions: 600 (rw-------)
;;        Owner: openvpn:openvpn
;;        Command: `sudo chown openvpn:openvpn /etc/openvpn/dh2048.pem && sudo chmod 600 /etc/openvpn/dh2048.pem`
;;
;;    - server.crt:
;;        Permissions: 600 (rw-------)
;;        Owner: openvpn:openvpn
;;        Command: `sudo chown openvpn:openvpn /etc/openvpn/server.crt && sudo chmod 600 /etc/openvpn/server.crt`
;;
;;    - server.csr:
;;        Permissions: 644 (rw-r--r--)
;;        Owner: root:root
;;        Command: `sudo chown root:root /etc/openvpn/server.csr && sudo chmod 644 /etc/openvpn/server.csr`
;;
;;    - server.key:
;;        Permissions: 600 (rw-------)
;;        Owner: openvpn:openvpn
;;        Command: `sudo chown openvpn:openvpn /etc/openvpn/server.key && sudo chmod 600 /etc/openvpn/server.key`
;;
;; 3. Make sure the OpenVPN service has permission to access these files and that they are readable by the service, while keeping private keys protected with restricted access.
