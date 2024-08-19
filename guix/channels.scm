;; -*- mode: scheme; -*-
(use-modules (guix channels))
(use-modules (guix ci))

(list (channel
        (name 'guix)
        (url "https://git.savannah.gnu.org/git/guix.git")
        (branch "master")
        (introduction
         (make-channel-introduction "9edb3f66fd807b096b48283debdcddccfea34bad"
          (openpgp-fingerprint
           "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
        (name 'myguix)
        (url "https://github.com/b-vitamins/myguix.git")
        (branch "master")
        (introduction
         (make-channel-introduction "85d58b09dc71e9dc9834b666b658f79d2e212d65"
          (openpgp-fingerprint
           "883B CA6B D275 A5F2 673C  C5DD 2AD3 2FC0 2A50 01F7")))))
