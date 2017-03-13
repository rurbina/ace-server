#lang racket/base

; sample launch file for ace server
(require ace-server/ace)

(define hosts
  #hash{["degen.ml" . #hash{[base-path . "/home/rurbina/Dropbox/DeGen/web-static"]
                            [modules . (racket db ratamarkup/ratamarkup)]}]})

(ace #:hosts hosts)

