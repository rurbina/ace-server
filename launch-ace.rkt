#lang racket/base

; sample launch file for ace server
(require "ace.rkt")

(ace-server
 #:hosts (make-hash `(["local.degen.ml" . (make-hash `([base-path . "/www/degen"]))])))

