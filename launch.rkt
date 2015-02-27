#lang racket/base

(require "ace-server.rkt")

(ace-server-add-settings
 (ace-settings "degen.tk"
                      #rx"(.*?\\.)*degen.tk$|manzanita|.*"
                      "/home/rat/Dropbox/DeGen/web"))


(ace-server
 #:default-root "/home/rat/src/ace-server"
 #:port 8098)
