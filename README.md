ace-server
==========

This is an app server of sorts, intended to replicate the mechanics of using Nginx as the front-end and having something like php-fpm in the back-end for scripting. Eventually I'd like to make this FastCGI-compatible, but for now it's going to be just plain CGI.

Of course I could have just set up some xinetd script of sorts to make racket run every time, but the startup time would be so loooong. I also use small web servers and I don't want to make a separate servlet for every app/site I want to run, as it means managing more daemons and eating more memory.

