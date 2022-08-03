ace-server
==========

This is like a php-fpm but for Scribble-enriched HTML files. Scribble is a markup language in Racket that allows you to embed actual Racket code in any kind of text files.

`ace` is not really using Scribble's HTML output capabilities, it's only using it as a template toolkit of sorts, so you type your actual HTML and use @-expressions for includes and other processing.

Making a cgi-bin mechanism for this would be too slow because Racket can take several seconds to boot. Having a single process allows you to keep libs and settings loaded, allowing a snappier load. Also, you can load other Racket scripts (such as Geeklog) in a single threaded process and save more memory.
