---
title: About The Site
published: 2015-04-03
---

This site started life back in April 2009. Since then I keep gravitating between actively updating it and ignoring it completely, with an occasional burst where I fiddle around with the backend and design.

## Content
In the latest iteration of this site, I am trying to gather everything I have written (prose or code) in a single place. Currently, the best way to navigate the content is to use the [home page](/). When the site grows and more pages are added, I plan to add a tags, a section listing the latest entries in chronological order, and an <abbr title="Really Simple Syndication">RSS</abbr> feed.

## Colophon

The site is served as static HTML content, which is generated using [Hakyll][hakyll]. Hakyll is not the easiest tool to generate static sites, but it is very powerful and customisable. It uses [Pandoc][pandoc] to process textual content. Compilation rules are written in a [Haskell][haskell] based <abbr title="Embedded Domain Specific Language">EDSL</abbr>. This make it easy to generate content like the book lists (for e.g. see [1](Book List 2014), [2](Book List 2015)), which are backed by [flat files][flat-files] that store book info in <abbr title="Javascript Object Notation">JSON</abbr> format.

[hakyll]:http://jaspervdj.be/hakyll/
[haskell]:https://www.haskell.org
[pandoc]:http://johnmacfarlane.net/pandoc/
[flat-files]:https://github.com/deepakjois/website/tree/master/data

CSS styling is done using [Bootstrap 3.0][bootstrap] almost exclusively. I am not a big fan of the sans-serif font for the body text, so I intend to change that when I have more time to dig into Bootstrap and customise a version specific to this site.

[Amiri font][amiri] is used to typeset Urdu. Amiri is a nice balance between some of the more geometric and plain looking Arabic fonts that come bundled on most OSes, and the highly stylised [Nastaliq][nastaliq] fonts (in which Urdu is traditionally typeset) which make blingual typesetting on a webpage very complicated.

[bootstrap]:http://getbootstrap.com
[amiri]: http://www.amirifont.org/
[nastaliq]: http://en.wikipedia.org/wiki/Nasta%CA%BFl%C4%ABq_script

I went through a bit of pain to get [Cool URIs][cool-uris] to work properly. Hakyll’s default web server (invoked using `hakyll watch`) does not recognise the Content-type of pages with no extensions, so I had to [write my own][serve.go]. I also wrote my own [uploader][upload.go], because [s3cmd][] was pretty terrible at sniffing MIME-types effectively and handling uploads of filenames with unicode chars in them. Both the web server and uploader were written in [Go.][go]

[cool-uris]:http://www.w3.org/Provider/Style/URI.html
[go]:http://golang.org/
[serve.go]:https://github.com/deepakjois/website/blob/master/serve.go
[upload.go]:https://github.com/deepakjois/website/blob/master/upload.go
[s3cmd]: http://s3tools.org

The source code for this site is [hosted on Github][github-url]. This site is licensed under the [Creative Commons][cc] [public domain][cc0] license.

[github-url]:https://github.com/deepakjois/website
[cc]: http://en.wikipedia.org/wiki/Creative%20Commons
[cc0]: http://creativecommons.org/about/cc0

## Credits

* [Gwern.net][gwern] for a lot of ideas around organising the site content.
* Surabhi Agarwal for  proofreading and providing helpful suggestions.
* [Jasper][jasper] for building Hakyll.
* [Khaled Hosny][hosny] for Amiri font.

[gwern]:http://gwern.net
[jasper]: http://jaspervdj.be
[hosny]: http://www.khaledhosny.org/
