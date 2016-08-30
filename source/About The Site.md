---
title: About The Site
published: 2015-04-03
updated: 2016-08-30
---

This site started life back in April 2009. Since then I keep gravitating between actively updating it and ignoring it completely, with an occasional burst where I fiddle around with the backend and design.

## Content
The source code for this site is [hosted on Github][github-url]. This site is licensed under the [Creative Commons][cc] [public domain][cc0] license.

[github-url]:https://github.com/deepakjois/website
[cc]: http://en.wikipedia.org/wiki/Creative%20Commons
[cc0]: http://creativecommons.org/about/cc0

## Colophon

### Content Generation
The site is served as static HTML content, which is generated using [Hakyll][hakyll]. Hakyll is not the easiest tool to generate static sites, but it is very powerful and customisable. It uses [Pandoc][pandoc] to process textual content. Compilation rules are written in a [Haskell][haskell] based <abbr title="Embedded Domain Specific Language">EDSL</abbr>. This make it easy to generate content like the book lists (for e.g. see [1](Book List 2014), [2](Book List 2015)), which are backed by [flat files][flat-files] that store book info in <abbr title="Javascript Object Notation">JSON</abbr> format.

[hakyll]:http://jaspervdj.be/hakyll/
[haskell]:https://www.haskell.org
[pandoc]:http://johnmacfarlane.net/pandoc/
[flat-files]:https://github.com/deepakjois/website/tree/master/data

### Design and Typography
CSS styling is done using [Bootstrap 3.0][bootstrap] almost exclusively.

The typography of the site is a work in progress. [Amiri font][amiri] is used to typeset Urdu. Amiri is a nice balance between some of the more geometric and plain looking Arabic fonts that come bundled on most OSes, and the highly stylised [Nastaliq][nastaliq] fonts (in which Urdu is traditionally typeset) which make blingual typesetting on a webpage very complicated.

[bootstrap]:http://getbootstrap.com
[amiri]: http://www.amirifont.org/
[nastaliq]: http://en.wikipedia.org/wiki/Nasta%CA%BFl%C4%ABq_script

### Hosting
The site is hosted on [Amazon S3][s3], and uses [Amazon Route 53][route53] for DNS management.

[s3]:http://aws.amazon.com/s3/
[route53]:http://aw.amazon.com/route53/

I went through a bit of pain to get [Cool URIs][cool-uris] to work properly. The tools available out there do not deal well with text files that do not have an extension. I wrote my own [uploader][upload.go], because [s3cmd][] was not sniffing MIME-types correctly. Both the web server and uploader were written in [Go.][go]

[cool-uris]:http://www.w3.org/Provider/Style/URI.html
[go]:http://golang.org/
[upload.go]:https://github.com/deepakjois/website/blob/master/upload.go
[s3cmd]:http://s3tools.org

## Credits

* [Gwern.net][gwern] for a lot of ideas around organising the site content.
* Surabhi Agarwal for  proofreading and providing helpful suggestions.
* [Jasper][jasper] for building Hakyll.
* [Khaled Hosny][hosny] for Amiri font.

[gwern]:http://gwern.net
[jasper]: http://jaspervdj.be
[hosny]: http://www.khaledhosny.org/
