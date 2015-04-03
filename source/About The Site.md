---
title: About The Site
date: Apr 3, 2015
lead: This site started life back in April 2009.
---

Since then I keep gravitating between actively updating it and ignoring it completely, with an occasional burst where I fiddle around with the backend and design.

## Content
In the latest iteration, I am trying to gather everything I have written (prose or code) in a single place. I don’t write very extensively, which is something I have been trying to fix. One of the motivations of running this site is to put more of my thoughts down in writing. I appreciate receiving any suggestions or criticisms. You can send me [anonymous feedback][feedback].

[feedback]: https://docs.google.com/forms/d/1JHsgl6-FXZNdBkEZSHNrGkb9gTjqooqXOM6NvmGau3c/viewform"

Currently, the best way to navigate the content is to use the [home page](/). When the site grows and adds more pages, I plan to add a section listing the latest entries in chronological order (with an <abbr title="Really Simple Syndication">RSS</abbr> feed to go along with it as well).

## Colophon

The site is served as static HTML content, which is generated using [Hakyll][hakyll]. Hakyll is not the easiest tool to generate static sites, but it is very powerful and customisable. It uses [Pandoc][pandoc] to process textual content. Compilation rules are written in a [Haskell][haskell] based <abbr title="Embedded Domain Specific Language">EDSL</abbr>. This make it easy to generate content like the book lists (for e.g. see [1](Book List 2014), [2](Book List 2015)), which are backed by [flat files][flat-files] that store book info in <abbr title="Javascript Object Notation">JSON</abbr> format.

[hakyll]:http://jaspervdj.be/hakyll/
[haskell]:https://www.haskell.org
[pandoc]:http://johnmacfarlane.net/pandoc/
[flat-files]:https://github.com/deepakjois/website/tree/master/data

CSS styling is done using [Bootstrap 3.0][bootstrap] almost exclusively. I am not a big fan of the sans-serif font for the body text, so I intend to change that when I have more time to dig into Bootstrap and customise a version specific to this site.

[Amiri font] is used to typeset Urdu. Amiri is a nice balance between some of the more geometric and plain looking Arabic fonts that come bundled on most OSes, and the highly stylised [Nastaliq][nastaliq] fonts (in which Urdu is traditionally typeset) which make blingual typesetting on a webpage very complicated.

[bootstrap]:http://getbootstrap.com
[amiri]: http://www.amirifont.org/
[nastaliq]: http://en.wikipedia.org/wiki/Nasta%CA%BFl%C4%ABq_script
