---
pagetitle: Code
---
## Software

### Dhun
[Dhun] is a minimalist, commandline music player for OS X. I wrote this while
reading some chapters of the brilliant book [OS X Internals][osxinternals].
Dhun uses a C interface to OS X libraries to handle audio playback and Spotlight
querying on OS X. The rest of the code is written in Ruby. _This project is currently
unmaintained_.

[Dhun]: http://github.com/deepakjois/dhun
[osxinternals]: http://www.amazon.com/Mac-OS-Internals-Systems-Approach/dp/0321278542

### hs-gchart
[hs-gchart] is a Haskell wrapper around the [Google Chart API][gchartapi]. I
wrote this to get a better grip on [Haskell].

[hs-gchart]: http://github.com/deepakjois/hs-gchart
[GChartTypes]: http://hackage.haskell.org/packages/archive/hs-gchart/0.1/doc/html/Graphics-GChart-Types.html

### Tabbie
Aditya Krishnan and I were the original creators of [Tabbie][tabbie], an open source
Tab Software for British Parliamentary style debating tournaments. It was
first used in NTU Worlds 2004. Tabbie has been [used in many tournaments][tabbie_cust] around
the world.

Klaas Van Schelven is the maintainer of Tabbie (last I checked).

[tabbie]: http://tabbie.wikidot.com
[ntu]: http://www.ntu.edu.sg
[tabbie_cust]: http://tabbie.wikidot.com/customers

### gchartrb
[gchartrb][gchartrb] is a Ruby wrapper around the [Google Chart API][gchartapi]. It is
inspired by another Ruby based graphing library called [Gruff][gruff]. _This project is currently unmaintained_.

[gchartrb]: http://github.com/deepakjois/gchartrb
[gchartapi]: http://code.google.com/apis/chart/
[gruff]: http://nubyonrails.com/pages/gruff

## Bits of Code

### Hakyll website
This site runs on [Hakyll]. Apart from using the standard Hakyll functions, I
also [wrote some code][hakyllsource] to read a list of books from a JSON file and
render it to HTML using [blaze-html] combinators.

[Hakyll]: http://jaspervdj.be/hakyll/
[hakyllsource]: https://github.com/deepakjois/website/tree/master/bin
[blaze-html]: https://github.com/jaspervdj/blaze-html

### Kindler
[kindler] is a tool to generate nice .mobi files (readable on a [Kindle]) from
webpages. It is a quick hack written in Ruby, with a Javascript bookmarklet for
your browser. It also makes use of [Readability], curl and [kindlegen].

[kindler]: http://github.com/deepakjois/kindler
[Kindle]: http://en.wikipedia.org/wiki/Amazon_Kindle
[Readability]:http://lab.arc90.com/experiments/readability/
[kindlegen]:http://www.amazon.com/gp/feature.html?ie=UTF8&docId=1000234621

### hs-twitterarchiver
[hs-twitterarchiver] is a script written in [Haskell]. It archives all your past
Tweets in a JSON file. It also has the ability to sync an already existing
archive and update it with the latest feeds.

[hs-twitterarchiver]: http://github.com/deepakjois/hs-twitterarchiver/tree/master

### Patches to yst
[yst] is the static web page creator that used to power this site. I contributed some
patches to it ([1][patch1],[2][patch2],[3][patch3]).

[yst]: http://github.com/jgm/yst/
[patch1]: http://github.com/jgm/yst/commit/1388233929b3330bea3faf466762b37b59e6e79f
[patch2]: http://github.com/jgm/yst/commit/e971e957816be32a537471d34b1d020e193cc3e3
[patch3]: http://github.com/jgm/yst/commit/2175d5b8f0c5ab47d44f44e64729a91c309bb9ac

### Project Euler Problems
Solved a few [Project Euler] problems to learn [Haskell]. I have put up some problem
[solutions on github][euler-github].

[Project Euler]: http://projecteuler.net
[Haskell]: http://www.haskell.org/
[euler-github]: http://github.com/deepakjois/projecteuler/tree/master
