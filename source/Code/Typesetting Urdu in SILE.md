---
title: Typesetting Urdu in SILE
published: 2015-05-04
---

### Introduction

[SILE][] is a new typesetting engine written by Simon Cozens. It is still under active development, but is already showing a lot of promise. Read the [SILE user manual][manual] to get a sense of what is possible currently.

[SILE]:http://www.sile-typesetter.org/
[manual]:http://www.sile-typesetter.org/images/sile-0.9.1.pdf

I stumbled upon SILE while trying to find alternatives to TeX for Urdu typesetting (see [Typesetting Urdu in Plain TeX](/Code/Typesetting Urdu in Plain TeX)). I was getting a bit frustrated with the half-baked support for BiDi typesetting in TeX. Moreover, I found the macro language for TeX quite idiosyncratic and hard to wrap my head around. SILE on the other hand is written in Lua, and the source is small enough to grasp in a single sitting. The user manual also does a great job of describing the SILE internals.

After reading the manual, I started typesetting my Urdu document immediately. There were some minor issues, so I decided to dig into the source code and hack around. I was able to make some fixes pretty quickly. More elaborate fixes required a deeper knowledge of the [Unicode BiDi algorithm][bidi], so I plan to continue after some reading and research.

[bidi]:http://www.unicode.org/reports/tr9/

### Prerequisites

* SILE (Read Chapter 2 of the user manual for installation instructions. You will need to install the [exp branch][exp] which contains some fixes for BiDi typesetting)
* [Jameel Noori Nastaleeq][jameel] font (for typesetting Urdu text in [Nastaʿlīq][nastaliq] script)

[exp]:https://github.com/deepakjois/sile/tree/exp
[jameel]:http://urdu.ca/UrduFonts.zip
[nastaliq]:http://en.wikipedia.org/wiki/Nasta%CA%BFl%C4%ABq_script

### Running

Download the [gist file][gist], unzip and run

[gist]:https://gist.github.com/deepakjois/79e89961b60de2e61e44/download

```
> sile urdu.sil
```

It should generate a PDF file [like this](https://dl.dropboxusercontent.com/u/953/site_assets/urdu_sile.pdf).

### Code

<script src="https://gist.github.com/deepakjois/79e89961b60de2e61e44.js"></script>
