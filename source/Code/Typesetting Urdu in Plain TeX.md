---
title: Typesetting Urdu in Plain TeX
published: 2015-04-12
---

### Introduction

[XeLaTeX][] makes it easy to typeset Urdu text. There is [a nice guide][guide] that has all the details.

[guide]:http://patriot.net/~abdali/urdumac.html#TypesettingWithTeX
[XeLaTeX]:http://www.xelatex.org/

This page is about typesetting Urdu in Plain [XeTeX], without any additional formats. It is not very straightforward and requires a lot of heavy lifting. I have been experimenting with it off and on.

[XeTeX]:http://xetex.sourceforge.net/

On a related note, I haven’t found a text editor that can handle bilingual text when one of the languages is written left-to-right and the other right-to-left. It happens a lot in this case because the commands are in English, and the text in Urdu. When delimiters and commands appear with Urdu text, the sequence can get messed up. To avoid confusion, you could try putting Urdu and non-Urdu symbols on different lines (see code below).

On OS X, the [Katib][] editor is decent for editing languages that are written from right-to-left, but it’s not without kinks. It is designed more as a Markdown text editor, so it isn’t ideal for editing code. If you have found something that works nicely, send me a message.

[Katib]:http://katibapp.com/

### Prerequisites

Before running the code sample below, you need to have the following installed

* XeTeX (of course)
* [Jameel Noori Nastaleeq][jameel] font (for typesetting Urdu text in [Nastaʿlīq][nastaliq] script)
* [Ek Mukta][ek-mukta] font (for typesetting some Hindi text in between)

[jameel]:http://urdu.ca/UrduFonts.zip
[ek-mukta]:https://github.com/girish-dalvi/Ek-Mukta/releases
[nastaliq]:http://en.wikipedia.org/wiki/Nasta%CA%BFl%C4%ABq_script

### Running

Download the [gist file][gist], unzip and run

[gist]:https://gist.github.com/deepakjois/b8f0935b4db21006b8ae/download

```
> xetex urdu.tex
```

It should generate a PDF file [like this](/assets/urdu_plain_tex.pdf).

### Code

<script src="https://gist.github.com/deepakjois/b8f0935b4db21006b8ae.js"></script>
