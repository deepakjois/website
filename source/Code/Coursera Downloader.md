---
title: Coursera Downloader
published: 2015-03-31
lead: Script to download lecture videos (and their subtitles) from Coursera
---

### Prerequisites
Before running the script below, you need to have the following installed:

* [Ruby](https://www.ruby-lang.org/en/) (the script is written using the Ruby language)
* RubyGems `nokogiri` and `typhoeus` (Additional Ruby libraries the script depends on)

### Usage

Download the [gist file][gist], unzip and invoke the script with the username, password, course name and section number.

[gist]:https://gist.github.com/deepakjois/439e1eb8697058735ef4/download

* The username should be your email ID
* The course name can be found in the URL for the course, e.g _humanlanguage-001_, _cryptography-002_ etc.
* The section number is a 0-based integer obtained by counting the number of sections from top, on the Video Lectures
download page.

The script downloads and saves subtitle files (if any) and prints a list of direct, unprotected CDN URLs for the video files which can then be downloaded separately.

### Example
First download the subtitles, and write the list of URLs to a file *video_urls.txt*.

```
ruby fetch_links.rb user@na.me password humanlanguage-001 6 > video_urls.txt
```

Then use your favorite file download tool ([aria2][], [wget][] or [curl][]) to download the video files.

```
aria2c -i video_urls.txt
```

```
wget -i video_urls.txt
```

```
xargs -n 1 curl -O < urls.txt
```

[wget]:https://www.gnu.org/software/wget/
[curl]:http://curl.haxx.se
[aria2]:http://aria2.sourceforge.net

<script src="https://gist.github.com/deepakjois/439e1eb8697058735ef4.js"></script>
