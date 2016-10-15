---
title: Log Book
published: 2016-08-30
---

## 13 Jun 2016

### Webpack
* Webpack has a `style-loader` and `css-loader` for processing `require`s which have CSS files in them. This can be combined with the `extract-text-plugin` to have component-specific CSS during coding, but bundling it while running the code.
* `style-loader` loads the raw style files, but it can be chained with `css-loader` to provide some additional functionality for @import etc.
* `copy-webpack-plugin` can be used to copy static assets from one place to another

### Tmux
We can setup tmux sessions in advance using scripting. Here is an example:
<http://minimul.com/increased-developer-productivity-with-tmux-part-4.html>

## 14 Jun 2016
* `html-webpack-plugin` uses underscore templates to create HTML pages with injected tags to incorporate javascript and css assets. Here is an example (the default template):
<https://github.com/jaketrent/html-webpack-template/blob/86f285d5c790a6c15263f5cc50fd666d51f974fd/index.html>

### Using ESLint with Babel and React
* It is recommended to install ESLint locally
* Although `eslint` supports ES6 (including JSX), use `babel-lint` to lint code because it supports additional stuff apart from ES6 like class properties, decorators, async/await, types.
* Install `eslint-plugin-react`
* Make sure you have .eslintignore file in place to ignore places like `node_modules`.
* Here is a sample configuration:

    {
      "parser": "babel-eslint",
      "env": {
        "browser": true,
        "node": true
      },
      "extends": ["eslint:recommended", "plugin:react/recommended"],
      "installedESLint": true,
      "plugins": ["react"]
    }

## Using Jest with React and Babel
* Install `react-addons-test-utils`
* <https://facebook.github.io/jest/docs/getting-started.html#babel-integration>

## 15 Jun 2016

### Intel NUC nuc6i7kyk
* Tech Specs: <http://www.intel.com/content/www/us/en/nuc/nuc-kit-nuc6i7kyk-features-configurations.html>
* Anandtech Review: <http://www.anandtech.com/show/10343/the-intel-skull-canyon-nuc6i7kyk-minipc-review>

## 21 Jun 2016

### coursera-dl setup
* Follow the instructions to setup a virtualenv on the coursera-dl github README
* Added a sleep in the get_page method in network.py to ensure that we are not hitting the Coursera Servers too aggressively

## 22 Jun 2016

### coursera-dl
* Disabling keep-alive might help too.

### Moocfetcher
filter launched courses and eliminating missed courses:

```
cat launched.json | jq -r '.courses[] | select(.slug  as $slug | (["course-design", "optobotics", "international-marketing", "language-coordinator"] | map($slug != .)) | all)'
```

### Amazon S3
Extracting data usage
`aws s3 ls s3://moocfetcher-course-archive --recursive  | grep -v -E "(Bucket: |Prefix: |LastWriteTime|^$|--)" | awk 'BEGIN {total=0}{total+=$3}END{print total/1024/1024" MB"}'`

### Coursera API
* Course catalog: <https://www.coursera.org/api/opencourse.v1/course/security>
* Information about On Demand Course: <https://www.coursera.org/api/onDemandCourses.v1?q=slug&slug=security>
* More information about On Demand Courses (found in coursera-dl codebase): 

```
https://www.coursera.org/api/onDemandCourseMaterials.v1/?q=slug&slug=security&includes=moduleIds,lessonIds,passableItemGroups,passableItemGroupChoices,passableLessonElements,itemIds,tracks&fields=moduleIds,onDemandCourseMaterialModules.v1(name,slug,description,timeCommitment,lessonIds,optional),onDemandCourseMaterialLessons.v1(name,slug,timeCommitment,elementIds,optional,trackId),onDemandCourseMaterialPassableItemGroups.v1(requiredPassedCount,passableItemGroupChoiceIds,trackId),onDemandCourseMaterialPassableItemGroupChoices.v1(name,description,itemIds),onDemandCourseMaterialPassableLessonElements.v1(gradingWeight),onDemandCourseMaterialItems.v1(name,slug,timeCommitment,content,isLocked,lockableByItem,itemLockedReasonCode,trackId),onDemandCourseMaterialTracks.v1(passablesCount)&showLockedItems=true
```

## 25 Jun 2016
Checking for english language courses
```
    cat launched.json | jq -r  '.courses[] | select(.primaryLanguageCodes) |
select(.primaryLanguageCodes | map(. == "en") | any) |  .slug' | sort
```

## 27 Jun 2016
### Ruby on OS X
System Ruby on OS X is a pain to manage. Use `rbenv` and `ruby-build` (which is a plugin for `rbenv`)

```
rbenv install 2.3.1
rbenv global 2.3.1
```
and then to set the ruby version in the terminal shell:
```
eval "$(rbenv init -)"
```

## 28 Jun 2016
### Golang
* Setting up Golang on terminal
```
export GOPATH=~/.gopath
export PATH=$GOPATH/bin:$PATH
open -a MacVim
```
* JSON Decoding: Make sure your receiver type/struct has public members

### Vim tips
Use `system` to run system commands
```
:echo system("goimports --help")
:echo system("which goimports")
```

## 11 Jul 2016
### Tmux
* Renumbering windows in Tmux: `move-window -r`:
<http://unix.stackexchange.com/questions/21742/renumbering-windows-in-tmux>

## 26 Jul 2016
### React
* Shallow rendering: <https://facebook.github.io/react/docs/test-utils.html#shallow-rendering>
	* Helpers for shallow rendering: <https://github.com/glenjamin/skin-deep>
	* Better assertions for shallow rendering: <https://jamesfriend.com.au/better-assertions-shallow-rendered-react-components>
	* <http://willcodefor.beer/react-testing-with-shallow-rendering-and-skin-deep/>
	* Shallow render lifecycle methods: <https://gist.github.com/jondlm/514405bea50fad6fd905>

## 28 Jul 2016
### React
* React Patterns: <https://github.com/krasimir/react-in-patterns>

### React and Go
* go-starter-kit: <https://github.com/olebedev/go-starter-kit>

### Nginx
* proxy_pass
* Using an API along with serving static files: <http://stackoverflow.com/questions/15137661/configuring-nginx-to-serve-static-files-and-proxy-pass-certain-urls>

### Golang Webapp frameworks
* Keep it simple if possible: <https://golang.org/doc/articles/wiki/>
* Otherwise, some useful libraries are:
    * Echo
    * Gorilla Mux Router

## 2 Aug 2016

### React and Polling
* A wrapped component that implements polling: <https://github.com/cameronbourke/react-async-poll>

### Golang HTTP handling
* HTTP Response snippets: <http://www.alexedwards.net/blog/golang-response-snippets#json>

### Tmux reorder windows
* `swap-window -t <x>` will swap current window with window <x>

### Golang CORS
* Handler to do CORS: <https://github.com/rs/cors>

```
package main

import (
    "net/http"

    "github.com/rs/cors"
)

func main() {
    mux := http.NewServeMux()
    mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
        w.Header().Set("Content-Type", "application/json")
        w.Write([]byte("{\"hello\": \"world\"}"))
    })

    // cors.Default() setup the middleware with default options being
    // all origins accepted with simple methods (GET, POST). See
    // documentation below for more options.
    handler := cors.Default().Handler(mux)
    http.ListenAndServe(":8080", handler)
}
```

### Animated Button using Bootstrap
* <http://www.bootply.com/128062>

## 18 Aug

### Golang API Testing
* <https://github.com/appleboy/gofight>
* <http://modocache.io/restful-go>
* For integration testing: <https://github.com/emicklei/forest>

## 21 Aug

### CSS Masonry Layout
* <https://codepen.io/dudleystorey/pen/eAqzk>

## 30 Aug

### Serve static file and API simultaneously with Go
* <https://elithrar.github.io/article/vue-react-ember-server-golang/>

## 8 Sept
### Minimal Web Server
```
 while true ; do  echo -e "HTTP/1.1 200 OK\n\n $(date)" | nc -l -p 1500  ; done
```

### Raspberry Pi Wifi Setup commandline
* <https://www.raspberrypi.org/documentation/configuration/wireless/wireless-cli.md>
* [Automatically connect a Raspberry Pi to a Wifi network](http://weworkweplay.com/play/automatically-connect-a-raspberry-pi-to-a-wifi-network/)
## 9 Sept
### USB Tethering
* Assign a static IP to host during USB tethering:
  *  <http://www.monblocnotes.com/node/1895> 
    * (Note: NetworkManager is not part of Rasbian, so ignore those instructions)
  * <https://www.raspberrypi.org/forums/viewtopic.php?f=36&t=47522> 
    * Following these instructions and then using `hostname -I` gives two IP addresses for the `usb0` interface, and both of them seem to work. It appears that one of them is assigned dynamically _after_ the static IP is configured. Not sure what exactly is happening.
### Raspberry Pi and Macbook direct network connection
* Lots of conflicting and confusing information online.
* Best working instructions: <http://kmahelona.blogspot.in/2013/04/share-your-internet-from-your-macbook.html>
	* Setup ethernet and assign a static IP 192.168.2.1 to Macbook, along with a public DNS like 8.8.8.8 and 8.8.4.4
	* Setup Raspberry Pi eth0 interface and assign static IP 192.168.2.2 to:
		
```
auto eth0
iface eth0 inet static
address 192.168.2.2
netmask 255.255.255.0
gateway 192.168.2.1
```
Turn on internet sharing on Mac, and reboot Raspberry Pi. After that you should be able to SSH into `raspberrypi.local` and also use the internet on the Pi.
### Auto-mounting USB drives on Raspberry Pi
It seems that USB drives do not automount on the Raspberry Pi by default. Here are the steps I needed:

* Install `usbmount`: `sudo apt-get install usbmount`
	* According to the original, page `usbmount` is unmaintained, but there is a repo on Github with updated code: <https://github.com/rbrito/usbmount>
	* It seems that `usbmount` basically works by adding some rules for `udevd` which are triggered everytime a USB device is added/removed
* By default USB Mount will mount the disks read-only. Change that by changing FS_MOUNTOPTIONS to something like `-fstype=vfat,umask=0000`. More details:
	* <http://raspberrypi.stackexchange.com/questions/29688/samba-and-usbmount-cant-write-on-automounted-device>
	* <http://unix.stackexchange.com/questions/39921/device-is-mounting-as-read-only-can-copy-files-as-root>
	* <http://raspberrypi.stackexchange.com/questions/41959/automount-various-usb-stick-file-systems-on-jessie-lite>
### Disabling Wifi and Bluetooth on Raspberry Pi
* <https://www.raspberrypi.org/forums/viewtopic.php?f=63&t=138610>
## 13 Sept
### Bash shell and history
* <http://samrowe.com/wordpress/advancing-in-the-bash-shell/>

## 16 Sep
### Lua simple grepping example using patterns
```
local pattern = "%w+%.%w+%("

for line in io.lines("src/bidi_bracket.lua") do
  if string.find(line,pattern) then print(line) end
end
```

_needed this recently to search for something…_
## 17 Sep
### Lua hex string to number conversion
```
local someHexString = "03FFACB"
local someNumber = tonumber(someHexString, 16)
```
## 19 Sep
### Go chaining handlers
* Alice: <https://github.com/justinas/alice>
	* Simple, minimalist way to chain handlers
## 20 Sep
### startup scripts with systemd
* <https://gauntface.com/blog/2015/12/02/start-up-scripts-for-raspbian>
* <https://wiki.archlinux.org/index.php/Systemd>
* Creating Custom Target with systemd:<http://unix.stackexchange.com/questions/301987/how-to-create-a-systemd-target>
### logging with systemd
* Example from influxdb: <https://github.com/influxdata/influxdb/issues/4490>

```
[Unit]
Description=InfluxDB is an open-source, distributed, time series database
Documentation=https://influxdb.com/docs/
After=network.target

[Service]
User=influxdb
Group=influxdb
LimitNOFILE=65536
Environment='STDOUT=/dev/null'
Environment='STDERR=/var/log/influxdb/influxd.log'
EnvironmentFile=-/etc/default/influxdb
ExecStart=/bin/sh -c "/opt/influxdb/influxd -config /etc/opt/influxdb/influxdb.conf ${INFLUXD_OPTS} > ${STDOUT} 2> ${STDERR}"
KillMode=control-group
Restart=on-failure

[Install]
WantedBy=multi-user.target
Alias=influxd.service
```

## 21 September
### React Native
* Curated List of Talks: <https://github.com/mightyCrow/awesome-react-native-talks>
### Automounting NTFS Disk on Raspberry Pi
* Latest Jessie seems to have in-built support for NTFS
* Finding UUID of the disk: `sudo blkid -o list`
* Adding a fstab entry: e.g: `UUID=xxxx /media/USBDRIVE ntfs-3g auto,users,permissions 0 0`
### usbmount bug
* https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=774149
* Basically because: _“You cannot start long-lived processes from a udev rule, this should be handled by systemd.”_
### Seeing all files installed by a Debian package
To see all the files the package installed onto your system, do this:
```
dpkg-query -L <package_name>
```

## 25 Sep
### Multiple GOPATHs
* `go help gopath` contains details about multiple gopaths, internal packages etc.
## 26 Sep
* Go Build Essential: <http://golang.rakyll.org/go-tool-flags/>
* `go build -x`