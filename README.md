## interfacelift-dl

A Clojure library designed to download wallpapers from interfacelift.com

<!-- Usage: IFL-DL [options] -->

### Options:
```
      --res RES        1920x1080  Resolution
  -d, --folder FOLDER  resources  Download folder
  -r, --range RANGE    (1)        Pages to download
  -h, --help                      Print help page
```
### Example:
```
  clj -M:main --res 2880x1800 -r "(range 10)"
  clj -M:main --res 2880x1800 -r "'(1 2 3)"
```
