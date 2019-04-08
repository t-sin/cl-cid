# cl-cid - CID library

## Requirements

- [cmap-resources](https://github.com/adobe-type-tools/cmap-resources)

## Installation

Locate this repository in your ASDF path or type `ros install t-sin/cl-cid` if you use [Roswell](https://github.com/roswell/roswell).

## Usage

First, you should load cmap from file. `list-cmap` tells what cmap you can load.

```lisp
CL-USER> (cl-cid:list-cmap)
("CNS1" "GB1" "Identity" "Japan1" "KR" "Korea1" "deprecated")
```

`load-cmap` loads cmap that has specified name into memory.

```lisp
CL-USER> (cl-cid:load-cmap "Japan1")
23060  ; number of rows in cmap "Japan1"
```

`code-cid` maps specified character code to CID.

```lisp
CL-USER> (cl-cid:code-cid (char-code #\茸))
10503  ; CID for '茸'

CL-USER> (eq (char-code #\茸)
             (cid:cid-code (cid:code-cid (char-code #\茸))))
T
```

## Author

- TANAKA Shinichi (<shinichi.tanaka45@gmail.com>)

## License

*cl-cid* is licensed under the Lisp GNU Lesser General Public License. See [COPYING](COPYING).
