## IKOS - API ##

IKOS-API provides a Python and OCaml API for [IKOS: Inference Kernel for Open Static Analyzers](http://ti.arc.nasa.gov/opensource/ikos/).

IKOS is a C++ library designed to facilitate the development of sound static analyzers based on Abstract Interpretation. IKOS-API provides an easy to use API in Python and OCaml. 

## Getting started ##

[http://ikos-api.bitbucket.org](http://ikos-api.bitbucket.org)

## Contents ##

* PyIkos :: IKOS API in Python
* OIkos  :: IKOS API in OCaml

## Dependencies ##

- IKOS: **boost**, **gmp**, **pthread**
- OIkos: IKOS, **ocaml**, **ocamlbuild**
- lustrec: OIkos, **ocamlgraph** : http://ocamlgraph.lri.fr
- PyIkos: IKOS, **python** (2 or 3), **boost_python**

## Build ##

You will need **python** for the build system, _waf_.

run `./waf configure` and then, `./waf build` to build all.

You can compile only OIkos or PyIkos using the options `--no-ocaml` or `--no-python`.
You can also choose a specific version of python, by specifying the path to the python interpreter : `./waf configure --python=/bin/python3`.

## License ##

* [IKOS](http://ti.arc.nasa.gov/opensource/ikos/) is released under the [NASA Open Source Agreement](http://ti.arc.nasa.gov/m/opensource/downloads/ikos/IKOS_NASA_Open_Source_Agreement.pdf).
* IKOS-API is released under the MIT License.
--------
The MIT License (MIT)

Copyright (c) 2014 Carnegie Mellon University

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

------
