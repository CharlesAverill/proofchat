<!---
This file was generated from `meta.yml`, please do not edit manually.
Follow the instructions on https://github.com/coq-community/templates to regenerate.
--->
# ProofChat

[![Docker CI][docker-action-shield]][docker-action-link]
[![coqdoc][coqdoc-shield]][coqdoc-link]

[docker-action-shield]: https://github.com/CharlesAverill/proofchat/actions/workflows/docker-action.yml/badge.svg?branch=master
[docker-action-link]: https://github.com/CharlesAverill/proofchat/actions/workflows/docker-action.yml


[coqdoc-shield]: https://img.shields.io/badge/docs-coqdoc-blue.svg
[coqdoc-link]: https://CharlesAverill.github.io/proofchat/docs/latest/coqdoc/toc.html


This project contains a verified TCP client/server chat application written in
Coq, extracted to OCaml. All networking is accomplished through
the builtin OCaml Unix library.

## Meta

- Author(s):
  - Charles Averill [<img src="https://zenodo.org/static/images/orcid.svg" height="14px" alt="ORCID logo" />](https://orcid.org/0000-0001-6614-1808) (initial)
- License: [MIT License](LICENSE)
- Compatible Coq versions: 8.19.1 or later
- Additional dependencies: none
- Related publication(s): none

## Building and installation instructions

The easiest way to install the latest released version of ProofChat
is via [OPAM](https://opam.ocaml.org/doc/Install.html):

```shell
opam repo add coq-released https://coq.inria.fr/opam/released
opam install coq-proofchat
```

To instead build and install manually, do:

``` shell
git clone https://github.com/CharlesAverill/proofchat.git
cd proofchat
make   # or make -j <number-of-cores-on-your-machine> 
make install
```


## To extract

Simply run `make` in the root directory to compile the contents of [theories](theories).

The extracted contents are placed in [proofchat](proofchat).
Here, run `dune build` to compile OCaml binaries for the client and server.

The client and server can be started via `dune exec proofchat.client`
and `dune exec proofchat.server`, respectively.

## Verification

To be approached. I'm mostly going to target the involution of my serialization
and deserialization routines.
