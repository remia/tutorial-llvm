Cpp
---

c++ -g -O3 toy.cpp `llvm-config --cxxflags --ldflags --libs --libfiles --system-libs` -std=c++14
./a.out

Ocaml
-----

brew install ocaml opam llvm
# For IDE syntax highlight / autocomplete
opam install merlin ocp-indent
opam install ocamlbuild camlp5 llvm ctypes-foreign

ocamlbuild -pkgs llvm,llvm.analysis toy.byte
