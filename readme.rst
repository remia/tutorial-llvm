Chapter 2
---------

c++ -g -O3 toy.cpp `llvm-config --cxxflags --ldflags --libs --libfiles --system-libs` -std=c++14

Chapter 3
---------

brew install ocaml opam llvm
# For IDE syntax highlight / autocomplete
opam install merlin ocp-indent
opam install ocamlbuild camlp5 llvm

ocamlbuild -pkgs llvm,llvm.analysis toy.byte

Chapter 4
---------

opam install ctypes-foreign
