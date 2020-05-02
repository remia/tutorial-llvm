Cpp
---

Access to the REPL

.. code-block::

    c++ -g -O3 toy.cpp -o toy `llvm-config --cxxflags --ldflags --libs --libfiles --system-libs` -std=c++14 && ./toy

Compile to object file, then execute

.. code-block::

    c++ -g -O3 toy.cpp -o toy `llvm-config --cxxflags --ldflags --libs --libfiles --system-libs` -std=c++14 && ./toy < mandlebrot.txt
    c++ main.cpp output.o -o main
    ./main

Ocaml
-----

brew install ocaml opam llvm
# For IDE syntax highlight / autocomplete
opam install merlin ocp-indent
opam install ocamlbuild camlp5 llvm ctypes-foreign

ocamlbuild -pkgs llvm,llvm.analysis toy.byte
