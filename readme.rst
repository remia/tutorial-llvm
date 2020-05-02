Cpp
---

Compilation

.. code-block::

    clang++ -g -O3 toy.cpp -o toy `llvm-config --cxxflags --ldflags --libs --libfiles --system-libs` -std=c++14

Access to the REPL

.. code-block::

    ./toy

Emit LLVM IR to stderr

.. code-block::

    ./toy -ir < fib.ks

Execute LLVM IR (result is the return code of second exec)

.. code-block::

    ./toy -ir < fib.ks |& clang -x ir - -o fib
    ./fib

Compile to object file, then execute from external program

.. code-block::

    ./toy -o < mandlebrot.ks
    clang++ main.cpp output.o -o main
    ./main

Ocaml
-----

.. code-block::

    brew install ocaml opam llvm
    # For IDE syntax highlight / autocomplete
    opam install merlin ocp-indent
    opam install ocamlbuild camlp5 llvm ctypes-foreign

    ocamlbuild -pkgs llvm,llvm.analysis toy.byte
