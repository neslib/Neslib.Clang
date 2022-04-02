# Neslib.Clang - libclang for Delphi

Libclang is the C Interface to Clang's C family of compilers. It provides a  relatively small API that exposes facilities for parsing source code into an  abstract syntax tree (AST), loading already-parsed ASTs, traversing the AST,  associating physical source locations with elements within the AST, and other  facilities that support Clang-based development tools.  

This C interface to Clang will never provide all of the information  representation stored in Clang's C++ AST, nor should it: the intent is to  maintain an API that is relatively stable from one release to the next,  providing only the basic functionality needed to support development tools.

You can find an article about *libclang for Delphi* on [my blog](https://blog.grijjy.com/2018/05/15/libclang-for-delphi/).

Neslib.Clang leverages libclang in two ways:  

The unit `Neslib.Clang.Api` providers the header translations for libclang  version 14.0. You can use these header translations as you would use libclang  from a C language. The documentation for the C API can be found here: <https://clang.llvm.org/doxygen/index.html>   

The unit `Neslib.Clang` provides a higher level abstraction of the libclang  API. It provides a thin class model on top of the libclang API, making it easier  to use from Delphi. In addition, it provides automatic memory management so you  don't have to worry about what resources to release and when.  

Documentation for the `Neslib.Clang` unit can be found in the Neslib.Clang.Chm file or on-line at https://neslib.github.io/Neslib.Clang.

## 32-bit and 64-bit versions

Because the Clang DLLs are very big (64+ MB), Neslib.Clang only ships with the 32-bit versions of these DLLs. If you prefer to use the 64-bit version, then you will have to download the 64-bit version of LLVM from the [LLVM Download Page](https://releases.llvm.org/) (be sure to pick the version that matches this Neslib.Clang version).

After installation, you will find `libclang.dll` in the bin directory. You also need to copy the VC runtime DLLs (`msvcp140.dll` and `vcruntime140.dll`) from that directory.

## License

Neslib.Clang is licensed under the Simplified BSD License. See License.txt for details.

