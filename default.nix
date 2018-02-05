with import <nixpkgs> {};

haskell.lib.buildStackProject rec
{
    name = "gaugereader";

    ghc = haskell.compiler.ghc822;

    libs = [ git
             llvm_5
             libffi
             ffmpeg-full
             zlib
           ] ++ (if stdenv.isDarwin
                 then [ darwin.cf-private
                        darwin.apple_sdk.frameworks.CoreFoundation
                        darwin.apple_sdk.frameworks.Kernel
                        darwin.apple_sdk.frameworks.Cocoa
                      ]
                 else [ cudatoolkit8.lib
                        cudatoolkit8.out
                        linuxPackages.nvidia_x11
                      ]
                );

    buildInputs = [ clang
                    gcc
                    haskell.compiler.ghc822
                    stack
                    pkgconfig
                    which
                  ] ++ libs;
}
