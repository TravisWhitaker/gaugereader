with import <nixpkgs> {};

runCommand "gaugereader-env" rec
{
    libs = [ git
             llvm_5
             libffi
             ffmpeg-full
             zlib
           ] ++ (if stdenv.isDarwin
                 then []
                 else [ cudatoolkit8.lib
                        cudatoolkit8.out
                        linuxPackages.nvidia_x11
                      ]
                );

    buildInputs = [ gcc
                    haskell.compiler.ghc822
                    stack
                    pkgconfig
                  ] ++ libs;

    LD_LIBRARY_PATH = stdenv.lib.makeLibraryPath libs;
} ""
