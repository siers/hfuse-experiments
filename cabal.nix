{ mkDerivation, base, bytestring, containers, HFuse, lib, unix, text, path, directory-tree, recursion-schemes, pretty-simple }:
mkDerivation {
  pname = "hfuse-experiments";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base bytestring containers HFuse unix text path directory-tree recursion-schemes pretty-simple ];
  homepage = "https://github.com/siers/hfuse-experiments";
  license = lib.licenses.bsd3;
  mainProgram = "MemoryFS";
}
