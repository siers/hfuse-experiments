{ mkDerivation, base, bytestring, HFuse, lib, unix, text, directory-tree }:
mkDerivation {
  pname = "hfuse-experiments";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base bytestring HFuse unix text directory-tree ];
  homepage = "https://github.com/siers/hfuse-experiments";
  license = lib.licenses.bsd3;
  mainProgram = "MemoryFS";
}
