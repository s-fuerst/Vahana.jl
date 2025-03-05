{ pkgs ? import <nixpkgs> {} }:

let
  libraries = with pkgs; [
    hdf5-mpi
    mpi
  ];
  libraryPath = pkgs.lib.makeLibraryPath libraries;
in pkgs.mkShell {
  buildInputs = with pkgs; [
    h5utils
  ] ++ libraries;
  
  shellHook = ''
    export LD_LIBRARY_PATH=${libraryPath}:$LD_LIBRARY_PATH
    export OMPI_MCA_osc="^ucx"

    julia --project -e 'using MPIPreferences; MPIPreferences.use_system_binary()'
    echo "HDF5 (non parallel) environment activated"
  '';
}
