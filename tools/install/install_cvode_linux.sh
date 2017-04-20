#!/bin/sh

# This file downloads and installs CVODE 2.9.0 into the directory given by input
# argument $1. This is dependent on the exisence of a gcc installation.
# Edit FORT_COMP to point to an existing gfortran or ifort executable.
# Example usage:
#   ./install_cvode_linux.sh /path/to/install/directory
FORT_COMP=/usr/bin/gfortran

cd $1
wget http://computation.llnl.gov/projects/sundials/download/cvode-2.9.0.tar.gz

tar -zxf cvode-2.9.0.tar.gz
cd cvode-2.9.0/
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=$1/cvode \
    -DCMAKE_C_COMPILER:FILEPATH=gcc \
    -DCMAKE_Fortran_COMPILER=$FORT_COMP \
    -DLAPACK_ENABLE:BOOL=ON \
    -DLAPACK_LIBRARIES=/usr/lib/liblapack.so:/usr/lib/libblas.so \
    -DFCMIX_ENABLE:BOOL=ON \
    ..
make -j8
make install