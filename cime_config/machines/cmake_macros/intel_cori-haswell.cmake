string(APPEND FFLAGS " -fp-model consistent -fimf-use-svml")
if (NOT DEBUG)
  string(APPEND FFLAGS " -qno-opt-dynamic-align")
endif()
string(APPEND CXXFLAGS " -fp-model consistent")
set(PETSC_PATH "$ENV{PETSC_DIR}")
set(SCC "icc")
set(SCXX "icpc")
set(SFC "ifort")
string(APPEND SLIBS " -L$ENV{NETCDF_DIR} -lnetcdff -Wl,--as-needed,-L$ENV{NETCDF_DIR}/lib -lnetcdff -lnetcdf")
string(APPEND SLIBS " -mkl -lpthread")
if (NOT MPILIB STREQUAL mpi-serial)
  string(APPEND SLIBS " -L$ENV{ADIOS2_DIR}/lib64 -ladios2_c_mpi -ladios2_c -ladios2_core_mpi -ladios2_core")
endif()