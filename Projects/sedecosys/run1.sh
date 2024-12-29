#!/bin/sh
#
rm *.exe
#
SRC_DIR=../../src
INCLUDE="-I${PWD}"
#FFLAGS = -O0 -Wall -g -fcheck=array-temps,bounds,do,mem,pointer,recursion -ffpe-trap=invalid,zero,overflow,underflow
#FFLAGS = -fbounds-check -ffree-form
#FFLAGS = -fbounds-check -ffree-form
#gfortran -fbounds-check -ffree-form ${SRC_DIR}/mod_geochem.F ${SRC_DIR}/mod_sedecosys_test6.F ${SRC_DIR}/mod_output.F ${SRC_DIR}/main_sedtest.F -O2 ${INCLUDE} -I/opt/homebrew/Cellar/netcdf-fortran/4.6.1/include -I/opt/homebrew/Cellar/netcdf-fortran/4.6.1/include -L/opt/homebrew/Cellar/netcdf-fortran/4.6.1/lib -lnetcdff -o ecosys_test.exe
gfortran -fbounds-check -ffree-form -O0 -g -fcheck=array-temps,bounds,do,mem,pointer,recursion ${SRC_DIR}/mod_calendar.f90 ${SRC_DIR}/mod_geochem.F ${SRC_DIR}/mod_foodweb.F ${SRC_DIR}/mod_param.F ${SRC_DIR}/mod_reef_flow.F ${SRC_DIR}/mod_heat.F ${SRC_DIR}/mod_coral.F ${SRC_DIR}/mod_macroalgae.F ${SRC_DIR}/mod_seagrass.F ${SRC_DIR}/mod_sedecosys.F ${SRC_DIR}/mod_reef_ecosys.F ${SRC_DIR}/mod_input.F ${SRC_DIR}/mod_output.F ${SRC_DIR}/main.F -O2 ${INCLUDE} -I/usr/include -L/usr/lib -lnetcdff -o ecosys_test.exe
# gfortran -fbounds-check -ffree-form ${SRC_DIR}/mod_calendar.f90 ${SRC_DIR}/mod_geochem.F ${SRC_DIR}/mod_param.F ${SRC_DIR}/mod_reef_flow.F ${SRC_DIR}/mod_heat.F ${SRC_DIR}/mod_foodweb.F ${SRC_DIR}/mod_coral.F ${SRC_DIR}/mod_macroalgae.F ${SRC_DIR}/mod_seagrass.F ${SRC_DIR}/mod_sedecosys_MM4.F ${SRC_DIR}/mod_reef_ecosys_MM1.F ${SRC_DIR}/mod_input.F ${SRC_DIR}/mod_output.F ${SRC_DIR}/main.F -O2 ${INCLUDE} -I/opt/homebrew/Cellar/netcdf-fortran/4.6.1/include -I/opt/homebrew/Cellar/netcdf-fortran/4.6.1/include -L/opt/homebrew/Cellar/netcdf-fortran/4.6.1/lib -lnetcdff -o ecosys_test.exe
rm *.mod
#
# mkdir -p output
#
./ecosys_test.exe < sedecosys.in
# ./ecosys_test.exe
#
