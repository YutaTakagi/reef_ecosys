#!/bin/sh
#
rm *.exe
#
SRC_DIR=../../src
INCLUDE="-I${PWD}"
gfortran -fbounds-check -ffree-form ${SRC_DIR}/mod_calendar.f90 ${SRC_DIR}/mod_geochem.F ${SRC_DIR}/mod_foodweb.F ${SRC_DIR}/mod_param.F ${SRC_DIR}/mod_reef_flow.F ${SRC_DIR}/mod_heat.F ${SRC_DIR}/mod_coral.F ${SRC_DIR}/mod_macroalgae.F ${SRC_DIR}/mod_seagrass.F ${SRC_DIR}/mod_sedecosys.F ${SRC_DIR}/mod_reef_ecosys.F ${SRC_DIR}/mod_input.F ${SRC_DIR}/mod_output.F ${SRC_DIR}/main.F -O2 ${INCLUDE} -I/usr/include -L/usr/lib -lnetcdff -o ecosys_test.exe
rm *.mod
#
mkdir -p output
#
#./ecosys_test.exe < coral_01.in
./ecosys_test.exe < coral_bl_T25.in
./ecosys_test.exe < coral_bl_T33.in
./ecosys_test.exe < coral_bl_T14.in
#
