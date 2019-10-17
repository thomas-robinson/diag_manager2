FTN=mpiifort
CC=mpiicc
MPFTN=mpiifort
FFLAGS=#-no-wrap-margin -g -traceback 
INCLUDE=-Ilibyaml/include -I. -I/opt/intel/2019_up5/impi/2019.5.281/intel64/include
CFLAGS=#-g -traceback
LIB=-Llibyaml/src/.libs -L/opt/intel/2019_up5/impi/2019.5.281/intel64/lib
l=-lyaml

GFTN=gfortran
GCC=gcc
GINCLUDE=
GLIB=

fms_diag_manager_test: fms_diag_manager2.o fms_diag_concur.o fms_diag_data.o 
	$(MPFTN) $(INCLUDE) $(LIB) $(FFLAFS) $^ test/diag_manager2/diag_manager_test1.F90 -o $@
	mpirun -n 6 ./$@ : -n 7 ./$@
	mpirun -n 6 ./$@
	mpirun -n 10 ./$@
	mpirun -n 5 ./$@ : -n 6 ./$@
	mpirun -n 10 ./$@ : -n 7 ./$@
	mpirun -n 6 ./$@ : -n 6 ./$@
	mpirun -n 6 ./$@ : -n 6 ./$@ : -n 3 ./$@ : -n 6 ./$@ : -n 1 ./$@
	rm $@
	$(MPFTN) $(FFLAFS) $^ test/diag_manager2/diag_manager_test2.F90 -o $@
	./$@
	make clean
diag_table_test: diag_table_test.x
#	make clean
	./$< 
	make clean
parser_test: parser.x
#	make clean
	./$< diag_yaml
	make clean
diag_table_test.x: fms_diag_parse.o fms_diag_table.o 
	$(FTN) $(INCLUDE) $(LIB) $(l) $(FFLAFS) $^  test/diag_table_test.F90 -o $@
fms_diag_manager2.o:fms_diag_table.o fms_diag_data.o fms_diag_concur.o
	$(MPFTN) $(INCLUDE) $(LIB) $(l) $(FFLAFS) $^ fms_diag_manager2.F90 -c 
fms_diag_table.o: fms_diag_parse.o fms_diag_data.o
	$(FTN) $(INCLUDE) $(LIB) $(l) $(FFLAFS) $^ fms_diag_table.F90 -c 
fms_diag_data.o: 
	$(FTN) $(INCLUDE) $(LIB) $(l) $(FFLAFS) $^ fms_diag_data.F90 -c 
fms_diag_parse.o:
	$(CC)  $(INCLUDE) $(LIB) $(l) $(CFLAGS) $^ fms_diag_parse.c -c 
fms_diag_concur.o: fms_diag_data.o
	$(MPFTN) $(INCLUDE) $(LIB) $(l) $(FFLAFS) $^ fms_diag_concur.F90 -c
parser.x: fms_diag_parse.o
	$(CC)  $(INCLUDE) $(LIB) $(l) -dynamic $(CFLAGS) $^ parser.c -o $@
concur.x: fms_diag_concur.o fms_diag_data.o
	$(MPFTN) $(FFLAFS) $^ test/concur/fms_diag_concur_test.F90 -o $@
	mpirun -n 6 ./$@ : -n 7 ./$@
	mpirun -n 6 ./$@
	mpirun -n 5 ./$@ : -n 6 ./$@
	mpirun -n 10 ./$@ : -n 7 ./$@
	mpirun -n 6 ./$@ : -n 6 ./$@
	mpirun -n 6 ./$@ : -n 6 ./$@ : -n 3 ./$@ : -n 6 ./$@ : -n 1 ./$@

clean:
	rm -rf *.x *.o *.mod *.swo
