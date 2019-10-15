FTN=ifort 
CC=icc
FFLAGS=-no-wrap-margin -g -traceback 
INCLUDE=-Ilibyaml/include -I. 
CFLAGS=-g -traceback
LIB=-Llibyaml/src/.libs
l=-lyaml

GFTN=gfortran
GCC=gcc
GINCLUDE=
GLIB=

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
fms_diag_table.o: fms_diag_parse.o fms_diag_data.o
	$(FTN) $(INCLUDE) $(LIB) $(l) $(FFLAFS) $^ fms_diag_table.F90 -c 
fms_diag_data.o: 
	$(FTN) $(INCLUDE) $(LIB) $(l) $(FFLAFS) $^ fms_diag_data.F90 -c 
fms_diag_parse.o:
	$(CC)  $(INCLUDE) $(LIB) $(l) $(CFLAGS) $^ fms_diag_parse.c -c 
parser.x: fms_diag_parse.o
	$(CC)  $(INCLUDE) $(LIB) $(l) -dynamic $(CFLAGS) $^ parser.c -o $@
clean:
	rm -rf *.x *.o *.mod *.swo
