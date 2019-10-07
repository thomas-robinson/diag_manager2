FTN=ftn 
CC=cc
FFLAGS=-no-wrap-margin 
INCLUDE=-I../libyaml/include -I. 
CFLAGS=
LIB=-L../libyaml/src/.libs
l=-lyaml

diag_table_test: diag_table_test.x
	./$<
	make clean
parser_test: parser.x
	./$< diag_yaml
	make clean
diag_table_test.x: fms_diag_table.o fms_diag_parser.o
	$(FTN) $(INCLUDE) $(LIB) $(l) $(FFLAFS) $^    test/diag_table_test.F90 -o $@
fms_diag_table.o: fms_diag_parser.o
	$(FTN) $(INCLUDE) $(LIB) $(l) $(FFLAFS) $^ -c fms_diag_table.F90
fms_diag_parser.o:
	$(CC)  $(INCLUDE) $(LIB) $(l) $(CFLAGS) $^ -c fms_diag_parser.c
parser.x: fms_diag_parser.o
	$(CC)  $(INCLUDE) $(LIB) $(l) -dynamic $(CFLAGS) $^ parser.c -o $@
clean:
	rm -rf diag_table_test.x *.o *.mod *.swo
