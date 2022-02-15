@REM FTN95 needs a special batch file to compile if you need to increase the 
@REM stack size. The only way to do that is to compile and link seperately,
@REM specifying the stack size with slink. Since pbharness accepts one command
@REM for the compile and link this batch file will do both in one.
@REM %1 is the name (with no extension) of the file to be compiled

ftn95 %1 %2 %3 %4 %5 %6 %7 %8 %9
slink %1.obj /stack:0x10000000