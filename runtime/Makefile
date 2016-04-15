CC = $(shell if [ `uname` = "CYGWIN_NT-6.1-WOW" ] ; then echo 'x86_64-pc-cygwin-gcc' ; else echo 'gcc' ; fi)

FLAGS = -Wall -Werror -c -std=c99 -m64

goto.o : goto.c goto.h
	$(CC) $(FLAGS) goto.c

clean :
	rm -f *.o