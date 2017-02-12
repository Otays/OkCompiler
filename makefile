CC = gcc
CFLAGS = -I. 
LEX=lex

scanner: lex.yy.o driver.o
	gcc $(CFLAGS) -o scanner driver.o lex.yy.o -ll

lex.yy.o: lex.yy.c
	gcc $(CFLAGS) -c lex.yy.c

lex.yy.c: scanner.l
	lex scanner.l

driver.o: driver.c
	$(CC)  $(CFLAGS) -c driver.c 
clean:
	rm -f lex.yy.* *.o *~ scanner     



