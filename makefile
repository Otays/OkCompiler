GRAMMAR = parser.y
CC = gcc
CFLAGS = -w -I. 
YFLAGS = -v -d 
YACC = yacc # bison

mcc: y.tab.o lex.yy.o driver.o tree.o colorlogs.o
	gcc $(CFLAGS) -o mcc driver.o y.tab.o lex.yy.o tree.o colorlogs.o -ll

y.tab.o: y.tab.c
	gcc $(CFLAGS) -c y.tab.c 

y.tab.c: $(GRAMMAR)
	$(YACC) $(YFLAGS) $(GRAMMAR)

lex.yy.o: lex.yy.c
	gcc $(CFLAGS) -c lex.yy.c

lex.yy.c: scanner.l
	lex scanner.l

tree.o: tree.c tree.h
	gcc $(CFLAGS) -c tree.c
	
colorlogs.o: colorlogs.c
	gcc  $(CFLAGS) -c colorlogs.c
	
driver.o: driver.c
	$(CC)  $(CFLAGS) -c driver.c 
	 

clean:
	rm -f y.tab.* y.output lex.yy.* *.o *~ mcc     
run:
	./mcc < test.mc


