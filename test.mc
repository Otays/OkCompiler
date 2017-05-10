/** COPY RIGHT --- 1991 SOFTWARE INC **/
int x;

int main() { 
  int y;
  x = 7;
  y = 4;
	
  return fib(x) + fib(y * 3);  // 13 + 144
}

int fib(int x) { 
  int y;
  y = 1337*1337;
  
  if (x == 1) { return x; }
  if (x == 0) { return x; } else { return fib(x-1) + fib(x-2); }

}







