#include <stdbool.h>


bool even (int n) {

  if (n < 0)
    /* no unary expressions available ...*/
    n = 0 - n;

  while (n > 1) {
    n = n - 2;
  }
  return (n == 0) ;
}
