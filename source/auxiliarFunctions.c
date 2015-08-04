/* auxiliarFunctions
$ gcc -fPIC -shared auxiliarFunctions.c -o auxiliarFunctions.so
$ clang -fPIC -shared auxiliarFunctions.c -o auxiliarFunctions.so
*/

#include "stdio.h"

// putchard - putchar that takes a double and returns 0.
void writeLnInt(int X) {
  printf("%d\n", X);
  return;
}
