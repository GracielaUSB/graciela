/* auxiliarFunctions
$ gcc -fPIC -shared auxiliarFunctions.c -o auxiliarFunctions.so
$ clang -fPIC -shared auxiliarFunctions.c -o auxiliarFunctions.so
*/

#include "stdio.h"

// putchard - putchar that takes a double and returns 0.
void writeLnInt(int x) {
    printf("%d\n", x);
    return;
}


void writeLnDouble(double x) {

    printf("%f\n", x);
    return;

}


void writeLnBool(int x) {

    if (x == 1) 
    	printf("%s\n", "true");
    else 
    	printf("%s\n", "false");

    return;
}
