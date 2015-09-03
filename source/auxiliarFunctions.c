/* auxiliarFunctions
$ gcc -fPIC -shared auxiliarFunctions.c -o auxiliarFunctions.so
$ clang -fPIC -shared auxiliarFunctions.c -o auxiliarFunctions.so
*/

#include "stdio.h"
#include "stdlib.h"



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


void writeLnString(char *x) {

    printf("%s\n",x);
    return;

}


void writeInt(int x) {
    printf("%d", x);
    return;
}


void writeDouble(double x) {

    printf("%f", x);
    return;

}


void writeBool(int x) {

    if (x == 1) 
    	printf("%s", "true");
    else 
    	printf("%s", "false");

    return;
}


void writeString(char *x) {

    printf("%s",x);
    return;

}


int randomInt() {
 
	return rand();

}


void abortt(int x) {

	switch (x) {

		case 1:
    		printf("%s\n", "Ninguna de las guardias del selector es validad");
			printf("%s\n", "ABORT");  
			exit(0);

		case 2:
			printf("%s\n", "ABORT");  
			exit(0);
	} 
}