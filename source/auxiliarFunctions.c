/* auxiliarFunctions
$ gcc -fPIC -shared auxiliarFunctions.c -o auxiliarFunctions.so
$ clang -fPIC -shared auxiliarFunctions.c -o auxiliarFunctions.so
*/

#include "stdio.h"
#include "stdlib.h"

void _writeLnInt(int x) {
    printf("%d\n", x);
    return;
}


void _writeLnDouble(double x) {

    printf("%f\n", x);
    return;

}


void _writeLnBool(int x) {

    if (x == 1) 
    	printf("%s\n", "true");
    else 
    	printf("%s\n", "false");

    return;
}


void _writeLnString(char *x) {

    printf("%s\n",x);
    return;

}


void _writeInt(int x) {
    printf("%d", x);
    return;
}


void _writeDouble(double x) {

    printf("%f", x);
    return;

}


void _writeBool(int x) {

    if (x == 1) 
    	printf("%s", "true");
    else 
    	printf("%s", "false");

    return;
}


void _writeString(char *x) {

    printf("%s",x);
    return;

}


int _randomInt() {
 
	return rand();

}


void _abort(int x) {

	switch (x) {

		case 1:
    	printf("%s\n", "Ninguna de las guardias del selector es valida");
			printf("%s\n", "ABORT");  
			exit(0);

		case 2:
			printf("%s\n", "ABORT");  
			exit(0);
	} 
}
