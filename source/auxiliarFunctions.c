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


void _writeLnChar(int x) {
    char c = x;
    printf("%c\n", c);
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


void _writeChar(int x) {
    char c = x;
    printf("%c", c);
    return;

}


void _writeString(char *x) {

    printf("%s",x);
    return;

}


int _randomInt() {
 
	return rand();

}


void _abort(int x, int line, int column) {

	switch (x) {

		case 1:
            printf("ABORT: En la línea %d, columna %d. El valor suministrado no cumplió ninguna de las guardias. \n", line, column);  
			exit(0);

		case 2:
            printf("ABORT: En la línea %d, columna %d.  \n", line, column);  
			exit(0);

        case 3:
            printf("ADVERTENCIA: En la línea %d, columna %d. La Precondición no es correcta. \n", line, column);  
            break;

        case 4:
            printf("ABORT: En la línea %d, columna %d. La Postcondición no se cumplió. \n", line, column);  
            exit(0);

        case 5:
            printf("ABORT: En la línea %d, columna %d. La Aserción no se cumplió. \n", line, column);  
            exit(0);
       
        case 6:
            printf("ABORT: En la línea %d, columna %d. La Invariante no se cumplió. \n", line, column);  
            exit(0);
        
        case 7:
            printf("ABORT: En la línea %d, columna %d. La Cota no se cumplió. \n", line, column);  
            exit(0);

        case 8:
            printf("ABORT: En la línea %d, columna %d. La Cota no se cumplió ZEROO. \n", line, column);  
            exit(0);

        case 9:
            printf("ABORT: En la línea %d, columna %d. Divisón por cero.\n", line, column);  
            exit(0);
	
        case 10:
            printf("ABORT: En la línea %d, columna %d. FORALL.\n", line, column);  
            exit(0);

        case 11:
            printf("ABORT: En la línea %d, columna %d. EXISTT satisfaceeeeee.\n", line, column);  
            exit(0);

    } 
}


    