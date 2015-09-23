/* auxiliarFunctions
$ gcc -fPIC -shared auxiliarFunctions.c -o auxiliarFunctions.so
$ clang -fPIC -shared auxiliarFunctions.c -o auxiliarFunctions.so
*/

#include "stdio.h"
#include "stdlib.h"

int _readIntStd () {
  int n;
  char c;
  scanf("%d", &n);
  scanf("%c", &c);
  return n;
}

char _readCharStd () {
  char n;
  char c;
  scanf("%c", &n);
  scanf("%c", &c);
  return n;
}

double _readDoubleStd () {
  double n;
  char c;
  scanf("%lf", &n);
  scanf("%c", &c);
  return n;
}

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


int _random() {
 
	return rand();
}


int _max(int x, int y) {
 
    if (x < y) 
        return y; 
    else 
        return x;
}


double _maxF(double x, double y) {
 
    if (x < y) 
        return y; 
    else 
        return x;
}


int _min(int x, int y) {
 
    if (x < y)
        return x; 
    else 
        return y;
}


double _minF(double x, double y) {
 
    if (x < y)
        return x; 
    else 
        return y;
}


void _abort(int x, int line, int column) {

	switch (x) {

		case 1:
            printf("ABORT: En la línea %d, columna %d. El valor suministrado no cumple ninguna de las guardias. \n", line, column);  
			exit(0);

		case 2:
            printf("ABORT: En la línea %d, columna %d.  \n", line, column);  
			exit(0);

        case 3:
            printf("ADVERTENCIA: En la línea %d, columna %d. La Precondición no se satisface. \n", line, column);  
            break;

        case 4:
            printf("ABORT: En la línea %d, columna %d. La Postcondición no se satisface. \n", line, column);  
            exit(0);

        case 5:
            printf("ABORT: En la línea %d, columna %d. La Aserción no se satisface. \n", line, column);  
            exit(0);
       
        case 6:
            printf("ABORT: En la línea %d, columna %d. La Invariante no se satisface. \n", line, column);  
            exit(0);
        
        case 7:
            printf("ABORT: En la línea %d, columna %d. La Cota no decremento. \n", line, column);  
            exit(0);

        case 8:
            printf("ABORT: En la línea %d, columna %d. El resultado de la función de cota no puede ser negativa. \n", line, column);  
            exit(0);

        case 9:
            printf("ABORT: En la línea %d, columna %d. Divisón por cero.\n", line, column);  
            exit(0);
	
        case 10:
            printf("ADVERTENCIA: En la línea %d, columna %d. El Cuantificador Universal no se satisface.\n", line, column);  
            break;

        case 11:
            printf("ADVERTENCIA: En la línea %d, columna %d. El Cuantificador Existencial no se satisface.\n", line, column);  
            break;
    } 
}


    
