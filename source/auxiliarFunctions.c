/* auxiliarFunctions
$ gcc -fPIC -shared auxiliarFunctions.c -o auxiliarFunctions.so
$ clang -fPIC -shared auxiliarFunctions.c -o auxiliarFunctions.so
*/

#include "stdio.h"
#include "stdlib.h"
#include "wchar.h"
#include "locale.h"
#include "string.h"

int8_t* _openFile(char* nombreArchivo) {
    FILE* file;

    file = fopen(nombreArchivo, "r");
    if(file == NULL) 
    {
      printf ("%s %s", "Error abriendo el archivo: ", nombreArchivo);
      exit(EXIT_FAILURE);
    }
    return (int8_t *) file;
}

void _closeFile(int8_t* file) {
    FILE* f = (FILE*) file;
    fclose(f);
}

int _readFileInt(int8_t* file) {
  FILE* f = (FILE*) file;

  int n;
  int r = fscanf(f, "%d", &n);

  if (r == EOF)
  {
    printf("%s\n", "Error: Fue alcanzado el final del archivo al intentar ejecturar una lectura"); 
    exit(EXIT_FAILURE);
  }
  if (r == 0)
  {
    printf("%s\n", "Error: El valor obtenido después de la lectura no es del tipo int");
    exit(EXIT_FAILURE);
  }

  return n;
}

char _readFileChar(int8_t* file) {
  FILE* f = (FILE*) file;
  char n;
  int r = fscanf(f, "%c", &n);

  if (r == EOF)
  {
    printf("%s\n", "Error: Fue alcanzado el final del archivo al intentar ejecturar una lectura"); 
    exit(EXIT_FAILURE);
  }
  if (r == 0)
  {
    printf("%s\n", "Error: El valor obtenido después de la lectura no es del tipo char");
    exit(EXIT_FAILURE);
  }

  return n;
}

double _readFileDouble(int8_t* file) {
  FILE* f = (FILE*) file;
  double n;
  int r = fscanf(f, "%lf", &n);

  if (r == EOF)
  {
    printf("%s\n", "Error: Fue alcanzado el final del archivo al intentar ejecturar una lectura"); 
    exit(EXIT_FAILURE);
  }
  if (r == 0)
  { 
    printf("%s\n", "Error: El valor obtenido después de la lectura no es del tipo double");
    exit(EXIT_FAILURE);
  }

  return n;
}

int _readIntStd () {
    int  n;
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
    char   c;

    scanf("%lf", &n);
    scanf("%c" , &c);
    return n;
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

    setlocale(LC_CTYPE, "");
    wint_t ch = x;

    printf("%lc", x);
    return;
}


void _writeString(short *x) {

    int i=0;
    setlocale(LC_CTYPE, "");

    while (!(x[i] == 0)) {

        if ((x[i] == 92) && (x[i+1] == 110)) {
          printf("\n");
          i++;
        } else
          printf("%lc",x[i]);
        
        i++;
    };

    return;
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

    setlocale(LC_CTYPE, "");
    wint_t ch = x;

    printf("%lc\n", x);
    return;
}


void _writeLnString(short *x) {

    _writeString(x);
    printf("\n");
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
            printf("\nABORT: En la línea %d, columna %d, El valor suministrado no cumple ninguna de las guardias. \n\n", line, column);  
			exit(0);

		case 2:
            printf("\nABORT: En la línea %d, columna %d.  \n\n", line, column);  
			exit(0);

        case 3:
            printf("\nADVERTENCIA: En la línea %d, columna %d, La Precondición no se satisface. \n", line, column);  
            break;

        case 4:
            printf("\nABORT: En la línea %d, columna %d, La Postcondición no se satisface. \n\n", line, column);  
            exit(0);

        case 5:
            printf("\nABORT: En la línea %d, columna %d, La Aserción no se satisface. \n\n", line, column);  
            exit(0);
       
        case 6:
            printf("\nABORT: En la línea %d, columna %d, La Invariante no se satisface. \n\n", line, column);  
            exit(0);
        
        case 7:
            printf("\nABORT: En la línea %d, columna %d, La Cota no decremento. \n\n", line, column);  
            exit(0);

        case 8:
            printf("\nABORT: En la línea %d, columna %d, El resultado de la función de cota no puede ser negativa. \n\n", line, column);  
            exit(0);

        case 9:
            printf("\nABORT: En la línea %d, columna %d, Divisón por cero.\n\n", line, column);  
            exit(0);
	
        case 10:
            printf("\nADVERTENCIA: En la línea %d, columna %d, El Cuantificador Universal no se satisface.\n", line, column);  
            break;

        case 11:
            printf("\nADVERTENCIA: En la línea %d, columna %d, El Cuantificador Existencial no se satisface.\n", line, column);  
            break;

        case 12:
            printf("\nABORT: En la línea %d, columna %d, Overflow.\n\n", line, column);  
            exit(0);

        case 13:
            //printf("\nABORT: En la línea %d, columna %d, Rango vacio.\n", line, column);  
            //exit(0); 
            break;

        case 14:
            printf("\nABORT: En la línea %d, columna %d, Rango vacio.\n\n", line, column);  
            exit(0);

    } 
}


    
