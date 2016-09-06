/* graciela-lib
$ gcc -fPIC -shared graciela-lib.c -o graciela-lib.so
$ clang -fPIC -shared graciela-lib.c -o graciela-lib.so
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

int8_t* _malloc(int size){
  return malloc(size);
}

void _free(int8_t *mem){
  free(mem);
}

int _readFileInt(int8_t* file) {
  FILE* f = (FILE*) file;

  int n;
  int r = fscanf(f, "%d", &n);

  if (r == EOF)
  {
    printf("%s\n", "Error: Fue alcanzado el final del archivo al intentar efectuar una lectura");
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
    printf("%s\n", "Error: Fue alcanzado el final del archivo al intentar efectuar una lectura");
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
    printf("%s\n", "Error: Fue alcanzado el final del archivo al intentar efectuar una lectura");
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
  if (x == 0)
    printf("false");
  else
    printf("true");
  return;
}


void _writeChar(int x) {
  setlocale(LC_CTYPE, "");

  printf("%lc", x);
  return;
}


void _writeString(char *x) {
  int i=0;
  setlocale(LC_CTYPE, "");

  printf ("%s", x);
  return;
}


void _ln() {
  printf("\n");
  return;
}


int _random() {
  return rand();
}


int _max(int x, int y) {
  return x > y ? x : y;
}


double _maxF(double x, double y) {
  return x > y ? x : y;
}


int _min(int x, int y) {
  return x < y ? x : y;
}


double _minF(double x, double y) {
  return x < y ? x : y;
}


typedef enum
  { IF
  , MANUAL
  , POST
  , ASSERT
  , INVARIANT
  , NONDECREASING_BOUND
  , NEGATIVE_BOUND
  , DIVISION_BY_ZERO
  , OVERFLOW
  , UNDERFLOW
  , EMPTY_RANGE
  , NULL_POINTER_ACCESS
  } abort_t;

void _abort (abort_t reason, int line, int column) {
  printf ("ABORT: at line %d, column %d", line, column);
  switch (reason) {
    case IF:
      printf (":\n\tno true branch found in conditional.\n"); break;
    case MANUAL:
      printf (".\n"); break;
    case POST:
      printf (":\n\tthe postcondition was falsified.\n"); break;
    case ASSERT:
      printf (":\n\tthe assertion was falsified.\n"); break;
    case INVARIANT:
      printf (":\n\tthe invariant was falsified.\n"); break;
    case NONDECREASING_BOUND:
      printf (":\n\tthe bound didn't decrease.\n"); break;
    case NEGATIVE_BOUND:
      printf (":\n\tthe bound became negative.\n"); break;
    case DIVISION_BY_ZERO:
      printf (":\n\ta division by zero was attempted.\n"); break;
    case OVERFLOW:
      printf (":\n\ta value overflowed.\n"); break;
    case UNDERFLOW:
      printf (":\n\ta value underflowed.\n"); break;
    case EMPTY_RANGE:
      printf (":\n\tthe quantifier's range was empty.\n"); break;
    case NULL_POINTER_ACCESS:
      printf (":\n\ta null pointer was dereferenced.\n"); break;
    default:
      printf (":\n\tunknown reason.\n"); break;
  }
  exit (1);
}

typedef enum
  { PRE
  , FORALL
  , EXISTENTIAL
  } warning_t;

void _warn (warning_t reason, int line, int column) {
  printf ("WARNING: at line %d, column %d\n:", line, column);
  switch (reason) {
    case PRE:
      printf ("\tthe precondition was falsified.\n"); break;
    case FORALL:
      printf ("\tthe universal quantification was falsified.\n"); break;
    case EXISTENTIAL:
      printf ("\tthe existential quantification was falsified.\n"); break;
    default:
      printf ("\tunknown reason.\n"); break;
  }
}
