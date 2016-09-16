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
  return calloc(1,size);
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
  { A_IF
  , A_MANUAL
  , A_POST
  , A_ASSERT
  , A_INVARIANT
  , A_NONDECREASING_BOUND
  , A_NEGATIVE_BOUND
  , A_DIVISION_BY_ZERO
  , A_OVERFLOW
  , A_UNDERFLOW
  , A_EMPTY_RANGE
  , A_NULL_POINTER_ACCESS
  , A_REPRESENTATION_INVARIANT
  } abort_t;

void _abort (abort_t reason, int line, int column) {
  printf ("\x1B[0;31mABORT:\x1B[m at line %d, column %d", line, column);
  switch (reason) {
    case A_IF:
      printf (":\n\tno true branch found in conditional.\n"); break;
    case A_MANUAL:
      printf (".\n"); break;
    case A_POST:
      printf (":\n\tthe postcondition was falsified.\n"); break;
    case A_ASSERT:
      printf (":\n\tthe assertion was falsified.\n"); break;
    case A_INVARIANT:
      printf (":\n\tthe invariant was falsified.\n"); break;
    case A_NONDECREASING_BOUND:
      printf (":\n\tthe bound didn't decrease.\n"); break;
    case A_NEGATIVE_BOUND:
      printf (":\n\tthe bound became negative.\n"); break;
    case A_DIVISION_BY_ZERO:
      printf (":\n\ta division by zero was attempted.\n"); break;
    case A_OVERFLOW:
      printf (":\n\ta value overflowed.\n"); break;
    case A_UNDERFLOW:
      printf (":\n\ta value underflowed.\n"); break;
    case A_EMPTY_RANGE:
      printf (":\n\tthe quantification range was empty.\n"); break;
    case A_NULL_POINTER_ACCESS:
      printf (":\n\ta null pointer was dereferenced.\n"); break;
    case A_REPRESENTATION_INVARIANT:
      printf (":\n\tthe representation invariant was falsified.\n"); break;
    default:
      printf (":\n\tunknown reason.\n"); break;
  }
  exit (1);
}

typedef enum
  { W_MANUAL
  , W_PRE
  , W_POST
  } warning_t;

void _warn (warning_t reason, int line, int column) {
  printf ("\x1B[0;35mWARNING:\x1B[m at line %d, column %d", line, column);
  switch (reason) {
    case W_MANUAL:
      printf (".\n"); break;
    case W_PRE:
      printf (":\n\tthe precondition was falsified.\n"); break;
    case W_POST:
      printf (":\n\tthe postcondition was falsified.\n"); break;
    default:
      printf (":\n\tunknown reason.\n"); break;
  }
}
