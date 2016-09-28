/* graciela-lib
$ gcc -fPIC -shared graciela-lib.c -o graciela-lib.so
$ clang -fPIC -shared graciela-lib.c -o graciela-lib.so
*/

#include "math.h"
#include "stdio.h"
#include "limits.h"
#include "stdlib.h"
#include "wchar.h"
#include "locale.h"
#include "string.h"

int8_t* _openFile(char* name) {
  FILE* file;

  file = fopen(name, "r");
  if(file == NULL)
  {
    printf ("%s %s", "Error abriendo el archivo: ", name);
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
  , A_NEGATIVE_INDEX
  , A_OUT_OF_BOUNDS_INDEX
  , A_BAD_ARRAY_ARG
  , A_NEGATIVE_ROOT
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
    case A_NEGATIVE_INDEX:
      printf (":\n\ta negative index was used to access an array.\n"); break;
    case A_OUT_OF_BOUNDS_INDEX:
      printf (":\n\tout of bounds index in access to array.\n"); break;
    case A_BAD_ARRAY_ARG:
      printf (":\n\tbad array argument, size mismatch.\n"); break;
    case A_NEGATIVE_ROOT:
      printf (":\n\tattempted to take the square root of a negative value.\n"); break;
    default:
      printf (":\n\tunknown reason.\n"); break;
  }
  exit (1);
}

typedef enum
  { W_MANUAL
  , W_PRE
  , W_POST
  , W_INVARIANT
  , W_REPINVARIENT
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
    case W_INVARIANT:
      printf (":\n\tthe invariant was falsified.\n"); break;
    case W_REPINVARIENT:
      printf (":\n\tthe representation invariant was falsified.\n"); break;
    default:
      printf (":\n\tunknown reason.\n"); break;
  }
}

int _abs_i (int x, int line, int column) {
  if (x < 0)
    if (x == INT_MIN)
      _abort (A_OVERFLOW, line, column);
    else
      return (-x);
  else
    return x;
}

double _abs_f (double x) {
  return x < 0 ? (-x) : x;
}

int _sqrt_i (int x, int line, int column) {
  if (x < 0)
    _abort (A_NEGATIVE_ROOT, line, column);
  else
    return floor(sqrt(x));
}

double _sqrt_f (double x, int line, int column) {
  if (x < 0)
    _abort (A_NEGATIVE_ROOT, line, column);
  else
    return sqrt(x);
}


int _float2int (double x, int line, int column) {
  if (-2147483648.49 <= x && x <= 2147483647.49)
    return (x >= 0 ? (int)(x+0.5) : (int)(x-0.5));
  else
    _abort (A_OVERFLOW, line, column);
}

int _char2int (char x) {
  return (int)(x);
}

char _float2char (double x, int line, int column) {
  if (0.0 <= x && x <= 255.49)
    return (char)(x+0.5);
  else
    _abort (A_OVERFLOW, line, column);
}

char _int2char (int x, int line, int column) {
  if (0 <= x && x <= 255)
    return (char)(x);
  else
    _abort (A_OVERFLOW, line, column);
}

double _char2float (char x) {
  return (double)(x);
}

double _int2float (int x) {
  return (double)(x);
}


int _traceIntString (int x) {
  printf("TRACE: %i\n", x);
  return x;
}
double _traceFloatString (double x) {
  printf("TRACE: %f\n", x);
  return x;
}
char _traceCharString (char x) {
  printf("TRACE: %c\n", x);
  return x;
}
int _traceBoolString (int x) {
  printf("TRACE: %s\n", x ? "true" : "false");
  return x;
}
int _traceStringIntString (char* x, int y) {
  printf("TRACE: (%s, %i)\n", x, y);
  return y;
}
double _traceStringFloatString (char* x, double y) {
  printf("TRACE: (%s, %f)\n", x, y);
  return y;
}
char _traceStringCharString (char* x, char y) {
  printf("TRACE: (%s, %c)\n", x, y);
  return y;
}
int _traceStringBoolString (char* x, int y) {
  printf("TRACE: (%s, %s)\n", x, y ? "true" : "false");
  return y;
}
