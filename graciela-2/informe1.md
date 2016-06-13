---
papersize: letter
geometry: margin=1in
documentclass: article
fontsize: 12pt
...

# Primera etapa (Revisión bibliográfica y refinamiento del front-end)

## Revisión del trabajo previo

Se revisaron los archivos de código fuente escritos por Araujo y Jiménez. A
continuación, se presentan los archivos junto con su función dentro del
compilador.

graciela-lib.c [ ]

:   Librería utilizada para las llamadas al sistema, para imprimir los errores a
    momento de ejecución y realizar las lecturas de variables, ya sea de
    archivos o de la entrada estandar.

Aborts.hs [ ]

:   Utilizado para el código intermedio del LLVM, se encarga de generar las
    etiquetas para realizar los saltos en caso de un error a momento de
    ejecución.

AST.hs [ ]

:   Tipo de datos para la representación del arbol sintáctico.

ASTtype.hs [ ]

:   Recorrido del arbol para la verificación de tipos de todos los nodos,
    tambien posee la creación de los rangos para los cuantificadores.

Codegen.hs [ ]

:   Genera el código intermedio para el LLVM de cada uno de los nodos del arbol.

CodegenState.hs [ ]

:   Monad de estado utilizado para ir generando el código intermedio del LLVM,
    definir funciones para el LLVM, etiquetas para los saltos, etc.

Contents.hs [ ]

:   Tipo de datos usado como contenido de la tabla de símbolos, si es una
    variable, una función o un procedimiento  (Depende de lo que sea, se guardan
    diferente información).

Declarations.hs [ ]

:   Parseo de las declaraciones y almacenamiento de variables  y constantes en
    la tabla de símbolos.

Expressión.hs [ ]

:   Analizador sintáctico de todas las expresiones posibles.

Lexer.hs [ ]

:   Analizador lexicográfico.

Limits.hs [ ]

:   Límites establecidos para los enteros y los flotantes del lenguaje.

Location.hs [ ]

:   Todo lo referente a la localización de variables, errores, etc.

Main.hs [ ]

:   Programa principal, se realiza la lectura de parámetros de entrada, los
    ajustes por arquitectura y se imprimen las listas de errores.

MyParseError.hs [ ]

:   Tipo de datos para los errores ocurridos en el analizador sintáctico.

MyTypeError.hs [ ]

:   Tipo de datos para los errores ocurridos en las verificaciones de tipos.

Parser.hs [ ]

:   Analizador sintáctico de la mayor parte del lenguaje, instrucciones,
    funciones procedimientos, aserciones, entre otras.

ParserError.hs [ ]

:   Utilizado para leer tokens al momento de encontrar un error, y poder
    realizar la recuperación de errores.

ParserState.hs [ ]

:   Utilizado para guardar los errores, los datos en la tabla, cambiar de scope
    en la tabla, entre otras cosas.

ParserType.hs [ ]

:   Analizador sintáctico de los tipos del lenguaje

ReduceAST.hs [ ]

:   Recorta partes del AST cuando las operaciones son obvias, una suma de
    constantes, etc.

State.hs [ ]

:   Monad utilizado para guardar: Los errores de parseo, la tabla de símbolos,
    los errores de tipo y los nombres de los archivos para las lecturas.

SymbolTable.hs [ ]

:   Tabla de símbolos del compilador.

Token.hs [ ]

:   Los tokens generados por el lexer.

TokenParser.hs [ ]

:   Analizador sintáctico para las palabras reservadas del lenguaje.

Type.hs [ ]

:   Tipos utilizados dentro del compilador.

TypeState.hs [ ]

:   Funciones para crear las instancias de errores que seran guardados en el
    estado.

VerTypes.hs [ ]

:   Realiza todas las verificaciones de tipos necesarias, entre ellos tambien
    estan los errores que tienen que ver con los identificadores, procedimiento
    no declarado,  numero de argumentos incorrecto, entre otros.
