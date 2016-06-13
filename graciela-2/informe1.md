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

graciela-lib.c [x]

:   Librería utilizada para las llamadas al sistema, para imprimir los errores a
    momento de ejecución y realizar las lecturas de variables, ya sea de
    archivos o de la entrada estandar. Este archivo es necesario, ya que llvm
    no provee funciones de entrada/salida de manera nativa.

Aborts.hs [x]

:   Utilizado para el código intermedio del LLVM, se encarga de generar las
    etiquetas que se usan para los saltos en caso de un error a momento de
    ejecución.

AST.hs [x]

:   Tipo de datos para la representación del arbol sintáctico.

ASTtype.hs [x]

:   Recorrido del arbol para la verificación de tipos de todos los nodos,
    tambien incluye la creación de los rangos para los cuantificadores.

Codegen.hs [x]

:   Genera el código intermedio para LLVM de cada uno de los nodos del arbol.

CodegenState.hs [x]

:   Monad de estado utilizado para ir generando el código intermedio de LLVM,
    definir funciones para LLVM, etiquetas para los saltos, etc.

Contents.hs [x]

:   Tipo de datos usado como contenido de la tabla de símbolos, si es una
    variable, una función, un procedimiento, o un argumento a una función o
    procedimiento (Depende de lo que sea, se guarda diferente información).

Declarations.hs [x]

:   Parseo de las declaraciones y almacenamiento de variables  y constantes en
    la tabla de símbolos.

Expression.hs [x]

:   Analizador sintáctico de todas las expresiones posibles.

Lexer.hs [x]

:   Analizador lexicográfico.

Limits.hs [x]

:   Límites establecidos para los enteros y los flotantes del lenguaje.
    *puede ser mejorado con Numeric.Limits y Data.Int+m{ax,in}Bound*

Location.hs [x]

:   Todo lo referente a la localización de variables, errores, etc.

Main.hs [x]

:   Programa principal, se realiza la lectura de parámetros de entrada, los
    ajustes por arquitectura. Si hubo errores en la entrada, se imprimen, y
    si no, se compila el archiv LLVM generado usando Clang.

MyParseError.hs [x]

:   Tipo de datos para los errores ocurridos en el analizador sintáctico.

MyTypeError.hs [x]

:   Tipo de datos para los errores ocurridos en las verificaciones de tipos.

Parser.hs [x]

:   Analizador sintáctico de la mayor parte del lenguaje, instrucciones,
    funciones procedimientos, aserciones, entre otras.
    *Planeamos refactorizar los parsers en una carpeta Parser con varios
    módulos dedicados. Este módulo sería algo como `program`.*

ParserError.hs [ ]

:   Utilizado para leer tokens al momento de encontrar un error, y poder
    realizar la recuperación de errores.

ParserState.hs [ ]

:   Utilizado para guardar los errores, los datos en la tabla, cambiar de scope
    en la tabla, entre otras cosas.

ParserType.hs [x]

:   Analizador sintáctico de los tipos del lenguaje. *Iría en la carpeta Parser*

ReduceAST.hs [!]

:   Recorta partes del AST cuando las operaciones son obvias, una suma de
    constantes, etc.

State.hs [x]

:   Monad utilizado para guardar: Los errores de parseo, la tabla de símbolos,
    los errores de tipo y los nombres de los archivos para las lecturas.

SymbolTable.hs [x]

:   Tabla de símbolos del compilador.

Token.hs [x]

:   Los tokens generados por el lexer.

TokenParser.hs [?]

:   Analizador sintáctico para las palabras reservadas del lenguaje. *Why?*

Type.hs [x]

:   Tipos utilizados dentro del compilador.

TypeState.hs [?]

:   Funciones para crear las instancias de errores que seran guardados en el
    estado.
    *básicamente `err`?*

VerTypes.hs [?]

:   Realiza todas las verificaciones de tipos necesarias, entre ellos tambien
    estan los errores que tienen que ver con los identificadores, procedimiento
    no declarado,  numero de argumentos incorrecto, entre otros.
    *relación con ASTType?*
