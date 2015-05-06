
 Proyecto creado por Joel Araujo y José Luis Jiménez.  
 Tutores: Ricardo Monascal y Ernesto Hernández Novich.

- - -

# GaCeLa 2.0

* Hablar filosoficamente del lenguaje ****

## Consideraciones Léxicográficas

 * Las palabras descritas a continuación son reservadas por el lenguaje, por lo tanto no pueden ser utilizadas como identificadores ni ser redefinidas.

|          |             |           |             |         |           |        |           |
|----------|-------------|-----------|-------------|---------|-----------|--------|-----------|
| program  | pre         | post      | bound       | func    | proc      | in     | out       |
| div      | mod         | max       | min         | exist   | sigma     | pi     | union     |
| if       | fi          | inv       | do          | od      | gcd       | abs    | sqrt      |
| length   | var         | const     | abort       | random  | skip      | write  | writeln   |
| readInt  | readDouble  | readChar  | readString  | toInt   | toDouble  | toChar | toString  |
| boolean  | int         | double    | char        | string  | array     | True   | False     |
| MIN_INT  | MIN_DOUBLE  | MAX_INT   | MAX_DOUBLE  |         |           |        |           | |
- - -



## Referencias de la Gramática

 * Los terminales se encuentran encerrados en comillas dobles `"x"`

 * Las reglas vacías (lamda producciones) se representan como `%empty`.

 * Las producciones provenientes de un mismo No terminal son indicadas a través del separador `|`.

	    PONER GRAMATICA


## Estructura del programa

* Todo programa en **GaCeLa 2.0** consta de un `program` con su respectivo identificador. 

* Es posible que un programa no posea declaraciones de tipos algebraicos o abstractos.

* Un archivo **vacío** no es un programa valido.

* La estructura del programa es la siguiente:

		(Declaraciones Algebraicas y Abstractas)

		program <Identificador> |[
			<Funciones y Procedimientos>
            <Acciones del programa>
        ]|


## Alcance  FALTA

* Por cada nuevo bloque, se crea un nuevo nivel de anidamiento.

* Se pueden utilizar variables que ya fueron declaradas en niveles superiores.

* Las variables de un anidamiento inferior no pueden ser vistas en uno superior.

* Si se declara una variable que ya estaba declarada en un nivel superior, ésta será solapada por la nueva declaración.

* No se puede declarar una variable del mismo nombre que otra que se encuentre previamente declarada en el mismo nivel de anidamiento.

* Los parámetros de una función tiene el mismo nivel de anidamiento que las variables locales de la función y por lo tanto no se puede declarar una variable local con el mismo nombre de un parámetro.

## Tipos de datos

### Primitivos

#### Boleanos

* Corresponde a los valores lógicos de comparación y son representador por las palabras reservadas `True` y `False`. Se declaran utilizando la palabra reservada `boolean`. Ocupan 8 Bits de memoria.

#### Enteros

*  Corresponde a los números enteros, pueden tomar valores positivos y negativos que no contengan parte decimal. Se declaran utilizando la palabra reservada `int`. Ocupan 32 Bits de memoria.

#### Flotantes

*  Corresponde a los números reales o punto flotantes IEEE-754, pueden tomar valores positivos y negativos que contengan parte decimal. Se declaran utilizando la palabra reservada `double`. Ocupan, por defecto, Ocupan 32 Bits de memoria.


#### Caracteres

*  Corresponde a un carácter imprimible de **ASCII**.  Se declaran utilizando la palabra reservada `char`. Ocupan 8 Bits de memoria.


### Compuestos

#### Arreglos FALTA

* Corresponde al tipo compuesto `array`.

* Son unidimensionales con base cero.

* Solo pueden ser de un solo tipo específico.

* Su ocupación en memoria corresponde a la anchura del tipo por el tamaño del arreglo.

* Pueden ser de tipos primitivos, `struct` o `union`.

* Deben ser **estrictamente** de indicie mayor o igual a 1.

* La declaración de los arreglos es la siguiente:

		array <Tipo> : <Identificador> [<Número>];


#### Arreglos Multidimensionales FALTA

* Son arreglos con más de una dimensión de base cero.

* Solo pueden ser de un solo tipo específico.

* Su ocupación en memoria corresponde a la anchura del tipo por el tamaño de cada una de las dimensiones del arreglo.

* Pueden ser de tipos primitivos, `struct` o `union`.

* La declaración de los arreglos multidimensionales es la siguiente:

		array <array <array ... <Tipo> [<Número>] > ... [<Número>]> [<Número>]> :
        <Identificador> [<Número>];

#### Algeibraicos FALTA

* Corresponde al tipo compuesto `struct`.

* Pueden contener campos de diferentes tipos.

* Su tamaño en memoria corresponde a la suma total de todos los elementos que lo componen.

* El tipo de los campos puede corresponder tanto a tipos definidos en el lenguaje como a tipos definidos por el usuario (structs, unions).

* Dentro de la estructura no se aceptan listas de declaraciones de variables, se deben declarar una por una.

* La declaración de estructuras es la siguiente:

        struct <Identificador> {
            <Tipo> : <Identificador>;
            ...
            <Tipo> : <Identificador>;
        }

#### Algebraicos FALTA

* Corresponde al tipo compuesto `union`.

* Pueden contener variables de diferentes tipos.

* Puede tener variables de cualquier tipo definido en el lenguaje.

* Su tamaño en memoria corresponde al tamaño del elemento más grande que lo compone.

* Las uniones declaradas pueden ser utilizadas como un tipo compuesto.

* Dentro de la uniones no se aceptan listas de declaraciones de variables, se deben declarar una por una.

* La declaración de uniones es la siguiente:

        union <Identificador> {
            <Tipo> : <Identificador>;
            ...
            <Tipo> : <Identificador>;
        }

### Especial

#### Cadenas de caracteres

* Representa una secuencia de caracteres **ASCII** y se declaran utilizando la palabra reservada `string`.

* Se escriben utilizando las comillas dobles `" "`. Pueden contener cualquier caracter incluyendo el salto de linea `\n`.

* Puede concatenarse dos cadenas de caracteres utilizando el operador `+`.



## Variables

* Las variables pueden ser de cualquier tipo primitivo, compuesto o especial. Se declaran utilizando la palabra reservada `var`.

* Las constantes son variables que solo se les asigna un valor y no pueden ser redefinidas. Se declaran utilizando la palabra reservada `const`.

* Las variables no debe ser inicializadas obligatoriamente, es responsabilidad del programador inicializar variables antes de ser utilizadas (El lenguaje no provee un valor por defecto de inicialización).

* Se pueden definir una o más variables de un mismo tipo en la misma línea, como tambien se pueden inicializar al mismo tiempo.

* El nivel de anidamiento es el **mismo** para variables declaradas en el mismo bloque.

* Se permite utilizar identificadores que fueron declarados en un nivel de anidamiento superior (El identificador superior dejaría de ser visible en el nivel de anidamiento actual).

* La declaración de variables es la siguiente:


		var <Identificador> : <Tipo> ;
        var <Identificador>, ..., <Identificador> : <Tipo>;
        var <Identificador> := <Expresion>, <Identificador> := <Expresion> : <Tipo>;
        const <Identificador> := <Expresion> : <Tipo>;

## Asignación

 * Solo se puede asignar expresiones que sean del mismo tipo que la del identificador.

 * Solo se puede hacer **una** asignación por línea, no existe asignación múltiple.

 * Las asignaciones se hacen por valor y profundamente.

 * Las asignaciones son de la siguiente forma:

		<Variable> := <Expresión>;
		<Variable>, ..., <Variable> := <Expresión>, ..., <Expresión>;
		Asignacion RARA de Arreglos

## Funciones

* Las funciones tienen cero o más parámetros de entrada.

* Los parámetros que sean de tipo primitivo son pasados por valor y los de tipo compuesto (`array`, `struct`, `union`) son pasados por referencia.

* Los parámetros de entrada de una función tienen el mismo nivel de anidamiento que las variables locales de la función.

* Se pueden retornar tanto tipos primitivos como tipos compuestos.

* En caso de que la función no retorne nada, se debe colocar `void` como tipo de retorno.

* El nivel de anidamiento entre las funciones es **distinto**.

* Las funciones pueden ser recursivas.

* La declaración de funciones es la siguiente:

        <Tipo> solver <Identificador> (<Parámetros>)
        solve
            <Variables Locales>
            <Instrucciones>

            return <Expresión>; /~ En caso de que el tipo de retorno
                                       sea void esta línea no existe ~/
        end

* Se puede utilizar las funciones en el programa luego de ser declaradas.

* Las llamadas de funciones se pueden utilizar como **expresiones** en el programa, al menos que su tipo de retorno sea `void`.

* La llama de funciones es la siguiente:

        <Variable>(<Parámetros>);
        <Variable> := <Variable>(<Parámetros>); /~ Puede ser una
                                                             expresión más grande ~/


## Operadores

 * Los operadores aritméticos ( `+`, `-`, `*`, `^`, `div`, `mod`, `gcd`, `max`, `min`) sólo pueden ser utilizados en conjunto con operandos enteros o flotantes, los dos operandos deben ser del mismo tipo. El resultado es del mismo tipo que los operandos.

 * El operador unitario (`-`) sólo puede ser utilizado con operandos enteros o flotantes.

 * Los operadores relacionales (`>=`, `<=`, `>`, `<`) sólo pueden ser utilizados con operandos enteros o flotantes, los dos operandos deben ser del mismo tipo. El resultado es de tipo booleano.

 * Los operadores de comparación (`==`, `!=`) operan sobre todos los tipos primitivos y compuestos. El resultado es de tipo booleano.

 * Los operadores lógicos (`\/`, `/\`, `!`) sólo pueden ser utilizados con operandos booleanos. El resultado es de tipo booleano.

 * Los operadores de todas las expresiones se evalúan de izquierda a derecha.

 * Precedencia de los operadores de mayor precedencia a la menor:

        ! - (unitario lógico y aritmético)
        * / mod (multiplicación, división, mod)
        + - (suma, resta)
        < <= > >= (relacionales)
        == != (comparación)
        && ("y" Lógico)
        || ("o" Lógico)
        := (Asignación)

## Selector


* Existen un solo tipo de selector el cual puede contener una o mas condiciones cada una con su respectiva secuencia de acciones.

* El selector evalua **todas** la expresion de todo los condicionales, en caso de que uno o mas sean verdaderos pasa a ejecutar las lista de acciones asociada a la primera que resulto verdadera, en caso de que ninguna sea verdadera entonces no se ejecutara ninguna.

*  El selector es de la siguiente manera:

		if <Expresion> -> <Acciones>
        |  ...
        | <Expresion> -> <Acciones>
        fi

## Iteración

* La expresion asociada a la palabra reservada `inv` representa la invariante que debe cumplir el ciclo en el inicio de cada iteracion.

* La expresion asociada a la palabra reservada `bound` representa la cota que decrese en cada iteracion del ciclo.

* El iterador evalua **todas** la expresion de todo los condicionales, en caso de que uno o mas sean verdaderos pasa a ejecutar las lista de acciones asociada a la primera que resulto verdadera y luego se vuelven a evaluar de nuevo todos los condicionales, en caso de que ninguna sea verdadera entonces no el ciclo termina.

* La iteración acotada es la siguiente:

		{inv <Expresion>} {bound <Expresion>}
 		do B1 -> S1
 		|     ...
  		|     Bn -> Sn
 		od


## Entrada

* `readInt` lee un numero entero de la entrada estandar y lo almacena en la variable indicada, la variable necesariamente tiene que ser de tipo `int`.

* `readDouble` lee un numero flotante de la entrada estandar y lo almacena en la variable indicada, la variable necesariamente tiene que ser de tipo `double`.

* `readString` lee una cadena de caracteres e de la entrada estandar y lo almacena en la variable indicada. la variable necesariamente tiene que ser de tipo `string`.

* La variable empleada para almacenar la información debe estar previamente declarada.

* La entrada de datos se especifica como sigue:

        readInt(<Variable>);
        readDouble(<Variable>);
        readString(<Variable>);


## Salida

* Para imprimir en la salida estandar se utiliza la palabra reservada `write`.

* Solamente se pueden imprimir en la salida estandar cadenas de caracteres.

* Para imprimir variables de tipo entero, flotante o booleano tienen que convertirse en cadena de caracteres previamente.

* Se pueden imprimir constantes o literales de tipo enteros o flotantes.

* Si se desea imprimir la información de una variable, ésta debe estar previamente declarada e inicializada.

* El lenguaje incorpora la función `writeln` que imprime un salto de línea inmediatamente después de la información indicada.

* La salida de datos se especifica como sigue:

        write(toString(<Expresión>));
        writeln("Esto es una cadena de caracteres con un salto de linea");


## Funciones Predefinidas

### Conversión de Tipos


* `toInt` dado un parametro de entrada de tipo flotante o una cadena de caracteres numericos, retorna un numero entero asociado.

* `toDouble` dado un parametro de entrada de tipo entero o una cadena de caracteres numericos, retorna un numero entero asociado.

* `toString` dado un parametro de entrada de tipo entero o flotante, retorna una cadena de caracteres asociado al numero.

* La conversión explicita de tipos es como sigue:

		toInt(<Expresión>);
		toDouble(<Expresión>);
		toString(<Expresión>);


### Otras funciones

#### Tamaño

 * `length` dado un arreglo o una cadena de caracteres retorna la cantidad de casillas que tiene el arreglo o la cantidad de caracteres que posee la cadena, respectivamente.

#### Valor Absoluto

* `abs` dado una variable entera o flotante retorna el numero natural asociado.

#### Raiz Cuadrada

* `sqrt` dado una variale entera o flotante retorna la raiz cuadrada del numero


## Entrada y Salida de Archivos
