
 Proyecto creado por Joel Araujo y José Luis Jiménez.
 Tutores: Ricardo Monascal y Ernesto Hernández Novich.

- - -

# GaCeLa 2.0

* Hablar filosoficamente del lenguaje ****

## Consideraciones Léxicográficas

 * Las palabras descritas a continuación son reservadas por el lenguaje, por lo tanto no pueden ser utilizadas como identificadores ni ser redefinidas.

|          |             |           |             |         |           |        |           |
|----------|-------------|-----------|-------------|---------|-----------|--------|-----------|
| program  | pre         | post      | bound       | func     | proc      | in     | out      |
| inout    | ref         | max       | min         | exist    | sigma     | pi     | union    |
| if       | fi          | inv       | do          | od       | mod       | abs    | sqrt     |
| with     | var         | const     | abort       | random   | skip      | write  | writeln  |
| read     | toInt       | toDouble  | toChar      | toString | true      | false  | array    |
| boolean  | int         | double    | char        | begin    | end       | MIN_INT| MAX_INT  |
|MIN_DOUBLE| MAX_DOUBLE
- - -



## Referencias de la Gramática

 * Los terminales se encuentran encerrados en comillas dobles `"x"`

 * Las reglas vacías (lamda producciones) se representan como `%empty`.

 * Las producciones provenientes de un mismo No terminal son indicadas a través del separador `|`.

	    PONER GRAMATICA


## Estructura del programa

* Todo programa en **GaCeLa 2.0** consta de un `program` obligatorio con su respectivo identificador.

* Los programas pueden definir varios procedimientos y funciones, como también podría no tener ninguno.

* Un archivo **vacío** no es un programa valido.

* La estructura del programa es la siguiente:


		program <Identificador> begin
			<Funciones y Procedimientos>
            |[
				<Declaracion de variables>
            	<Acciones del programa>
        	|]
		end


## Alcance

* Por cada nuevo bloque, se crea un nuevo alcance.

* Se pueden utilizar variables que ya fueron declaradas en los alcances superiores.

* No se permite solapamiento de variables, por lo tanto no se puede definir ninguna variable que posea el mismo nombre de alguna variable que se encuentre en el alcance actual.

* Si se declara una variable que ya estaba declarada en un nivel superior, ésta será solapada por la nueva declaración.

* Las variables de un alcance inferior no pueden ser vistas (utilizadas) en un alcance superior.

* Los parámetros de un procedimiento tiene el mismo nivel de anidamiento que las variables locales del procedimiento y por lo tanto no se puede declarar una variable local con el mismo nombre de un parámetro.

## Tipos de datos

### Primitivos

#### Boleanos

* Corresponde a los valores lógicos de comparación y son representador por las palabras reservadas `True` y `False`. Se declaran utilizando la palabra reservada `boolean`. Ocupan 1 Bit de memoria.

#### Enteros

*  Corresponde a los números enteros, pueden tomar valores positivos y negativos que no contengan parte decimal. Se declaran utilizando la palabra reservada `int`. Ocupan 32 Bits de memoria.

#### Flotantes

*  Corresponde a los números reales o punto flotantes IEEE-754, pueden tomar valores positivos y negativos que contengan parte decimal. Se declaran utilizando la palabra reservada `double`. Ocupan 32 Bits de memoria.


#### Caracteres

*  Corresponde a un carácter imprimible de **ASCII**.  Se declaran utilizando la palabra reservada `char`. Ocupan 8 Bits de memoria.


### Compuestos

#### Arreglos

* Corresponde al tipo compuesto `array`.

* Son unidimensionales con base cero.

* Solo pueden ser de un solo tipo específico.

* Su ocupación en memoria corresponde a la anchura del tipo por el tamaño del arreglo.

* Deben ser **estrictamente** de indicie mayor o igual a 1, y únicamente del tipo `int`.

* Se pueden utilizar variable previamente declaradas para dar el número de la dimensión del arreglo, siempre y cuando sean del tipo `int`.


* La declaración de los arreglos es la siguiente:


		var <Identificador> : array [<Número>] of <Tipo>;


#### Arreglos Multidimensionales

* Son arreglos con más de una dimensión de base cero.

* Solo pueden ser de un solo tipo específico.

* Su ocupación en memoria corresponde a la anchura del tipo por el tamaño de cada una de las dimensiones del arreglo.

* Deben ser **estrictamente** de indicie mayor o igual a 1, y únicamente del tipo `int`.

* Se pueden utilizar variable previamente declaradas para dar el número de la dimensión del arreglo, siempre y cuando sean del tipo `int`.

* La declaración de los arreglos multidimensionales es la siguiente:


		var <Identificador> : array [<Número>] of <array [<Número>] of ... <array 		      [<Número>] of <Tipo>;


## Operadores

* Los operadores aritméticos ( `+`, `-`, `*`, `^`, `div`, `mod`, `max`, `min`) sólo pueden ser utilizados en conjunto con operandos enteros o flotantes, los dos operandos deben ser del mismo tipo. El resultado es del mismo tipo que los operandos.

* El operador unitario (`-`) sólo puede ser utilizado con operandos enteros o flotantes.

* Los operadores relacionales (`>=`, `<=`, `>`, `<`) permite cual tipo primitivo, siempre y cuando los dos operadores sean del mismo tipo. El resultado es de tipo booleano.

* Los operadores de comparación (`==`, `!=`) operan sobre todos los tipos primitivos y compuestos. El resultado es de tipo booleano.

* Los operadores lógicos (`\/`, `/\`, `!`, `==>`, `<==`) sólo pueden ser utilizados con operandos booleanos. El resultado es de tipo booleano.

* Los operadores de todas las expresiones se evalúan de izquierda a derecha.

* Precedencia de los operadores de mayor precedencia a la menor:

        ! - (unitario lógico y aritmético)
        * / mod max min (multiplicación, división, mod, máximo, mínimo)
        + - (suma, resta)
        < <= > >= (relacionales)
        == != (comparación)
        /\ \/ ("y" y "o" Lógico)
        ==> <== (implicación , consecuente)
        := (Asignación)

## Variables

* Las variables pueden ser de cualquier tipo primitivo o compuesto. Se declaran utilizando la palabra reservada `var`.

* No pueden existir ninguna variable con el mismo nombre de una constante que se encuentre en el mismo alcance, y viceversa.

* Las variables pueden inicializarse al mismo tiempo de que son declaradas.

* Las variables no deben ser inicializadas obligatoriamente, es responsabilidad del programador inicializar variables antes de ser utilizadas. En caso de no ser inicializadas tendrán el valor por defecto (`0`, `0.0`, `true`, `''`).

* Se pueden declarar una o más variables de un mismo tipo en la misma declaración, como también pueden ser inicializadas al mismo tiempo.

* El nivel de alcance es el **mismo** para variables declaradas en el mismo bloque.

* Se permite utilizar identificadores que fueron declarados en un alcance **distinto**.

* La declaración de variables es la siguiente:


		var <Identificador> : <Tipo> ;
        var <Identificador>, ..., <Identificador> : <Tipo>;
        var <Identificador>, <Identificador> ... := <Expresión>, <Expresión>, ... : <Tipo>;


## Constantes

* Las constantes pueden ser de cualquier tipo primitivo o compuesto. Se declaran utilizando la palabra reservada `var`.

* Las constantes son "variables" que solo se les asigna un valor y no pueden ser redefinidas en ningún momento. Se declaran utilizando la palabra reservada `const`.

* La inicialización de las constantes es **obligatoria**.

* La declaración de variables es la siguiente:


		const <Identificador> := <Expresión> : <Tipo>;
        const <Identificador>, <Identificador> ... :=<Expresión>, <Expresión>, ... :<Tipo>;



## Asignación

* Solo se puede asignar una expresión (`R-Value`) que sean del mismo tipo que la del variable (`L-Value`).

* Se permite hacer más de una asignación en una misma instrucción.

* Las asignaciones se hacen por valor y profundamente.

* Los arreglos pueden utilizarse como `L-Value` para sustituir un elemento especifico como también para sustituir todo el arreglo, en este ultimo caso el arreglo que se utilizara como `R-Value` debe ser de la misma dimensión necesariamente.

* No se pueden utilizar constantes como `L-Value`.

* Las asignaciones son de la siguiente forma:

		<Variable> := <Expresión>;
		<Variable>, ..., <Variable> := <Expresión>, ..., <Expresión>;
        <Variable>[<Numero>] := <Expresión>;




## Selector

* Existen dos tipo de selector, el primero el cual puede contener una o mas condiciones cada una con su respectiva secuencia de acciones, y el segundo que **solo** puede ser utilizado en el cuerpo de las funciones, puede contener una o mas condiciones cada una con su respectiva expresión que será el resultado de la función.

* El selector evalúa la expresión de los condicionales, en caso de que uno o mas sean verdaderos pasa a ejecutar las lista de acciones asociada a la **primera** que resulto verdadera, en caso de que ninguna sea verdadera entonces no se ejecutara ninguna y el programa terminara su ejecución con un mensaje de **"ABORT"**.


*  El selector es de la siguiente manera:


		if <Expresión> -> <Acciones>
        []  ...
        [] <Expresión> -> <Acciones>
        fi

        func <Identificador> : (<Parámetros>) -> <Tipo de Retorno>
            begin
                if <Expresión> -> <Expresión>
        		[]  ...
        		[] <Expresión> -> <Expresión>
        		fi
            end


## Iteración

* El iterador evalúa la expresión de los condicionales, en caso de que uno o mas sean verdaderos pasa a ejecutar las lista de acciones asociada a la primera que resulto verdadera y luego se vuelven a evaluar de nuevo todos los condicionales, en caso de que ninguna sea verdadera entonces no el ciclo termina.

* La expresión asociada a la palabra reservada `inv` representa la invariante que debe cumplir el ciclo en el inicio de cada iteración, si el invariante no se cumple el programa terminara su ejecución con un mensaje de  **"ABORT"**.

* La expresión asociada a la palabra reservada `bound` representa la cota que decrece en cada iteración del ciclo.

* La iteración acotada es la siguiente:


		 {inv <Expresion> inv}
         {bound <Expresion> bound}
 		do <Expresion> -> <Acciones>
         []  ...
         [] <Expresion> -> <Acciones>
         od


## Aserciones

#### Precondición

* La precondición corresponde a la condición que **deberían cumplir** los procedimientos definidos por el programador, su función es verificar que los valores de las variables de entrada y locales (Ya sean inicializadas, leídas de un archivo o entrada estándar) cumplan la expresión especificada.

* Si la precondición no se satisface se dará un mensaje de **advertencia** al programador y la postcondición del procedimiento **no** será verificada.


		{pre <Expresión> pre}


#### Postcondición

* La postcondición corresponde a la condición que **debe cumplir** los procedimientos definidos por el programador al final de su ejecución, por lo que determina el rango de resultados posibles que tendrá el procedimiento.

* Si la postcondición no se satisface el programa terminara su ejecución con su mensaje de error correspondiente, al menos que la precondición no fuera correcta.


		(post <Expresión> post)

#### Aserción

* La aserción corresponde a la condición que se debe cumplir en un momento específico del flujo del programa, ya sea de un procedimiento o del cuerpo principal del programa.

* El programador puede poner una aserción luego de cualquier instrucción dada, pero estas **no son** obligatorias.

* Si la aserción no se satisface el programa terminara su ejecución con su mensaje de error correspondiente.


		<Instrucción>
		{a <Expresión> a}


#### Invariantes

* La invariante corresponde a la condición que se **debe cumplir** tanto antes de entrar al ciclo como también después de cada iteración para verificar el buen funcionamiento del mismo, generalmente contiene un cuantificador.

* Si la invariante no se satisface el programa terminara su ejecución con su mensaje de error correspondiente.


#### Función de Cota

* La función de cota corresponde a la condición que garantiza que el ciclo terminara, por cada una de las iteraciones de un iterador se verificara que el valor sea menor al valor obtenido en el ciclo anterior, si no se cumple, el programa terminara su ejecución con su mensaje de error correspondiente.


* Las cotas también verifican que su valor actual no sea menor o igual a numero entero `0`, si no se satisface el programa terminara su ejecución con su mensaje de error correspondiente.



		{inv   <Expresión>   inv}
        {bound <Expresión> bound}
    	<Iteración>


## Funciones

* Las funciones tienen cero o más parámetros de entrada.

* Todos los parámetros de las funciones son pasados por valor.

* Se pueden retornar cualquier tipo existente en el lenguaje.

* En caso de que la función no retorne nada, se debe colocar `void` como tipo de retorno.

* El alcance entre las funciones es **distinto**, por lo que no se puede utilizar sus variables entre ellas.

* Las funciones pueden ser recursivas.

* La declaración de funciones es la siguiente:


		func <Identificador> : (<Parámetros>) -> <Tipo de Retorno>
          begin
              <Expresión>
          end

 * Los **parámetros** se definen de la siguiente manera:


		<Identificador> : <Tipo>


##### Llamada de funciones

* Se puede utilizar las funciones dentro de cualquier otra función, en procedimientos o en el programa principal.

* Las llamadas de funciones se utilizan como **expresiones** en el programa, por lo que solo pueden estar en un `R-Value`.

* La llama de funciones es de la siguiente manera:

        <Variable> := <Identificador>(<Parámetros>);



## Procedimientos

* Los procedimientos tienen cero o más parámetros de entrada.

* Todos los parámetros de los procedimientos pueden pasarse por valor o por referencia.

* Los parámetros de entrada de los procedimientos tienen el mismo alcance que las variables locales de los procedimientos.

* El alcance entre los procedimientos es **distinto**, por lo que no se puede utilizar sus variables entre ellos.

* Los procedimientos pueden ser recursivos.

* La declaración de los procedimientos es la siguiente:


		proc <Identificador> : (<Parámetros>)
        begin
			<Declaración de variables>
			<Entrada>
            <Precondición>
            |[
				<Acciones del procedimiento>
            ]|
            <Poscondición>
        end


* Los **parámetros** se definen de la siguiente manera:


		<Tipo de Parámetro> <Identificador> : <Tipo>


##### Tipos de parametros

* Los parámetros de entrada de los procedimientos tiene **tres** tipos de comportamiento distintos, `in`, `inout`, `out` y `ref`.

* Los `in` son parámetros pasados por **valor**, por lo que no pueden ser utilizados como `L-Value`.

* Los `out` son parámetros que no poseen ningún valor de entrada por lo tanto, para poder usarlos como `L-Value` deben de ser inicializados dentro del procedimiento; al finalizar el procedimiento se retorna el último valor asociado a la variable.

* Los `inout` son parámetros que poseen un valor de entrada y retornan su ultimo valor asociado al final del procedimiento.

* Los `ref` son parámetros pasados por **referencia** por lo tanto todo valor que se le asigné dentro del procedimiento también cambiara el valor de la variable que fue utilizada en la llamada del procedimiento.

* Es importante destacar que los arreglos **únicamente** pueden ser pasados como parámetro `ref`, por otro lado, los parámetros de tipo `out` e `inout` solo permiten pasar variables, por lo que no acepta ningún tipo de expresión.


##### Llamada de procedimientos

* Se puede utilizar los procedimientos dentro de cualquier otro procedimiento o en el programa principal.

* Las llamadas de procedimientos se utilizan como **instrucciones** en el programa, por lo que pueden ser usadas en una expresión.

* La llama de funciones es de la siguiente manera:

        <Identificador>(<Parámetros>);


## Cuantificadores

* Todo cuantificador comienza con el token reservado `(% ` y termina con `%)`.

* La **variable** cuantificada necesariamente debe de tipo booleana entera, carácter o booleana; como también debe estar presente en el rango especificado, al menos que sea el rango vacío.

* El **rango** .....

* La **expresión** corresponde a la verificación que se realizara para todo el rango dado por el programador.


		(%<Tipo de Cuantificador> <variable> : <Tipo> | <Rango> | <Expresión> %)

 ####Tipo de Cuantificadores

* **Universal (**`forall`**):**  Es utilizado para verificar que todos los elementos que componen el rango cumplen la expresión dada, la expresión necesariamente debe de ser tipo booleana. Si el cuantificador no se cumple se dará una **advertencia** al programador y el resultado será `false`, su expresión neutra es `True`.

* **Existencial (**`exist`**):** Es utilizado para verificar que al menos uno los elementos que componen el rango cumplen la expresión dada, la expresión necesariamente debe de ser tipo booleana. Si el cuantificador no se cumple se dará una **advertencia** al programador y el resultado será `false`, su expresión neutra es `True`.

* **Sumatoria (**`sigma`**):** Es utilizado para sumar **todos** los valores que resultan de la expresión en el rango dado, por lo que la expresión debe ser de tipo entero. Su expresión neutra es el numero entero `0`.


* **Productoria (**`pi`**):** Es utilizado para multiplicar **todos** los valores que resultan de la expresión en el rango dado, por lo que la expresión debe ser de tipo entero. Su expresión neutra es el numero entero `1`.


* **Máximo (**`max`**):**  Es utilizado para obtener el numero entero mayor de todos los resultados de la expresión verificada para el rango dado, la expresión necesariamente debe de ser tipo entera. Su expresión neutra es `MAX_INT`.


* **Mínimo (**`min`**):**  Es utilizado para obtener el numero entero menor de todos los resultados de la expresión verificada para el rango dado, la expresión necesariamente debe de ser tipo entera. Su expresión neutra es `MIN_INT`.




## Entrada

* Se utiliza la palabra reservada `read` para leer valor tanto del **pront** como para leer valores almacenados en un archivo.

* La entrada solo puede ser utilizada dentro de un **procedimiento** antes de su respectiva precondición y luego de la declaración de variables locales.

* Para poder leer de un archivo se utiliza la palabra reservada `with` seguido del nombre del archivo, mientras que para leer valores en el **pront** no se utiliza ninguna palabra reservada.

* La variable empleada para almacenar la información debe estar previamente declarada, también pueden ser utilizados los parámetros de entrada del procedimiento siempre y cuando **no** sean del tipo `in`.

* Por cada una de las variables que se utilicen como parámetros de la entrada, se leerá un valor de la entrada, los tipos de las variables necesariamente deben ser del mismo tipo de los valores ingresados.

* Los archivos que se utilicen para leer son abiertos y manejados por el mismo lenguaje por lo que el programador no tiene que manejar el descriptor de archivos, cuando un archivo es utilizado de nuevo por el programador el lenguaje prosigue a leer los valores que queden en el archivo.

* La entrada de datos se especifica como sigue:


		read(<Variables>);
        read(<Variables>) with <Nombre del Archivo>;


## Salida

* Para imprimir en la salida estándar se utiliza la palabra reservada `write`.

* Se pueden imprimir todos los tipos primitivos del lenguaje, como también se pueden imprimir cadenas de caracteres.

* Si se desea imprimir la información de una variable, ésta debe estar previamente declarada e inicializada.

* El lenguaje incorpora la función `writeln` que imprime un salto de línea inmediatamente después de la información indicada.

* La salida de datos se especifica como sigue:


        write(<Expresión>);
        writeln("Esto es una cadena de caracteres con un salto de linea");


## Funciones Predefinidas

### Conversión de Tipos

* `toInt` dado un parámetro de entrada de tipo flotante, retorna un numero entero asociado.

* `toDouble` dado un parámetro de entrada de tipo entero, retorna un numero entero asociado.

* `toChar` dado un parámetro de entrada de tipo entero o flotante, retorna un carácter asociado al numero.

* La conversión explicita de tipos es como sigue:


		toInt(<Expresión>);
		toDouble(<Expresión>);
		toString(<Expresión>);


### Otras funciones

#### Valor Absoluto

* `abs` dado una variable entera o flotante retorna el numero natural asociado.

#### Raíz  Cuadrada

* `sqrt` dado una variable entera o flotante retorna la raíz  cuadrada del numero, el resultado es de tipo flotante.

