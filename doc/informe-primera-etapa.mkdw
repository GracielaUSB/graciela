Universidad Simón Bolívar
Departamento de Computación y Tecnología de la Información
Proyecto: Implementación de un Compilador Nativo para GCL

#Informe  Étapa I

##Introducción
##Desarrollo

###Investigación Previa
Comenzamos llevando a cabo una investigación previa sobre el lenguaje GCL y en una implantación anterior de un compilador del lenguaje. Ésta fue hecha por un grupo dentro de la Universidad Simón Bolívar.

Con respecto al lenguaje GCL, es un lenguaje desarrollado por Edsger Dijkstra que permitía demostrar la validez de los programas mediante la forma en que afectaban el **estado de las variables** en el programa. El estado de las variables es el conjunto de todos los posibles valores que pueden tomar las variables definidas dentro del programa. Por ejemplo, en un programa con dos variables del tipo entero, tenemos el estado (5, 10) como un estado válido. En total tenemos ℤxℤ posibles estados.

Sobre el estado se definen **aserciones**. Estas aserciones son expresiones lógicas que permiten limitar el conjunto de estados. Usando el ejemplo anterior, sean *X* y *Y* el nombre de dos variables del tipo entero, la asercion *{ X > 0 ∧ Y < 0}* es válida sobre el conjunto de estados.

Cada programa escrito en GCL cuenta con una **precondición** y una **postcondicion**. La precondicion es una aserción sobre el estado del programa **antes** del comienzo de la ejecución. La postcondición es una aserción sobre el estado del programa después de la ejecución. En caso de demostrar la validez del programa, está garantizado que para cualquier estado definidido por la precondición, la ejecución del programa terminará en un estado que satisface la postcondicion.
 
La semántica del lenguaje se expresa en terminos de la **precondición más débil** del programa. La precondición mas débil representa a **todos** los estados para el cual la ejecución de algúna instrucción *S* termina en un estado que satisface una postcondición Q.

Para modificar el estado de las variables se usan **acciones**. Entre las distintas acciones tenemos:

* **Asignación**: La asignación es la única acción que modifica directamente el estado del programa. Permite sustituir el valor de una variable en el estado actual del programa. Se escribe de la siguiente forma:

        {P} x := E {Q} es equivalente a [P => Q(x := E)]
	
	En el estado *P* se sustituye el valor de la variable *X* por el valor de la expresión *E*.

Con respecto a la precondición más débil:

        wp('x := E', Q) == Q(x:=E)

Podemos verlo de la siguiente forma: La ejecución de la asignación terminará en un estado que satisface *Q* si *x* con el valor de *E* es un estado válido. 

* **Concatenación**: La concatenación de acciones permite construir una acción más grande a partir de otras más pequeñas. Sean, *S* y *T* acciones, podemos escribir la concatenación de la siguiente forma:

        {P} S ; T {Q} es equivalente a existe un estado R, tal que
        {P} S {R} y {R} T {Q}

	El *';'* se usa como **separador** de acciones.

  Precondición más débil:

        wp('S;T', Q) = wp('S', wp('T', Q))
        
La precondición más débil de T con la precondición Q sirve de postcondición para la precondición más débil de S.

* **Abort**:Se utliza para terminar con el programa. Si la ejecución termina alcanza un punto en el cual **abort** debe ser ejecutado, entonces el programa está erroneo.

* **Selección**: La selección permite ejecutar ciertas acciones dependiendo del estado del programa. Se escribe de la siguiente forma:

        if B0 -> S0
        [] Bi -> Si
        [] Bn -> Sn
        fi

    Los B~i~ -> S~i~se les llama **comandos con guardia** (guarded commands). Los S~i~ son acciones del lenguaje y los B~i~ son expresiones lógicas. Las guardias funcionan como condiciones sobre el conjunto de estados.

	Al momento de la ejecución se selecciona una guardia verdadera sobre el conjunto de estados para ejecutar su acción. En el caso que más de una guardia su cumpla se selecciona alguna al azar. En el caso que ninguna se cumpla el programa aborta, esto obliga al programador a escribir todos los escenarios en los cuales se ejecutaría la instrucción. Definir la selección de esta forma tiene la siguientes ventajas:

    *  Si más de una guardia es cierta entonces el programador deja a juicio del lenguaje cual guardia ejecutar y dependiendo del criterio usado se ejecuta cualquiera. En el caso que se implante un compilador para el lenguaje, se puede escoger la optimice la ejecución del programa. Para esos estados no importa cual acción se ejecute, cualquiera debe terminar en un estado válido para la continuación del programa.

    * Las principales ventajas de ejecutar el abort cuando ninguna guardia es cierta son las siguientes: 
        * Legilibilidad del código: Al obligar al programador a escribir todos los escenarios ayuda a cualquiera que lea el programa a entender bajo que casos se deben ejecutar las insrucciones definidas en el comando guardado. Incluso, pueden haber condiciones para las cuales no debe ejecutarse ninguna acción, para esto existe la acción **skip**, que será definida posteriormente.

        * Facilita el aprendizaje: Para los programadores novatos es bueno pensar en todos los estados para los cuales algún comando guardado debe ser ejecutado. El programador está forzado a entender el problema que desea solucionar y a desarollar todas las alternativas de ejecución del programa.


    Con respecto a la precondición más débil:
        
        wp(IF, Q) = (Ei: 1 <= i <= n: Bi) /\
                    (Ai: 1 <= i <= n: Bi => wp(Si, Q))

La cual podemos intepretar de la siguiente forma: Al momento de la ejecución alguna guardia debe ser cierta, esto lo aseguramos con la primera parte de la conjunción. La segunda parte asegura que para todas las guardias B~i~, el conjunto de estados que define a precondición más débil de S~i~ con la postcondicón de la selección, es subconjunto de aquel definido por B~i~. Es decir, cualquier guardia que se escoja terminará en un estado que satisface la postcondición.

**Repetición**: Nos permite ejecutar un conjunto de comandos con guardia de manera continua hasta que ninguna guardia se cumpla. Tiene la siguiente sintaxis:

    do B0 -> S0
    [] Bi -> Si
    od Bn -> Sn

Se comporta de la siguiente manera: Al momento de la ejecución se selecciona alguna guardia B~i~ que sea cierta para la ejecución de su conjunto de acciones S~i~. Después de la ejecución de S~i~, vuelve al principio y escoge otra guardia cierta y así sucesivamente hasta que ninguna guardia sea cierta. Si al momento de escoger la guardia hay más de una cierta se ejecuta cualquiera al azar. En cambio, si ninguna lo es, la instrucción skip es ejecutada.

Además, para asegurarnos de la validez debe proveerse un **invariante** y una **función de cota**. El invariante es una aserción que debe mantenerse antes y después de la ejecución de cada iteración, es decir, la ejecución de cualquier comando con guardia no destruye la validez del invariante. Y la función de cota es una función no negativa y decreciente en cada iteración, que nos garantiza la terminación del ciclo. 

Con respecto a la precondición más débil tenemos:

* wp(DO, Q) = (E k : 0 <= k : H~k~(Q))

* H~k~(Q) = H~0~(Q) \/ wp(IF, H~k-1~(Q)) para k > 0

* H~0~(Q) = !(Ei : 1 <= i <= n : Bi) /\ Q

Podemos intepretarla de la siguiente manera:

H~k~ (Q) representa la precondición más débil después al menos **k** selecciones de un comando con guardia, dejando al sistema en un estado que satisface Q.

Para *k = 0* la postcondición debe mantenerse y ningún comando guardado debe ser seleccionado para ejecución.

Para *k > 0*, tenemos dos casos. Aquel en que no existe ninguna guardia cierta para la ejecución, en cuyo caso el primer término se cumple. Y aquel en que al menos uno se cumple y debemos asegurarnos que la ejecución del comando con guardia seleccionado termina en un estado que permita la ejecución de *k-1* iteraciones nuevas, en cuyo caso el segundo término se cumple.


**Skip**: Es útil para cuando en ciertas condiciones explícitamente no se debe hacer nada. Por ejemplo, si tenemos un programa en el que queremos sumar un valor solo en el caso que una variable sea negativa, podemos escribir el siguiente programa:
    
	
		if (x < 0)  -> x = x + 10;
		[] (x >= 0) -> skip;
		fi

###Desarrollo Técnico

* **Variables y Anidamientos:** El lenguaje no permite ningún tipo de variables globales para evitar que el estudiante haga una mala práctica de ellas, debido a que suele suceder que los estudiantes como están aprendiendo a programar utilizan solamente variables globales para todas las variables que necesitan por la facilidad que les genera (gracias a su alcance global), pero esto es un **mal hábito** de programación que no les ayuda a entender el alcance y anidamiento de las variables. Por lo tanto, solo se permite el uso de **variables locales** a los procedimientos y para todo nuevo bloque de código, cada uno **define un alcance** por lo que todas las variables declaradas en él solo pueden ser visibles en los bloques de código que se encuentren **en su interior**, pero él no puede ver las variables locales a los bloques** de su interior**.

        |[     // Primer bloque de codigo
            var foo, bar : int;
            var qux : boolean;

            |[    // Segundo bloque de codigo
                const baz := 79.7, bee := 5.5 : double;
                var   aux := 49 : int;

                foo := aux;
            ]|
        ]|

    Las variables declaradas dentro del primer bloque tienen alcance en el segundo por lo tanto pueden ser utilizadas en el cómo se puede apreciar en este ejemplo, no obstante las variables del segundo no se pueden utilizar en el primer bloque, pero en el caso hipotético de existir un tercer bloque de código dentro del segundo, si se podrían utilizar (y así sucesivamente).


* **Redefinición de variables:** La redefiniciones de variables **no es válida**, si una variable se declaró previamente en el anidamiento actual no se puede declarar de nuevo una variable que tenga el **mismo nombre**, sin importar su tipo. Esta decisión se tomó para minimizar la cantidad de problemas y errores que pueda tener el estudiante al momento de programar, puesto que en muchas ocasiones no tienen un buen conocimiento sobre los **alcances de las variables** y creen estar usando una variable cuando en realidad están usando otra del mismo nombre.

        |[
            var foo, bar : int;
            var qux : boolean;

            |[
                const baz := 79.7, qux := 5.5 : double;
                var   bar := 49 : int;

                foo := bar;
            ]|
        ]|

    En el ejemplo anterior se declaró la variable *bar* en los dos bloques de código, lo que no está permitido en el lenguaje por lo tanto, el código daría un error al momento de compilarlo.

* **Lectura de variables:** Todos los datos que se deseen leer tanto de un archivo como atreves del prompt del sistema **solamente** se pueden realizar antes de colocar la precondición de un procedimiento,  por lo que no pueden hacerse directamente en un bloque de código o en una función del programa. Las variables que se van a utilizar para la lectura tienen que estar declaradas como **variables locales** del procedimiento o deben ser parámetros de entrada de tipo *out* o *inout*. Realizar la lectura solamente en este punto del programa nos asegura que para todo **valor específico** de lectura en las variables se podrá verificar en la precondición y siempre tendrá el mismo **resultado** al terminar el procedimiento y por lo tanto en su postcondición, teniéndose así un comportamiento **puro** dentro de los procedimientos.

        proc : foo(in bar : boolean, out qux : int)
        [
             var text   : string;
             var i := 0 : int;
             var a, b   : double;

             read(a, text, qux) with archivo1;
             (...)
        ]

    Como se puede apreciar las tres variables que se utilizan en la lectura fueron previamente declaradas dentro del procedimiento o son parámetros de entrada que permiten retorno de valores, luego de la lectura vendría el resto del procedimiento.

* **Funciones y Procedimientos:** Las funciones son **puras**, por lo tanto, no se permite el cambio de estados dentro de **ninguna** función, solo se puede emplear expresiones en ellas las cuales tendrán **un solo resultado** final del tipo especificado en la declaración de la función, en cambio, en los procedimientos se pueden emplear tanto expresiones como acciones del lenguaje como también permite tener **más de un resultado** gracias a los distintos comportamientos que pueden tener los parámetros de entrada.

        func : foo(op1 : int, op2 : int, op3 : int) -> int
        |[
            (op2 * op3) / op1
        ]|

    La función anterior no realiza ningún cambio de estados, solamente aplica una regla de tres sobre tres números dados, por lo que se considera una función pura.

* **Parámetros de entrada:** Debido a que las funciones son puras los parámetros de entrada no se pueden altera, son únicamente de **tipo lectura** y no debe especificarse con ninguna palabra reservada, mientras que los parámetros en los procedimientos tienen comportamientos distintos: los *in* que se comportan como una constante solo puede usarse su valor pero** nunca alterarse**, los *out* que no tiene ningún valor de entrada aunque si **retornan su ultimo valor asociado** al final del procedimiento, y por ultimo están los *inout* que tienen **las dos funciones**.

* **Tipos estrictos:** Nuestro lenguaje es de tipo estricto **fuertemente tipados**, como resultado toda variable dentro del programa tiene que **saberse su tipo asociado** a tiempo de compilación y **no se puede comportar como ningún otro tipo** distinto al de el sin antes usar una función de conversión de tipos suministrada por el lenguaje, un ejemplo de ello sería el tener que convertir un numero entero para poder usarlo dentro de una cadena de caracteres.

* **UTF-8:** El lenguaje permite que el programador aparte de usar el formato de codificación de caracteres **ASCII** pueda utilizar **UTF-8** para escribir los símbolos matemáticos, booleanos y cuantificadores. Permitiendo que el código sea más legible y elegante.

         ASCII: (sigma k | 0 <= k < 10 : sqrt(2)*2)
         UTF-8: (∑ k | 0 ≤ k < 10 : √2×2)


* **Alex/Happy:** En primera instancia como el lenguaje se implementara en **haskell** se consideró en emplear el **analizador lexicográfico** Alex para generar los tokens y el **analizador sintáctico** happy para crear el árbol de derivación, mas se terminó descartando su uso debido a que la recuperación de errores es muy pobre (por no decir nula) y una de las **principales metas** es tener un **recuperador de errores robusto** para poder dar mensajes de errores más precisos y detallados que puedan facilitar el aprendizaje al estudiante.

* **Parsec/Attoparsec:** En segunda instancia se consideró utilizar *parsec* o *attoparsec* pero este último, aunque es **más eficiente** que parsec, **no permite** utilizar transformadores de *monads* que brindan facilidades para el funcionamiento entre los distintos *parsers* del uso de la tabla de símbolos, anidamiento, entre otros. En cambio, *parsec* si permite el uso de trasformadores de *monads* y permite un excelente recuperador de errores para dar mensajes de errores lo más detallados posibles.

* **Gramática LL:** El lenguaje que se está diseñando es sencillo y no posee un grado de complejidad alto permitiendo el uso de una gramática LL que no tenga recursión izquierda y que todas las derivaciones posean *firsts* distintos (las dos necesarias para que sea una gramática de este tipo), la principal razón aparte de facilitar la recuperación de errores, es el mejoramiento de rendimiento que se tiene con esta gramática en la librería de *parsec*.



###Próximos Pasos

* **Pramación funcional avanzada (ci4251):** En el trimestre Abril-Julio 2015 abriran la materia *ci4251* que nos permitirá aprender las técnicas necesarias para continuar con el desarrollo del proyecto.

* **Parser:** Comenzar a escribir un parser para el lenguaje que nos permita hacer el análisis sintáctico y traducir los programas correctos a una representación de árbol intermedia. En caso contrario, éste parser debe reportar todos lo errores que encuentre.

* **Tabla de símbolos**: Durante esta etapa del proyecto desarrollamos una tabla de símbolos. El manejo de alcances de nuestro lenguaje  nos permite tener una representación sencilla mediante un árbol de diccionarios. Por cada alcance nuevo, agregamos una rama nueva al alcance actual y a ésta se comienzan a agregar los símbolos del nuevo alcance. 

* **Verificación de tipos:** Después de terminar con el análisis sintáctico, desarrollaremos un verificador de tipos estricto para nuestro lenguaje. Al igual que en la etapa anterior, éste deberá reportar todos los errores que encuentre.

* **Código de máquina**: Para ayudarnos a traducir nuestra representación intermedia a código de máquina, usaremos una herramienta llamada *LLVM*. Ésta define un lenguaje de representación intermedia en cual podemos traducir los programas escritos en el lenguaje y la herramienta genera el código necesario para ser ejecutado en varios modelos de CPU.

* **Debugger y Profiler**: Después de terminar con todas las etapas mencionadas anteriormente, esperamos agregar un *debugger* y un *profiler* a nuestro lenguaje. 

##Conclusión