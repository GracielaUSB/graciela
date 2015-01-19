# Especificación Formal de GCL

## Especificación de programas

Los programas en GCL mantienen la siguiente estructura:

	[
  		<declaracion de constantes>
  		<declaracion de variables>
  		{Precondicion}
  		<lista de instrucciones>
  		{Postcondicion}
  	]

##Declaración de constantes y  variables

Las constantes y variables definen el espacio de estados sobre el que trabajará el programa. Éstas asocian un nombre a un tipo de datos.

Los tipos de datos son conjuntos de valores que cuentan con operaciones definidas. El lenguaje GCL consta de los siguientes:

 - Enteros (int): Está conformado por los números énteros. 
 - Reales (float): Contiene a los números reales.
 - Lógicos (boolean): Lo conforman los valores lógicos True y False.
 - Caracteres (char): Incluye a todas las letras del alfabeto latino, tanto en su forma mayúscula como minúscula.
 - Secuencia de valores: Podemos definirlo como una función sobre un subconjunto consecutivo de los números enteros a un conjunto de valores pertenecientes a otro tipo del lenguaje. Sean p <= q, tales que p <= i < q y denotamos [p,q) como la secuencia consecutiva de todos los *i*. La declaración:  ***f : secuencia [p,q) de enteros***, define una variable con una función asociada: [p,q) -> Z.
 
El valor asociado a una variable declarada constante no puede cambiar durante la ejecución del programa.

## Precondicion y postcondición

Las precondición y postcondición son expresiones lógicas que expresan las condiciones que satisface el espacio de estados de entrada y salida, respectivamente.

Las expresiones lógicas estás conformada por operadores lógicos y cuantificadores.

### Operadores Lógicos

Incluye aquellos operadores definidos sobre los valores lógicos *True* y *False*. Entre estos tenemos:
- P ∧ Q (conjunción) 	  = El predicado ∧ evalúa a *True* cuando P y Q evalúan a *True*. Evalúa a *False* en cualquier otro caso.
- P ∨ Q (disjunción) 	  = El predicado ∨ evalúa a *False* cuando P y Q evalúan a *False*. Evalúa a *True* en cualqier otro caso.
- P ≡ Q (equivalencia)  = El predicado ≡ es el predicado que evalúa a *True* cuando ambos P y Q evalúa al mismo valor. Evalúa a *False* en cualquier otro caso. 
- P ⇒ Q (implicación)	  = Es el predicado que evalúa a *False* cuando P evalúa a *True* y Q a *False*. Evalúa a *True* en cualquier otro caso.
-   ¬ P (negación)		  = Es el predicado qe evalúa a *True* cuando P es *False*; evalúa a *False* cuando P es *True*.

### Cuantificadores

En general podemos escribir cuantificadores de la forma:

> (+x : R(i) : F(i)) 
 
donde:

* \+ es un cualquier operador que cumple con las siguientes caracteristicas:

	+ Conmutativo = x + y = y + x
	+ Asociativo = (x + y) + z = x + (y + z)
	+ Tiene una identidad *e* tal que: e + x = x + e = x

* R(i) es un predicado llamado *rango de cuantificacion*.
* F(i) es el *termino* y debe estar definido para todos los valores que satisfacen el *rango de cuantificacion*.

Entre los cuantificadores mas usados tenemos:

| Cuantificador | Formula | Identidad |
|---------------|---------|-----------|
| Existencial | (∃ i : R(i) : F(i)) = F(m) ∨ F(m+1) ∨ ... ∨ F(n)     |  False.
| Universal   | (∀ i : R(i):F(i)) = F(m) ∧ F(m+1)  ∧ ... ∧ F(n)      |  True.
| Sumatoria   | (∑ i : R(i): F(i)) = F(m) + F(m+1) + ... + F(n)      |  0.
| Productoria | (∏ : R(i)) : F(i) = F(m) + F(m + 1) + ... + F(n)     |  1.      
| Conteo      | (#i: R(i) :  F(i)) = (E i : R(i) ∧ F(i) : 1)         |  0.
| Maximo      | (max i : R(i) : F(i)) = max(F(m), F(m+1)), ... F(n)) |  max(F(i)).
| Minimo      | (min i : R(i) : F(i)) = min(F(m), F(m+1), .. F(n))   |  min(F(i)). 

##Intrucciones

### Skip

La ejecución de la instrucción **skip** no tiene efecto alguno en el estado actual de las variables.

### Asignación

Cualquier cambio en el estado del programa es debido a la ejecución de una intrucción de asignación.

La asignación es de la forma

>    	x := E

>	Donde x es una variable del programa y E es una expresión del mismo tipo de E.

Operacionalmente, su interpretación es: reemplaza el valor de la variable x por el valor de E.

### Secuenciación

La secuenciación permite definir un orden de ejecución de acciones.

Se denota por

>		S;T

Y se interpreta de la siguiente forma: S es ejecutada primero e inmediatamente comienza la ejecución de T.

###Selección

Se escribe de la siguiente forma:

>	**if B~0~ -> S~0~ [] ... [] B~n~ -> S~n~ fi**
    En la cual, 0<= i <= n, *B~i~* es una guardia (expresión lógica) y *S~i~* es una instrucción. Ademas, las construcciones *B~i~ -> S~i~* son llamadas **comandos guardados** (guarded commands).

Podemos interpretar la selección de la siguiente manera: Al momento de la ejecución todas las guardias son evaluadas, si ninguna de las guardias es cierta el programa aborta; en caso contrario, una de las guardias que evaluaron a cierto es seleccionada de manera *no determínistica* y la instrucción correspondiente es ejecutada.

### Iteración

La iteración nos permite seleccionar acciones que pueden ser ejecutadas una o más veces.

Tiene la forma:

> **do B~0~ -> S~n~ [] ... [] B~n~ -> S~n~**  
> donde, 0<= i <= n, *B~i~* es una guardia y *S~i~* es una acción del lenguaje.

Se puede interpretar de la siguiente manera:

Al momento de la ejecucion, se evaluan todas las guardias. Si todas las guardias evaluan a falso, entonces se ejecuta la instruccion **skip**; en caso contrario, una de las guardias que evaluó a cierto es seleccionada de forma *no deterministica* y su accion correspondiente es ejecutada. Posteriormente la iteración es ejecutada de nuevo.

### Reglas

### Skip

Regla:

> **{P} skip {Q} es equivalente a {P => Q}**

### Asignación

Regla:

>	**{P} x:= E {P => Q(x:=E)}**

### Secuenciación

Para probar su validez debemos encontrar un predicado R el cual ***{P} S {R}*** y  ***{R} T {Q}*** son ciertos. La ejecucion de S en el estado que satisface P termina en un estado R, y la ejecucion de T en el estado que cumple R termina en un estado que satisface Q.

Por lo cual terminamos con la siguiente regla:

> **{P} S;T {Q} **  
    es equivalente a  
    existe un predicado R para el cual
   **{P} S {T} y {R} T {Q}**

### Selección

Ahora podemos definir la regla de deduccion:

>	**{P} if B~0~ -> S~i~ [] ... [] B~n~ -> S~n~ fi {Q}**  
	es equivalente a  
	**P => B~0~ V ... V B~n~**  
	y  
	**{P /\\ B~0~} S~0~ {Q} /\\ ... /\\  {P /\\ B~n~} S~n~ {Q}**

### Definición formal de la gramática 

Usaremos la notación **wp(S,R)** para denotar la precondición más débil sobre el estado del sistema, tal que éste termina en un estado **R** después de la ejecución de la lista de acciones **S**.

wp() cumple con las siguientes propiedades:

* Ley del milagro excluido:
>	**wp(S,*False*) = *False* **
	La ejecucion de las acciones S sobre un conjunto de estados vacio, debe terminar en un conjunto de estados vacio.

* Propiedad de la monotonicidad
>	**Si Q ⇒ R, entonces
	wp(S,Q) ⇒ wp(S,R)** 
	Si el conjunto de estados descrito por el predicado Q es subconjunto del conjunto descrito por el predicado R, entonces  
el conjunto de estados compuestos por wp(S,Q) es subconjunto de wp(S,R).

* 3era propiedad
>	**(wp(S,R) ∧ wp(S,Q)) ≡ wp(S, R ∧ Q)**
En cualquier estado que satisface tanto wp(S,R) y wp(S,Q), la ejecucion de S solo puede transformarlo en un estado que satisface tanto R como Q.

* 4ta propiedad
>	**wp(S,Q) or wp(S,R) ⇒ wp(S, Q ∨ R)**
En Cualquier estado que puede satisfacer wp(S,Q) o wp(S,R), la ejecucion de S debe dejarlo en un estado que satisface Q o R. 

### Skip

> **wp(skip,Q) == Q**

### Asignación

>	**wp(x:=E, Q) == Q(x:=E)**

### Secuenciación

> **wp((S;T),Q) == wp(S, wp(T,Q))**

Se puede interpretar de la siguiente forma: La precondición más débil que satisface **{P} S;T {Q}** se obtiene encontrando el R mas débil tal que **{R} T {Q}** es verdadero, que luego es usado como postcondición para encontrar el P más débil tal que {P} S {R}.

### Selección

> **wp (if B~0~ -> S~0~ [] ... [] B~n~ -> S~n~, Q)**  
    es equivalente a  
   **(B~0~ \/ ... \/ B~n~) /\ (B0 => wp(S~0~,Q) /\ ... /\ wp(S~n~, Q))** 

En conclusión, *(B~0~ \/ ... \/ B~n~)* implica que al menos una de las guardias es seleccionada para la ejecución; mientras que  *(B0 => wp(S~0~,Q) /\ ... /\ wp(S~n~, Q))* implica que sin importar lan guardia que fue seleccionada, el conjunto de estados descrito por B~i~ debe ser subconjunto del estado descrito por wp(S~i~, Q); esto garantiza que la ejecución de S~i~ termina en un estado que satisface Q.

### Iteración

Sea 
>	H~0~ ≡ R ∧ ¬ (∃ j: 1 <= j <= n: B~j~)
	y para k > 0
	H~k~ (R) = wp(if B~j~ -> S~i~ if, H~k-1~(R)) ∨ H~0~
	entonces
	wp(do B~j~ -> S~j~ od, R) ≡ (∃ k : k >= 0 : H~k~(R))

H~k~ es la precondición más débil tal que la ejecución *k* guardias termina en un estado que satisface R.

Para k = 0 es necesario que no sea seleccionada ninguna guardia para su ejecución, como lo expresa el segundo término. Además, es necesario que la postcondición se mantenga, para eso está el primer término.

Para k > 0, existen dos casos: ninguna guardia es seleccionada para ejecución y R debe mantenerse, por lo que está el segundo termino; o al menos una es seleccionada y su ejecución termina en un estado tal que k - 1 futuras ejecuciones de guardias terminen en un estado que satisface R. En otras palabras, la postcondición resultante de la ejecución de una guardia sirve de precondición para la ejecución de la siguiente hasta que ninguna guardia sea seleccionada.
