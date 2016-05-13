# GraCieLa 2

Extensión al proyecto creado por Joel Araujo y José Luis Jiménez,
por Moisés Ackerman y Carlos Spaggiari.
Tutores: Ernesto Hernández-Novich y Ricardo Monascal.

- - -

Objetivos planteados para la segunda fase del compilador GraCieLa

## Tipos de Dato (ADTs y DTs)

Son el objetivo primordial pues forman gran parte de los temas manejados en
Algoritmos y Estructuras II. Antes de cualquier implementación, se debe
definir cuál será la sintaxis para la definición y el uso de estas estructuras
dentro de un programa GraCieLa.

### ADT
Un ADT (Abstract Data Type) consiste en:

* Un conjunto de declaraciones
    * de constantes y variables que pueden ser de cualquier tipo básico o
      uno de los siguientes:
        * conjunto
        * multiconjunto
        * secuencia
        * función
        * relación
        * 'tipo de tipo' (sólo básicos y tuplas de un nivel, para polimorfismo)
        * tuplas

* Un invariante de representación, es decir, un predicado que debe cumplirse
  (i.e., verificarse) siempre.

* Firmas de métodos

Un ADT se asemeja a las clases declaradas como `abstract` en `Java`, y algunos
ejemplos serían `Lista`, `Pila`, y `Cola`

### DT
Un DT (Data Type) consiste en lo mismo que un ADT pero con los siguientes
cambios:

* Las declaraciones manejan otros tipos, a saber:
    * todos los básicos,
    * arreglo
    * apuntador
* Además del invariante de representación, tiene un invariante de
  acoplamiento, que se debe cumplir siempre, y asocia las variables y
  constantes del DT con las del ADT.
* Los métodos llevan código (que cumple con la firma del ADT correspondiente)

> Verificar el invariante de acoplamiento no es sencillo

-- *Ricardo Monascal*

## Algoritmos de ordenamiento

Esto *ya* puede hacerse con el compilador actual, pero lo listamos porque es
un objetivo relevante para Algoritmos II.

## Apuntadores

Para cumplir el objetivo 1, es necesario proveer apuntadores. Se discutieron
las siguientes consideraciones:

* No se permitirá la aritmética de apuntadores.

* Debe haber una palabra reservada para el apuntador nulo.

* Sólo apuntan al heap

* Se debe proveer un mecanismo de detección de apuntadores muertos,
  por ejemplo, la técnica de las lápidas.

* Se deben proveer palabras claves con los efectos de `new` y `free`.

El profesor Monascal recomendó investigar las aplicaciones de la *Lógica de
separación* para la definición de invariantes en presencia de apuntadores,
aunque no necesariamente será implementada, pues sería una lógica más para
estudiantes que apenas vieron Lógica Simbólica.

## Tipos Algebraicos Libres

Estos se parecen a los `data` de Haskell, el ejemplo típico es
> TAL Lista x = Nil | Cons x (Lista x)

Sería interesante proveerlos, pero no está claro como interactuarían con los
DTs y ADTs. Además, su uso en Algoritmos II es muy limitado y no se le sacaría
tanto provecho a este *feature*.

## Compilación separada

También se sugirió la posibilidad de que el código de un programa GraCieLa
se encuentre repartido entre varios archivos de código fuente. Esto sería
ventajoso para el profesor, que podría entregar un ADT que no revele ninguna
implementación para que el ejercicio sea construir un DT que cumpla con
la interfaz dada.
