# Comentarios de la Prof. Rosseline Rodríguez, con meta-comentarios de EMHN.

Muchas gracias por tu resumen. Comento entre líneas y agrego los míos.

> Les escribo los detalles que vi que faltaron en el libro para que
> chequeen:
>
> 1) Decir que los cuantificadores permiten una sola variable, pero se
> hacer anidamiento de cuantificadores

Además:

* Comentar que los cuantificadores adicionales (sum, product,
  etc.) están presentes por conveniencia, pues son utilizados en muchos de
  los algoritmos empleados en la enseñanza. No los "inventamos" :)

* Dejar en claro que hace cada cuantificador en el caso vacío. Esto va
  tanto en el libro como en el manual de usuario.

> 2) Decir que el manejo de archivos es parte del diseño de uds. No
> está en GCL pero si en Graciela

Dejar en claro que lo que se quiere es evitar que el programador novel
tenga que familiarizarse con open-read/seek-close y combinarlo con
aserciones sobre lo leído.

> 3) El apartado "Errores de Ejecución"

De acuerdo.

> 4) Aclarar que el tamaño de los arreglos no siempre se conoce a
> tiempo de compilación

Cuando se conocen a tiempo de compilación, así son elaborados. Cuando
no, se elaboran a tiempo de ejecución y, al pasar por referencia,
operan como "Conformant Arrays" para que el procedimiento tenga acceso
al tamaño del arreglo.

> 5) Aclarar que la función de cota no es una condición sino una
> expresión entera cuyo valor se verifica que decrece en cada iteración

De acuerdo.

> 6) Decir qué hace el compilador si se pasa un arreglo como in, out o
> in-out

De acuerdo.

> 7) Decir que las variables se pueden inicializar dentro de la
> declaración

De acuerdo.

Además de las cosas que menciona la Prof. Rosseline tienen que:

* Revisar con *mucho* cuidado el uso de términos como "válido",
  "posible" y "cierto" para que sean adecuados en el contexto formal,
  tal como lo destacó la Prof. Carolina

* Explicar por qué se decidió incluir el 'abort'. Elaboren un
  argumento que combine la noción de que el condicional tiene un abort
  *implícito*, de modo que agregarlo de manera *explícita* no altera
  lo que ya el lenguaje provee, pero permite expresar programas en los
  cuales *todos* los caminos de cómputo estén *claramente*
  considerados (lo que explicó el Prof. Ricardo).

* Realicen una investigación sobre las implantaciones de GCL que
  existen, y determinen, en lo posible si están en uso o no. No harán
  la comparación de velocidad/modernidad, pero al menos debe mostrarse
  que estamos intentando una alternativa.

* Ordenar y comprobar que los programas de prueba del Kaldewaij
  funcionan. Entregarlos con el material de apoyo del libro.

Dejamos en suspenso:

* Hacer que el selector sea no-determinístico. Sabemos que no es
  difícil de implantar, pero nunca lo consideramos dentro del alcance.
  Es valioso como elemento didáctico, y si lo vamos a incorporar
  tendríamos que tener ambos comportamientos (con un flag a tiempo de
  compilación para indicar --deterministic o --non-deterministic), y
  además decidir cuál es el comportamiento estándar. Aquí es necesario
  tener opiniones de instructores de Algoritmos I para saber qué
  prefieren.

* Hacer que *si* se puedan redefinir variables en bloques anidados.

> Me parece que en el capítulo de Resultados deben decir que otro
> resultado es el compilador de Graciela, pareciera que sólo es el
> diseño del lenguaje.

A veces uno habla del lenguaje queriendo referirse al compilador, y a
veces uno habla del compilador queriendo referirse al lenguaje. En
efecto, hay dos resultados, el *lenguaje* Graciela, cuya evidencia
está escrita en los libros; y el *compilador* de Graciela, cuya
evidencia es el código fuente.

En la comunidad de Perl, usamos Perl para referirnos al lenguaje y
perl para referirnos al compilador/VM. ¿Qué tal si usan GraCieLa para
referirse al lenguaje y graciela para referirse al compilador?

Creo que no falta ninguna de las objeciones, porque yo tomé nota de
aquellas que hicieron Rosseline y Carolina. Si Ricardo tiene alguna, o
quiere aclarar más las que ya se han descrito, adelante.
