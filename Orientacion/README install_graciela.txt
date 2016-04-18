
El archivo installGraCieLa.deb, es la primera version del instalador del lenguaje GraCieLa.

Esta primera version no posee la instalacion de Haskell, ni de los paquetes necesarios, unicamente coloca los archivos y carpetas necesarias para poder utilizar el compilador graciela en cualquier directorio. 

Dentro del .deb hay un script que se encarga de todas las llamadas necesarias para crear el ejecutable.


Para compilar un archivo:

graciela <nombreArchivo.gcl> 

donde puedes colocar los siguientes flags:

-o, para especificar el nombre del ejecutable que desees, en caso de no colocarlo el ejecutable sera nombrado "a".

-r, si deseas que imprima solo el primer error encontrado a tiempo de compilacion.


