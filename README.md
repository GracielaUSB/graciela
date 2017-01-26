# Graciela

Proyecto elaborado por:
* Joel Araujo y José Luis Jiménez (Version 1.0).
* Moises Ackerman y Carlos Spaggiari (Versión 2.0)

Tutores: Ernesto Hernández-Novich y Ricardo Monascal.

- - -

Graciela es un lenguaje de programación basado en el Guarded Command Language (GCL) de Edsger Dijkstra, 
realizado con el fin de ofrecer una herramienta práctica que permita introducir a la programación formal a
los alumnos de los cursos de Algoritmos y Estructuras I y II de la Universidad Simón Bolívar.

El diseño del lenguaje está disponible [aquí](doc/gacela/diseno.md).

## Instalación con APT (Linux 🐧)

***TO DO***

## Instalación con Homebrew (macOS )

### Requisitos
* **Homebrew**. Seguir las intrucciones en su [página web](http://brew.sh)

Una vez cumplido los requisitos, se puede instalar graciela con los siguientes comandos:

    $ brew tap GracielaUSB/graciela
    $ brew install graciela

## Uso del compilador

Para compilar un archivo `.gcl` y ejecutarlo, se utilizan los siguientes comandos:

    $ graciela <nombre_del_programa>.gcl
    $ ./<nombre_del_ejecutable>

Tambien se puede correr el programa sin crear el ejecutable usando el comando:

    $ rungraciela <nombre_del_programa>.gcl

