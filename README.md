# Graciela

Proyecto creado por Joel Araujo y José Luis Jiménez.
Tutores: Ernesto Hernández-Novich y Ricardo Monascal.

- - -

* ***TO DO*** Hablar filosoficamente del lenguaje

El diseño del lenguaje está disponible [aquí](doc/gacela/diseno.md).

## Instalación Manual

Para instalar el compilador de Graciela en Linux, es necesario seguir estos
pasos:

* Obtener la plataforma Haskell 7.10.*, disponible en
  <https://www.haskell.org/platform/>.

* Instalar LLVM 3.5. En distribuciones basadas en Debian, esto puede hacerse
  con `# apt-get install llvm-3.5`. En otras distribuciones, la instrucción
  probablemente será la misma, cambiando `apt-get` por el administrador de
  paquetes relevante.

* Clonar este repositorio con
  `$ git clone git@github.com:GracielaUSB/gacela.git`.

* Para correr los ejecutables generados por el compilador es necesario compilar
  el archivo `source/auxiliarFunctions.c` y luego moverlo a
  `/lib/x86_64-linux-gnu`. Esto puede hacerse de la siguiente manera:

```
    # gcc -fPIC -shared source/auxiliarFunctions.c -o auxiliarFunctions.so
    # mv auxiliarFunctions.so /lib/x86_64-linux-gnu/
```

* Para evitar romper las bibliotecas de Cabal, es buena idea crear una
  *caja de arena*. Para esto, una vez dentro del directorio clonado, se ejecuta
  `$ cabal sandbox init`.

* Finalmente, se ejecuta `cabal install`. Una vez finalizado este paso (puede
  tomar tiempo mientras descarga las bibliotecas necesarias), el compilador
  estará en `<repositorio gacela>/cabal-sandbox/bin/gacela`.


## Instalación con Homebrew (OS X )

### Requisitos
* Command Line Tools o Xcode. Se puede descargar Xcode desde  <https://developer.apple.com/downloads> o solo los comandos necesarios desde la terminal usando:

      xcode-select --install

En caso de no tener [Homebrew](http://brew.sh), puede instalarse con el siguiente comando:

    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    
Una vez instalado Homebrew, se instala graciela con los siguientes comandos:

    brew tap adgalad/graciela
    brew install graciela

## Uso del compilador

Para compilar un archivo `.gcl`, se deben seguir estos pasos:

* Se usa el ejecutable generado en la sección anterior, seguido del nombre del
  archivo a compilar, y el número `0`, así:
  `# ./<directorios...>/gacela "<mi_programa>.gcl" 0`

* Luego, se compila el código LLVM generado en el paso anterior a código objeto,
  con el comando `$ llc-3.5 -filetype=obj "<mi_programa>.bc"`

* Ahora se debe enlazar el archivo objeto con la bibloteca en C incluida en este
  repositorio, con el comando
  `$ gcc "<mi_programa>.o" /lib/x86_64-linux-gnu/auxiliarFunctions.so -o <ejecutable_deseado>`

* Finalmente, se corre el ejecutable de la manera convencional, a saber,
  `$ ./<ejecutable>`.
