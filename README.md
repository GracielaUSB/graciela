# GraCieLa

Proyecto creado por Joel Araujo y José Luis Jiménez.
Tutores: Ernesto Hernández-Novich y Ricardo Monascal.

- - -

* ***TO DO*** Hablar filosoficamente del lenguaje

El diseño del lenguaje está disponible [aquí](doc/graciela/diseno.md).

## Instalación

Para instalar el compilador de GraCieLa en Linux, es necesario seguir estos
pasos:

* Obtener la plataforma Haskell 7.10.*, disponible en
  <https://www.haskell.org/platform/>.

* Instalar LLVM 3.5. En distribuciones basadas en Debian, esto puede hacerse
  con `# apt-get install llvm-3.5`. En otras distribuciones, la instrucción
  probablemente será la misma, cambiando `apt-get` por el administrador de
  paquetes relevante.

* Clonar este repositorio con
  `$ git clone git@github.com:GracielaUSB/graciela.git`.

* Para correr los ejecutables generados por el compilador es necesario compilar
  el archivo `source/graciela-lib.c` y luego moverlo a
  `/lib/x86_64-linux-gnu`. Esto puede hacerse de la siguiente manera:

```
    # gcc -fPIC -shared source/graciela-lib.c -o graciela-lib.so
    # mv graciela-lib.so /lib/x86_64-linux-gnu/
```

* Para evitar romper las bibliotecas de Cabal, es buena idea crear una
  *caja de arena*. Para esto, una vez dentro del directorio clonado, se ejecuta
  `$ cabal sandbox init`.

* Finalmente, se ejecuta `cabal install`. Una vez finalizado este paso (puede
  tomar tiempo mientras descarga las bibliotecas necesarias), el compilador
  estará en `<repositorio graciela>/cabal-sandbox/bin/graciela`.


## Cómo compilar un archivo 

Basta con ejecutar el comando `graciela` de la siguiente forma:

    graciela <mi_programa>.gcl 
