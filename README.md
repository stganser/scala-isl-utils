# Utilities for the isl Scala Bindings

## Description

This project provides utilities for using isl in Scala. Included are type conversions for function parameters, type aliases and useful additional functionality that is not provided by isl itself.

## Legal

This program is part of project Polyite, which is released under the MIT license. It depends on isl, which is MIT licensed. Most work on the Scala isl
bindings utilities has been done by Stefan Kronawitter and Stefan Ganser.

## Setup

To use isl with Scala on Linux, first, get and build isl with the Scala/Java bindings.

1. Make sure you have libgmp and libclang (both including headers) installed on your system, as well as libtool

2. Get and build isl
```bash
cd ${BASE_DIR}
git clone https://github.com/stganser/isl.git
cd isl
mkdir install
export ISL_INSTALL="${PWD}/install"
./autogen.sh
./configure --prefix=${ISL_INSTALL} --with-jni-include=/usr/lib/jvm/default-java/include/ --with-clang=system
```

3. Generate the bindings
```bash
cd ${BASE_DIR}/isl/interface
make isl-scala.jar
cp -r java/gen src
cp scala/src/isl/Conversions.scala src/isl
zip -r isl-scala.jar src
```
The last three steps include the source code of the bindings into the generated library.

Create a directory libs inside the project's root directory and copy
libisl.so, libisl_jni.so and isl-scala.jar to libs. Make sure to include isl-scala.jar in your Java classpath and the shared objects in your Java library path:

    -Djava.library.path=scala-isl-utils/libs
    -classpath scala-isl-utils/libs/isl-scala.jar

(C) 2017 Stefan Ganser
