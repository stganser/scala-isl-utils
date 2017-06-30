# Utilities for the isl Scala Bindings

## Description

This project provides utilities for using isl in Scala. Included are type conversions for function parameters, type aliases and useful additional functionality that is not provided by isl itself.

## Legal

This program is part of project Polyite, which is released under the MIT license. It depends on isl, which is MIT licensed. Most work on the Scala isl
bindings utilities has been done by Stefan Kronawitter and Stefan Ganser.

## Setup

To use isl with Scala on Linux, first, get and build isl with the Scala/Java bindings.

```bash
git clone TODO
```

Create a directory libs inside the project's root directory and copy
libisl.so, libisl_jni.so and isl-scala.jar to libs. Make sure to include isl-scala.jar in your Java classpath and the shared objects in your Java library path:

    -Djava.library.path=scala-isl-utils/libs
    -classpath scala-isl-utils/libs/isl-scala.jar

(C) 2017 Stefan Ganser
