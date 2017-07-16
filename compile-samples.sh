#!/bin/bash

java -cp build/frozen-0.0.1.jar:build/dependencies/* \
  com.xantoria.frozen.Main \
  samples/*.frz
