#!/bin/sh

gcc -g -shared -fPIC -o libbackcall.so backcall.c
gcc -g -shared -fPIC -L. -lbackcall -o libfoo.so libfoo.c
gcc -g -L . -lfoo -o app app.c

