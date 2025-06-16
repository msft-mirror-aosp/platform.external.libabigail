CXXFLAGS = -g -fPIC
LDFLAGS = ${CXXFLAGS} -shared

all: old-lib.so new-lib.so
.PHONY: all     

old-file.o: file.cpp
	g++ ${CXXFLAGS} -o "$@" -c "$<"

new-file.o: file.cpp
	g++ ${CXXFLAGS} -DNEW_VERSION -o "$@" -c "$<"

old-lib.so: old-file.o
	g++ ${LDFLAGS} -o "$@" "$<"

new-lib.so: new-file.o
	g++ ${LDFLAGS} -o "$@" "$<"

check: old-lib.so new-lib.so
	abidiff $^
.PHONY: check

clean:
	${RM} -f *.so *.o
.PHONY: clean
