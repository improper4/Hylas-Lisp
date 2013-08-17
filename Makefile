CC = `(which clang; which gcc; which cc) | head -n 1`
CXX = `(which clang++; which g++) | head -n 1`
LISP = `(which sbcl; which clisp; which cmucl) | head -n 1`

ERRORFLAGS = -Wall -Wextra -Werror
OPTFLAGS = -O4
LLVMFLAGS = `llvm-config --cppflags --ldflags --libs core jit native asmparser asmprinter linker`

CFLAGS = -c -fPIC $(ERRORFLAGS) $(OPTOPTFLAGS)
CXXFLAGS = $(CFLAGS) -std=c++0x

DEFINES = -D__STDC_LIMIT_MACROS -D__STDC_CONSTANT_MACROS

default:console

backend:
	# Compile the helper Hylas<->LLVM interface
	$(CXX) src/jit.cpp -o libhylas.o $(CXXFLAGS) $(DEFINES)
	$(CXX) -shared libhylas.o -o libhylas.so $(OPT) $(LLVMFLAGS)
	rm libhylas.o
	# Now libhylas.so can be loaded

#liblinenoise:
#	# Compiles linenoise.c into liblinenoise.so so it can be loaded from Common Lisp
#	cd include/linenoise
#	$(CC) linenoise.c -o liblinenoise.o $(CFLAGS)
#	$(CC) -shared liblinenoise.o -o liblinenoise.so
#	rm liblinenoise.o

console: backend
	#$(CXX) $(SOURCES) -o hylas.o $(CXXFLAGS) $(LLVMFLAGS)

gui: backend
	make -C UI/syntagma

clean:
	rm libhylas.so

doc:
	make -C docs/

system:
	# Symlink the folder Hylas is in with Quicklisp's local-projects folder, to
	# make Hylas quickloadable as a library
	ln -s `pwd` ~/.quicklisp/local-projects/hylas
