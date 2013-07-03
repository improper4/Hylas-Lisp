CXX = clang++ # >2012 >using gcc
CXXFLAGS = -std=c++0x -Wall -Wextra -Werror
OPT = -O0 -ggdb
LLVMFLAGS = `llvm-config --cppflags --ldflags --libs core jit native asmparser asmprinter linker` -ldl
SOURCES = UI/console/console.cpp
MAKELIB = ar -cvq
default:console

backend:
	# Compile the helper Hylas<->LLVM interface
	$(CXX) src/jit.cpp -o src/libhylas.o $(CXXFLAGS) $(OPT) $(LLVMFLAGS)
	$(MAKELIB) src/libhylas.a src/libhylas.o
	# Now libhylas.o can be loaded

console: backend
	#$(CXX) $(SOURCES) -o hylas.o $(CXXFLAGS) $(LLVMFLAGS)

gui: backend
	make -C UI/syntagma

clean:
	rm libhylas.o

system:
	# Symlink the folder Hylas is in with Quicklisp's local-projects folder, to
	# make Hylas quickloadable as a library
	ln -s
