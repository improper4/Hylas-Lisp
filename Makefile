CXX = clang++ # >2012 >using gcc
CXXFLAGS = -c -fpic -std=c++0x -Wall -Wextra -Werror
OPT = -O0 -g
LLVMFLAGS = `llvm-config --cppflags --ldflags --libs core jit native asmparser asmprinter linker`
SOURCES = UI/console/console.cpp
MAKELIB = ar -cvq
default:console

backend:
	# Compile the helper Hylas<->LLVM interface
	$(CXX) src/jit.cpp -o src/libhylas.o $(CXXFLAGS) $(OPT)
	$(CXX) -shared src/libhylas.o -o src/libhylas.so $(LLVMFLAGS)
	rm src/libhylas.o
	# Now libhylas.so can be loaded

console: backend
	#$(CXX) $(SOURCES) -o hylas.o $(CXXFLAGS) $(LLVMFLAGS)

gui: backend
	make -C UI/syntagma

clean:
	rm src/libhylas.so

system:
	# Symlink the folder Hylas is in with Quicklisp's local-projects folder, to
	# make Hylas quickloadable as a library
	ln -s
