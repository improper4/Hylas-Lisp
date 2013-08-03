<#include <cstdlib>
#include <climits>
#include <cmath>
#include <fstream>
#include <sstream>
#include <string>
#include <csetjmp>
#include <climits>
#include <map>
#include <vector>
#include <string>
#include <algorithm>
#include <exception>
#include <thread>
#include <unistd.h>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/ExecutionEngine/ExecutionEngine.h>
#include <llvm/ExecutionEngine/JIT.h>
#include <llvm/ExecutionEngine/GenericValue.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/PassManager.h>
#include <llvm/Linker.h>
#include <llvm/Analysis/Verifier.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/Assembly/PrintModulePass.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/ADT/Twine.h>
#include <llvm/Assembly/Parser.h>
#include <llvm/Support/DynamicLibrary.h>
#include <llvm/Support/Path.h>

struct arch {
    char* vendor;
    char* model;
};

extern "C" {
    char link(char* lib) {
        return 1;
    }

    size_t nconcurrent() {
        return std::thread::hardware_concurrency();
    }

    char* getarch() {
        llvm::TargetMachine* = llvm::getTarget();
    }
}
