#include <cstdlib>
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
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/IPO.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/ADT/Twine.h>
#include <llvm/Assembly/Parser.h>
#include <llvm/Support/DynamicLibrary.h>
#include <llvm/Support/Path.h>

using namespace std;
using namespace llvm;

struct arch {
    const char* vendor;
    const char* model;
};

LLVMContext& Context = getGlobalContext();

struct Code {
    Module* program;
    PassManager passes;
    ExecutionEngine* engine;
    Linker* loader;
    const char* error;
};

Code* error(Code* code, const char* desc) {
    code->error = desc;
    return code;
}

Code* init() {
    Code* code = new Code;
    InitializeNativeTarget();
    code->program = new Module("Hylas Lisp",Context);
    code->engine = ExecutionEngine::createJIT(code->program);
    code->loader = new Linker(code->program);
    //code->loader->addSystemPaths();
    code->engine =  EngineBuilder(code->program).create();
    return code;
}

void init_optimizer(Code* code) {
    code->passes.add(createBasicAliasAnalysisPass());
    code->passes.add(createInstructionCombiningPass());
    code->passes.add(createReassociatePass());
    code->passes.add(createGVNPass());
    code->passes.add(createCFGSimplificationPass());
    code->passes.add(createAggressiveDCEPass());
}

Code* jit(Code* code, const char* ir) {
    SMDiagnostic errors;
    string parser_errors;
    ParseAssemblyString(ir,code->program,errors,Context);
    llvm::Function* entryfn = code->engine->FindFunctionNamed("entry");
    if(entryfn == NULL)
      return error(code,"ERROR: Couldn't find program entry point.");
    if(!errors.getMessage().empty())
    {
      entryfn->eraseFromParent();
      return error(code,errors.getMessage().data());
    }
    if(verifyModule(*code->program,ReturnStatusAction,&parser_errors))
    {
      entryfn->eraseFromParent();
      return error(code,parser_errors.data());
    }
    code->passes.run(*code->program);
    return code;
}

const char* run_entry(Code* code) {
    llvm::Function* entryfn = code->engine->FindFunctionNamed("entry");
    if(entryfn == NULL)
      return NULL; //"Couldn't find program entry point."
    std::vector<GenericValue> args;
    GenericValue retval = code->engine->runFunction(entryfn,args);
    code->engine->freeMachineCodeForFunction(entryfn);
    entryfn->eraseFromParent();
    return (const char*)retval.PointerVal;
}

extern "C" {
    char link(const char* lib) {
        if(lib)
            return 1;
        else
            return 0;
    }

    size_t nconcurrent() {
        return std::thread::hardware_concurrency();
    }

    bool warm_shutdown(Code* code) {
        delete code;
        return true;
    }
}
