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

extern "C" {
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

    Code* hylas_error_code(const char* desc) {
        Code* code = new Code;
        code->error = desc;
        return code;
    }

    const char* get_error(Code* code) {
        return code->error;
    }

    Code* init_optimizer(Code* code) {

        code->passes.add(createBasicAliasAnalysisPass());
        code->passes.add(createInstructionCombiningPass());
        code->passes.add(createReassociatePass());
        code->passes.add(createGVNPass());
        code->passes.add(createCFGSimplificationPass());
        code->passes.add(createAggressiveDCEPass());
        return code;
    }

    Code* backend_init() {
        Code* code = new Code;
        code->error = NULL;
        InitializeNativeTarget();
        code->program = new Module("Hylas Lisp",Context);
        code->engine = ExecutionEngine::createJIT(code->program);
        code->loader = new Linker(code->program);
        //code->loader->addSystemPaths();
        code->engine =  EngineBuilder(code->program).create();
        return init_optimizer(code);
    }

    Code* jit_ir(Code* code, const char* ir) {
        SMDiagnostic errors;
        string parser_errors;
        ParseAssemblyString(ir,code->program,errors,Context);
        Function* entryfn = code->program->getFunction(StringRef("entry"));
        if(entryfn == NULL) {
            return hylas_error_code("ERROR: Couldn't find program entry point.");
        }
        if(!errors.getMessage().empty()) {
            entryfn->eraseFromParent();
            return hylas_error_code(errors.getMessage().data());
        }
        if(verifyModule(*code->program,ReturnStatusAction,&parser_errors)) {
            entryfn->eraseFromParent();
            return hylas_error_code(parser_errors.data());
        }
        code->passes.run(*code->program);
        return code;
    }

    char link(const char* lib) {
        using namespace llvm::sys;
        if(lib)
            return DynamicLibrary::LoadLibraryPermanently(lib);
        else
            return 0;
    }

    size_t nconcurrent() {
        return std::thread::hardware_concurrency();
    }

    const char* run_entry(Code* code) {
        Function* entryfn = code->engine->FindFunctionNamed("entry");
        if(entryfn == NULL)
          return NULL; //"Couldn't find program entry point."
        std::vector<GenericValue> args;
        GenericValue retval = code->engine->runFunction(entryfn,args);
        code->engine->freeMachineCodeForFunction(entryfn);
        entryfn->eraseFromParent();
        return (const char*)retval.PointerVal;
    }

    char delete_code(Code* code) {
        delete code;
        return 1;
    }

    const char* repeat(const char* in){
        return in;
    }
}
