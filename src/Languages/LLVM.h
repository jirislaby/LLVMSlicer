// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef LANGUAGES_LLVM_H
#define LANGUAGES_LLVM_H

#include <llvm/Module.h>
#include <llvm/Function.h>
#include <llvm/BasicBlock.h>
#include <llvm/Instruction.h>
#include <llvm/Instructions.h>
#include <llvm/Value.h>
#include <llvm/GlobalValue.h>
#include <llvm/Type.h>
#include <llvm/DerivedTypes.h>
#include <llvm/Intrinsics.h>
#include <llvm/ADT/SetVector.h>
#include <llvm/Support/InstIterator.h>
#include <llvm/Support/raw_os_ostream.h>

namespace llvm {

    bool hasExtraReference(llvm::Value const* const V);
    bool isConstantValue(llvm::Value const* const V);
    bool isPointerValue(llvm::Value const* const V);
    bool isPointerToPointerValue(llvm::Value const* const V);
    bool isPointerManipulation(llvm::Instruction const* const I);
    llvm::Type const* getPointedType(llvm::Value const* const V);
    llvm::Type *getPointedType(llvm::Type *T);
    const llvm::Type *getPointedType(const llvm::Type *T);
    bool isGlobalPointerInitialization(llvm::GlobalVariable const* const G);
    llvm::FunctionType const* getCalleePrototype(llvm::CallInst const* const C);
    bool isMemoryAllocation(llvm::Value const* const V);
    bool isMemoryDeallocation(llvm::Value const* const V);
    bool isMemoryCopy(llvm::Value const* const V);
    bool isMemoryMove(llvm::Value const* const V);
    bool isMemorySet(llvm::Value const* const V);
    bool memoryManStuff(llvm::Value const* const V);
    bool isInlineAssembly(const llvm::Value *V);
    bool isInlineAssemblyWithSideEffect(const llvm::Value *V);
    bool callToMemoryManStuff(llvm::CallInst const* const C);
    llvm::Instruction const *getFunctionEntry(const llvm::Function *F);
    template<typename OutIterator>
    void getFunctionCalls(llvm::Function const* const F, OutIterator out);
    template<typename OutIterator>
    void getFunctionCalls(llvm::Function* const F, OutIterator out);
    template<typename OutIterator>
    void getFunctionExits(llvm::Function const* const F, OutIterator out);
    template<typename OutIterator>
    void getFunctionExits(llvm::Function const* const F, OutIterator out);
    bool isLocalToFunction(llvm::Value const* const V,
                           llvm::Function const* const F);
    bool callToVoidFunction(llvm::CallInst const* const C);
    llvm::Instruction const* getSuccInBlock(llvm::Instruction const* const);
    const llvm::Value *elimConstExpr(const llvm::Value *V);
}

namespace llvm {

    template<typename OutIterator>
    void getFunctionCalls(llvm::Function* const F, OutIterator out)
    {
        for (llvm::inst_iterator i = llvm::inst_begin(F);
                i != llvm::inst_end(F); i++)
            if (llvm::CallInst const* c =
                    llvm::dyn_cast<llvm::CallInst const>(&*i))
		if (!isInlineAssembly(c))
		    *out++ = c;
    }

    template<typename OutIterator>
    void getFunctionCalls(llvm::Function const* const F, OutIterator out)
    {
        getFunctionCalls(const_cast<llvm::Function*>(F),out);
    }

    template<typename OutIterator>
    void getFunctionExits(llvm::Function* const F, OutIterator out)
    {
        for (llvm::inst_iterator i = llvm::inst_begin(F);
                i != llvm::inst_end(F); i++)
            if (llvm::ReturnInst const* r =
                    llvm::dyn_cast<llvm::ReturnInst const>(&*i))
                *out++ = r;
    }

    template<typename OutIterator>
    void getFunctionExits(llvm::Function const* const F, OutIterator out)
    {
        getFunctionExits(const_cast<llvm::Function*>(F),out);
    }

}

#endif
