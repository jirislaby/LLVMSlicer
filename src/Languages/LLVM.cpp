// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include "llvm/Constant.h"
#include "llvm/Constants.h"
#include "llvm/InlineAsm.h"
#include "llvm/Instructions.h"
#include "llvm/Value.h"

#include "LLVM.h"

namespace llvm {

    bool hasExtraReference(llvm::Value const* const V)
    {
        if (llvm::isa<llvm::AllocaInst const>(V))
            return true;
        if (llvm::isa<llvm::GlobalVariable const>(V))
            return true;
        if (llvm::isa<llvm::Function const>(V))
            return true;
        //if (llvm::isa<llvm::Argument const>(V))
        //    return true;
        return false;
        //return V->isDereferenceablePointer();
    }

    bool isConstantValue(llvm::Value const* const V) {
      return isa<ConstantPointerNull>(V) ||
        (isa<Constant const>(V) &&
         !isa<GlobalValue>(V) &&
         !isa<UndefValue>(V));
    }

    bool isPointerValue(llvm::Value const* const V)
    {
        if (!V->getType()->isPointerTy())
            return false;
        if (!hasExtraReference(V))
            return true;
        return llvm::dyn_cast<llvm::PointerType const>(V->getType())
                    ->getElementType()->isPointerTy();
    }

    bool isPointerToPointerValue(llvm::Value const* const V)
    {
        return isPointerValue(V) && getPointedType(V)->isPointerTy();
    }

    bool isPointerManipulation(llvm::Instruction const* const I) {
        if (isa<AllocaInst>(I)) {
          return false;
        } else if (I->getOpcode() == llvm::Instruction::Load)
        {
            if (llvm::dyn_cast<llvm::PointerType const>(
                        I->getOperand(0)->getType())->
                    getElementType()->isPointerTy())
                return true;
        }
        else if (I->getOpcode() == llvm::Instruction::Store)
        {
            if (I->getOperand(0)->getType()->isPointerTy())
                return true;
        }
        else if (I->getOpcode() == llvm::Instruction::BitCast)
        {
            if (I->getType()->isPointerTy() &&
                    I->getOperand(0)->getType()->isPointerTy())
                return true;
        }
        else if (I->getOpcode() == llvm::Instruction::GetElementPtr)
        //else if (llvm::GetElementPtrInst const* const gep =
        //            llvm::dyn_cast<llvm::GetElementPtrInst>(I))
        {
            return true;
        } else if (const llvm::CallInst *C =
                        llvm::dyn_cast<llvm::CallInst>(I)) {
          if (isInlineAssembly(C))
            return false;
          return memoryManStuff(C->getCalledValue());
        } else if (const PHINode *PHI = dyn_cast<PHINode>(I)) {
          return isPointerValue(PHI);
        } else if (const ExtractValueInst *EV =
                   dyn_cast<const ExtractValueInst>(I)) {
          return isPointerValue(EV);
        } else if (const InsertValueInst *IV =
                   dyn_cast<const InsertValueInst>(I)) {
          return isPointerValue(IV->getInsertedValueOperand());
        } else if (isa<IntToPtrInst>(I)) {
          return true;
        } else if (const SelectInst *SEL = dyn_cast<SelectInst>(I)) {
          if (isPointerValue(SEL))
            return true;
        }

        assert(!isPointerValue(I) &&
               "Instruction cannot be a of pointer type here!");

        return false;
    }

    llvm::Type const* getPointedType(llvm::Value const* const V)
    {
        const Type *t = getPointedType(V->getType());
        if (hasExtraReference(V))
            t = getPointedType(t);
        return t;
    }

    Type *getPointedType(Type *T) {
      return dyn_cast<PointerType>(T)->getElementType();
    }

    const Type *getPointedType(const Type *T) {
      return dyn_cast<PointerType>(T)->getElementType();
    }

    bool isGlobalPointerInitialization(llvm::GlobalVariable const* const G)
    {
        if (G->isDeclaration())
            return false;
        llvm::Value const* const op = G->getOperand(0);
        return op->getType()->isPointerTy() && (
                    hasExtraReference(op) ||
                    llvm::dyn_cast<llvm::PointerType const>(op->getType())
                            ->getElementType()->isFunctionTy()
                    );
    }

    llvm::FunctionType const* getCalleePrototype(const CallInst *C) {
	assert(!isInlineAssembly(C) && "Inline assembly is not supported!");

        if (llvm::Function const* const fn =
                llvm::dyn_cast<llvm::Function const>(C->getCalledValue()))
            return fn->getFunctionType();
        else
        {
            llvm::Type const* const fnT =
                llvm::dyn_cast<llvm::PointerType const>(
                    C->getCalledValue()->getType())
                        ->getElementType();
            return llvm::dyn_cast<llvm::FunctionType const>(fnT);
        }
    }

    bool isMemoryAllocation(llvm::Value const* const V)
    {
        if (llvm::Function const* const F =
                llvm::dyn_cast<llvm::Function const>(V))
            return F->isDeclaration() && F->hasName() &&
                   F->getName().equals("malloc");
        return false;
    }

    bool isMemoryDeallocation(llvm::Value const* const V)
    {
        if (llvm::Function const* const F =
                llvm::dyn_cast<llvm::Function const>(V))
            return F->isDeclaration() && F->hasName() &&
                   F->getName().equals("free");
        return false;
    }

    bool isMemoryCopy(llvm::Value const* const V)
    {
        if (llvm::Function const* const F =
                llvm::dyn_cast<llvm::Function const>(V))
                return F->getIntrinsicID() == llvm::Intrinsic::memcpy;
        return false;
    }

    bool isMemoryMove(llvm::Value const* const V)
    {
        if (llvm::Function const* const F =
                llvm::dyn_cast<llvm::Function const>(V))
                return F->getIntrinsicID() == llvm::Intrinsic::memmove;
        return false;
    }

    bool isMemorySet(llvm::Value const* const V)
    {
        if (llvm::Function const* const F =
                llvm::dyn_cast<llvm::Function const>(V))
                return F->getIntrinsicID() == llvm::Intrinsic::memset;
        return false;
    }

    bool memoryManStuff(llvm::Value const* const V)
    {
        return isMemoryAllocation(V) || isMemoryDeallocation(V) ||
               isMemoryCopy(V) || isMemoryMove(V) || isMemorySet(V);
    }

    bool isInlineAssembly(const Value *V) {
	if (const CallInst *c = dyn_cast<const CallInst>(V))
	    return c->isInlineAsm();
	return false;
    }

    bool isInlineAssemblyWithSideEffect(const Value *V) {
	if (const CallInst *c = dyn_cast<const CallInst>(V))
	    if (const InlineAsm *a = dyn_cast<const InlineAsm>(c->getCalledValue()))
		return a->hasSideEffects();
	return false;
    }

    bool callToMemoryManStuff(llvm::CallInst const* const C)
    {
        return memoryManStuff(C->getCalledValue());
    }

    llvm::Instruction const* getFunctionEntry(llvm::Function const* const F)
    {
        return &F->getEntryBlock().front();
    }

    bool isLocalToFunction(llvm::Value const* const V,
                           llvm::Function const* const F)
    {
        if (llvm::Instruction const* I =
                llvm::dyn_cast<llvm::Instruction const>(V))
            return I->getParent()->getParent() == F;
        return false;
    }

    bool callToVoidFunction(llvm::CallInst const* const C)
    {
      if (isInlineAssembly(C))
        return false;
      return C->getType()->getTypeID() == llvm::Type::VoidTyID;
    }

    llvm::Instruction const* getSuccInBlock(llvm::Instruction const* const I)
    {
        llvm::BasicBlock::const_iterator it(I);
        return ++it == I->getParent()->end() ? 0 : &*it;
    }

    const Value *elimConstExpr(const Value *V) {
      if (const ConstantExpr *CE = dyn_cast<ConstantExpr>(V)) {
        if (Instruction::isBinaryOp(CE->getOpcode()))
          return V;
        assert((CE->getOpcode() == llvm::Instruction::GetElementPtr ||
                CE->isCast()) &&
          "Only GEP or CAST const expressions are supported for now.");
        return elimConstExpr(CE->getOperand(0));
      }
      return V;
    }
}
