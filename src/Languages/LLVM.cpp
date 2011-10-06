// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

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

    bool isConstantValue(llvm::Value const* const V)
    {
        return /*llvm::isa<llvm::ConstantPointerNull const>(V) || WTF? */
               llvm::isa<llvm::Constant const>(V);
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

    bool isPointerManipulation(llvm::Instruction const* const I)
    {
        if (I->getOpcode() == llvm::Instruction::Load)
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
        }
        else if (llvm::CallInst const* const C =
                        llvm::dyn_cast<llvm::CallInst>(I))
        {
            return memoryManStuff(C->getCalledValue());
        }
        return false;
    }

    llvm::Type const* getPointedType(llvm::Value const* const V)
    {
        llvm::Type const* t =
            llvm::dyn_cast<llvm::PointerType const>(V->getType())
                    ->getElementType();
        if (hasExtraReference(V))
            t = llvm::dyn_cast<llvm::PointerType const>(t)->getElementType();
        return t;
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

    llvm::FunctionType const* getCalleePrototype(llvm::CallInst const* const C)
    {
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
                   F->getNameStr()=="malloc";
        return false;
    }

    bool isMemoryDeallocation(llvm::Value const* const V)
    {
        if (llvm::Function const* const F =
                llvm::dyn_cast<llvm::Function const>(V))
            return F->isDeclaration() && F->hasName() &&
                   F->getNameStr()=="free";
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

    bool callToMemoryManStuff(llvm::CallInst const* const C)
    {
        return memoryManStuff(C->getCalledValue());
    }

    llvm::Instruction const* getFunctionEntry(llvm::Function const* const F)
    {
        return &*F->getEntryBlock().begin();
    }

    llvm::Function const*
    getFunctionOfInstruction(llvm::Instruction const* const I)
    {
        return I->getParent()->getParent();
    }

    llvm::Function* getFunctionOfInstruction(llvm::Instruction* const I)
    {
        return I->getParent()->getParent();
    }

    bool isLocalToFunction(llvm::Value const* const V,
                           llvm::Function const* const F)
    {
        if (llvm::Instruction const* I =
                llvm::dyn_cast<llvm::Instruction const>(V))
            return getFunctionOfInstruction(I) == F;
        return false;
    }

    bool callToVoidFunction(llvm::CallInst const* const C)
    {
        return C->getType()->getTypeID() == llvm::Type::VoidTyID;
    }

    llvm::Instruction const* getSuccInBlock(llvm::Instruction const* const I)
    {
        llvm::BasicBlock::const_iterator it(I);
        return ++it == I->getParent()->end() ? 0 : &*it;
    }
#if 0
    std::string toString(llvm::Value const* const V)
    {
        std::stringstream sstr;
        dump(sstr,V);
        return sstr.str();
    }

    std::ostream& dump(std::ostream& ostr, llvm::Value const* const V)
    {
        MONTY_TMPROF_BLOCK_LVL1();

        if (llvm::Function const* const F =
                llvm::dyn_cast<llvm::Function const>(V))
            return dump(ostr,F);
        //else if (llvm::CallInst const* const C =
        //        llvm::dyn_cast<llvm::CallInst const>(V))
        //    return dump(ostr,C->getCalledValue()());

        if (llvm::Instruction const* const I =
                llvm::dyn_cast<llvm::Instruction const>(V))
            ostr << I->getParent()->getParent()->getNameStr() << "::";
        else if (llvm::Argument const* const A =
                    llvm::dyn_cast<llvm::Argument const>(V))
            ostr << A->getParent()->getNameStr() << "::";
        if (V->hasName())
        {
            ostr << V->getNameStr();
        }
        else
        {
            llvm::raw_os_ostream ros(ostr);
            ros << '{';
            V->print(ros);
            if (!llvm::isa<llvm::ConstantPointerNull const>(V))
                ros << "  ";
            ros << '}';
            ros.flush();
        }
        return ostr;
    }

    std::ostream& dump(std::ostream& ostr, llvm::Function const* const f)
    {
        MONTY_TMPROF_BLOCK_LVL1();

        llvm::raw_os_ostream ros(ostr);
        ros << "function ";
        f->getType()->print(ros);
        ros << ' ' << f->getNameStr();
        ros.flush();
        return ostr;
    }
#endif
}
