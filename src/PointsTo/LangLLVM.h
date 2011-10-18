// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef POINTSTO_LANGLLVM_H
#define POINTSTO_LANGLLVM_H

#include <map>
#include <iterator>

#include "llvm/Constants.h"

#include "../AnalysisProps.h"
#include "../Languages/LLVM.h"
#include "PointsTo.h"
#include "PredefContainers.h"
#include "RuleExpressions.h"

namespace llvm { namespace ptr { namespace detail {

    typedef std::multimap<llvm::FunctionType const*,llvm::Function const*>
            FunctionsMap;

    typedef std::multimap<llvm::FunctionType const*,llvm::CallInst const*>
            CallsMap;

    void buildCallMaps(llvm::Module const& M, FunctionsMap& F, CallsMap& C);
    RuleCode<llvm::Value const*> argPassRuleCode(llvm::Value const* const l,
                                                 llvm::Value const* const r);


    template<typename OutIterator>
    void toRuleCode(llvm::Value const* const V, OutIterator out)
    {
        if (llvm::Instruction const* const I =
                llvm::dyn_cast<llvm::Instruction const>(V))
        {
            if (I->getOpcode() == llvm::Instruction::Load)
            {
                llvm::Value const* const op = I->getOperand(0);
                if (hasExtraReference(op))
                    *out++ = ruleCode<LLVM>(ruleVar(V) = ruleVar(op));
                else
                    *out++ = ruleCode<LLVM>(ruleVar(V) = *ruleVar(op));
            }
            else if (I->getOpcode() == llvm::Instruction::Store)
            {
                llvm::Value const* const l = I->getOperand(1);
                llvm::Value const* const r = I->getOperand(0);
                if (!hasExtraReference(l))
                    if (hasExtraReference(r))
                        *out++ = ruleCode<LLVM>(*ruleVar(l) = &ruleVar(r));
                    else
                    {
                        if (llvm::isa<llvm::ConstantPointerNull const>(r))
                            *out++ = ruleCode<LLVM>(*ruleVar(l) = ruleNull(r));
                        else
                            *out++ = ruleCode<LLVM>(*ruleVar(l) = ruleVar(r));
                    }
                else
                    if (hasExtraReference(r))
                        *out++ = ruleCode<LLVM>(ruleVar(l) = &ruleVar(r));
                    else
                    {
                        if (llvm::isa<llvm::ConstantPointerNull const>(r))
                            *out++ = ruleCode<LLVM>(ruleVar(l) = ruleNull(r));
                        else
                            *out++ = ruleCode<LLVM>(ruleVar(l) = ruleVar(r));
                    }
            }
            else if (I->getOpcode() == llvm::Instruction::BitCast)
            {
                llvm::Value const* const op = I->getOperand(0);
                if (hasExtraReference(op))
                    *out++ = ruleCode<LLVM>(ruleVar(V) = &ruleVar(op));
                else
                    *out++ = ruleCode<LLVM>(ruleVar(V) = ruleVar(op));
            }
            else if (llvm::GetElementPtrInst const* const gep =
                        llvm::dyn_cast<llvm::GetElementPtrInst>(I))
            {
                llvm::Value const* const op = gep->getPointerOperand();
                if (hasExtraReference(op))
                    *out++ = ruleCode<LLVM>(ruleVar(V) = &ruleVar(op));
                else
                    *out++ = ruleCode<LLVM>(ruleVar(V) = ruleVar(op));
            }
            else if (llvm::CallInst const* const C =
                            llvm::dyn_cast<llvm::CallInst>(I))
            {
                if (isMemoryAllocation(C->getCalledValue()))
                    *out++ = ruleCode<LLVM>(ruleVar(V) = ruleAllocSite(V));
                else if (isMemoryDeallocation(C->getCalledValue()))
                    errs() << "KOKO\n";
//                    *out++ = ruleCode<LLVM>(ruleDeallocSite(V));
                else if (isMemoryCopy(C->getCalledValue()) ||
                            isMemoryMove(C->getCalledValue()))
                {
                    llvm::Value const* const l = I->getOperand(0);
                    llvm::Value const* const r = I->getOperand(1);
                    *out++ = ruleCode<LLVM>(*ruleVar(l) = *ruleVar(r));
                }
            }
        }
        else if (llvm::GlobalVariable const * const G =
                        llvm::dyn_cast<llvm::GlobalVariable>(V))
        {
            llvm::Value const* const op = G->getOperand(0);
            *out++ = ruleCode<LLVM>(ruleVar(V) = &ruleVar(op));
        }
    }

}}}


namespace llvm { namespace ptr {

    template<>
    struct MemoryLocation<LLVM>
    {
        typedef llvm::Value const* Type;
    };

    template<bool IsMayAnalysis, bool IsInterproc>
    struct LLVMProgramStructure : public
        ProgramStructureAsVector<LLVM,
            llvm::AnalysisProperties<IsMayAnalysis,IsInterproc,
                                        false,false,false,false>,
            RuleCode<MemoryLocation<LLVM>::Type> >
    {
        typedef llvm::AnalysisProperties<IsMayAnalysis,IsInterproc,
                                            false,false,false,false>
                AnalysisProperties;
        typedef RuleCode<MemoryLocation<LLVM>::Type> RuleCode_t;
        typedef ProgramStructureAsVector<LLVM,AnalysisProperties,RuleCode_t> Base;

        LLVMProgramStructure(Module &M);
    };

    template<bool IsMayAnalysis, bool IsInterproc>
    struct ProgramStructure<LLVM,AnalysisProperties<IsMayAnalysis,IsInterproc,
                                                    false,false,false,false> >
    {
        typedef LLVMProgramStructure<IsMayAnalysis,IsInterproc>
                Type;
    };

}}

namespace llvm { namespace ptr { namespace detail {

    template<typename OutIterator>
    void collectCallRuleCodes(llvm::CallInst const* const c,
                              llvm::Function const* f, OutIterator out)
    {
        if (memoryManStuff(f) && !llvm::isMemoryAllocation(f))
            return;
        if (llvm::isMemoryAllocation(f))
        {
            llvm::Value const* const V = c;
            *out++ = ruleCode<LLVM>(ruleVar(V) = ruleAllocSite(V));
        }
        else
        {
            llvm::Function::const_arg_iterator fit = f->arg_begin();
            for (std::size_t i = 0; fit != f->arg_end(); ++fit, ++i)
                if (llvm::isPointerValue(&*fit))
                    *out++ = detail::argPassRuleCode(&*fit,c->getOperand(i));
        }
    }

    template<typename OutIterator>
    void collectCallRuleCodes(llvm::CallInst const* const c,
                              FunctionsMap::const_iterator b,
                              FunctionsMap::const_iterator const e,
                              OutIterator out)
    {
        if (llvm::Function const* f = c->getCalledFunction())
            collectCallRuleCodes(c,f,out);
        else
            for ( ; b != e; ++b)
                collectCallRuleCodes(c,b->second,out);
    }

    template<typename OutIterator>
    void collectReturnRuleCodes(llvm::ReturnInst const* const r,
                                CallsMap::const_iterator b,
                                CallsMap::const_iterator const e,
                                OutIterator out)
    {
        if (r->getNumOperands() == 0 ||
                !llvm::isPointerValue(r->getOperand(0)))
            return;
        llvm::Function const* const f = r->getParent()->getParent();
        for ( ; b != e; ++b)
            if (llvm::Function const* g = b->second->getCalledFunction())
            {
                if (f == g)
                    *out++ =detail::argPassRuleCode(b->second,r->getOperand(0));
            }
            else
                *out++ = detail::argPassRuleCode(b->second,r->getOperand(0));
    }

}}}

namespace llvm { namespace ptr {

    template<bool IsMayAnalysis, bool IsInterproc>
    LLVMProgramStructure<IsMayAnalysis,IsInterproc>::LLVMProgramStructure(
            Module &M)
        : Base()
//        , module(M)
    {

        typedef llvm::Module::const_global_iterator GlobalsIter;
        for (GlobalsIter g = M.global_begin(); g != M.global_end(); ++g)
            if (llvm::isGlobalPointerInitialization(&*g))
                detail::toRuleCode(&*g,std::back_inserter(this->getContainer()));

        detail::FunctionsMap FM;
        detail::CallsMap CM;
        detail::buildCallMaps(M,FM,CM);

        typedef llvm::Module::const_iterator FunctionsIter;
        for (FunctionsIter f = M.begin(); f != M.end(); ++f)
        {
            typedef llvm::Function::const_iterator BasicBlocksIter;
            for (BasicBlocksIter b = f->begin(); b != f->end(); ++b)
            {
                typedef llvm::BasicBlock::const_iterator InstructionsIter;
                for (InstructionsIter i = b->begin(); i != b->end(); ++i)
                    if (llvm::isPointerManipulation(&*i))
                        detail::toRuleCode(&*i,
                                    std::back_inserter(this->getContainer()));
                    else if (llvm::CallInst const* const c =
                                llvm::dyn_cast<llvm::CallInst>(&*i))
                        detail::collectCallRuleCodes(c,
                            FM.lower_bound(llvm::getCalleePrototype(c)),
                            FM.upper_bound(llvm::getCalleePrototype(c)),
                            std::back_inserter(this->getContainer()));
                    else if (llvm::ReturnInst const* const r =
                                llvm::dyn_cast<llvm::ReturnInst>(&*i))
                    {
                        llvm::FunctionType const* const t =
                            r->getParent()->getParent()->getFunctionType();
                        detail::collectReturnRuleCodes(r,CM.lower_bound(t),
                                CM.upper_bound(t),
                                std::back_inserter(this->getContainer()));
                    }
            }
        }
    }

}}

#endif
