// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef POINTSTO_LANGLLVM_H
#define POINTSTO_LANGLLVM_H

#include <map>
#include <iterator>

#include "llvm/Constants.h"

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
    RuleCode argPassRuleCode(llvm::Value const* const l,
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
                    *out++ = ruleCode(ruleVar(V) = ruleVar(op));
                else
                    *out++ = ruleCode(ruleVar(V) = *ruleVar(op));
            }
            else if (I->getOpcode() == llvm::Instruction::Store)
            {
                llvm::Value const* const l = I->getOperand(1);
                llvm::Value const* const r = I->getOperand(0);
                if (!hasExtraReference(l))
                    if (hasExtraReference(r))
                        *out++ = ruleCode(*ruleVar(l) = &ruleVar(r));
                    else
                    {
                        if (llvm::isa<llvm::ConstantPointerNull const>(r))
                            *out++ = ruleCode(*ruleVar(l) = ruleNull(r));
                        else
                            *out++ = ruleCode(*ruleVar(l) = ruleVar(r));
                    }
                else
                    if (hasExtraReference(r))
                        *out++ = ruleCode(ruleVar(l) = &ruleVar(r));
                    else
                    {
                        if (llvm::isa<llvm::ConstantPointerNull const>(r))
                            *out++ = ruleCode(ruleVar(l) = ruleNull(r));
                        else
                            *out++ = ruleCode(ruleVar(l) = ruleVar(r));
                    }
            }
            else if (I->getOpcode() == llvm::Instruction::BitCast)
            {
                llvm::Value const* const op = I->getOperand(0);
                if (hasExtraReference(op))
                    *out++ = ruleCode(ruleVar(V) = &ruleVar(op));
                else
                    *out++ = ruleCode(ruleVar(V) = ruleVar(op));
            }
            else if (llvm::GetElementPtrInst const* const gep =
                        llvm::dyn_cast<llvm::GetElementPtrInst>(I))
            {
                llvm::Value const* const op = gep->getPointerOperand();
                if (hasExtraReference(op))
                    *out++ = ruleCode(ruleVar(V) = &ruleVar(op));
                else
                    *out++ = ruleCode(ruleVar(V) = ruleVar(op));
            }
            else if (llvm::CallInst const* const C =
                            llvm::dyn_cast<llvm::CallInst>(I))
            {
                if (isMemoryAllocation(C->getCalledValue()))
                    *out++ = ruleCode(ruleVar(V) = ruleAllocSite(V));
                else if (isMemoryDeallocation(C->getCalledValue()))
                    errs() << "KOKO\n";
//                    *out++ = ruleCode(ruleDeallocSite(V));
                else if (isMemoryCopy(C->getCalledValue()) ||
                            isMemoryMove(C->getCalledValue()))
                {
                    llvm::Value const* const l = I->getOperand(0);
                    llvm::Value const* const r = I->getOperand(1);
                    *out++ = ruleCode(*ruleVar(l) = *ruleVar(r));
                }
            }
        }
        else if (llvm::GlobalVariable const * const G =
                        llvm::dyn_cast<llvm::GlobalVariable>(V))
        {
            llvm::Value const* const op = G->getOperand(0);
            *out++ = ruleCode(ruleVar(V) = &ruleVar(op));
        }
    }

}}}


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
            *out++ = ruleCode(ruleVar(V) = ruleAllocSite(V));
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

#endif
