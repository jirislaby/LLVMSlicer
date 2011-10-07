// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef CALLGRAPH_LANGLLVM_H
#define CALLGRAPH_LANGLLVM_H

#include <boost/tr1/memory.hpp>
#include <vector>

#include "Callgraph.h"
#include "../Languages/LLVM.h"

namespace llvm { namespace callgraph {

    template<>
    struct ProgramFunction<LLVM>
    {
        typedef llvm::Function const*
                Type;
    };

    struct LLVMPCallgraph : public BasicCallgraph<LLVM>
    {
        typedef BasicCallgraph<LLVM> Base;
        typedef std::tr1::shared_ptr<llvm::Module> ModulePtr;

        template<typename PointsToSets>
        LLVMPCallgraph(Module &M, PointsToSets const& PS);
    };

    template<>
    struct Callgraph<LLVM>
    {
        typedef LLVMPCallgraph
                Type;
    };

}}

namespace llvm { namespace callgraph {

    template<typename PointsToSets>
    LLVMPCallgraph::LLVMPCallgraph(Module &M, PointsToSets const& PS)
        : Base()
    {
        typedef llvm::Module::iterator FunctionsIter;
        for (FunctionsIter f = M.begin(); f != M.end(); ++f)
            if (!f->isDeclaration() && !memoryManStuff(f))
                for (llvm::inst_iterator i = llvm::inst_begin(*f);
                        i != llvm::inst_end(*f); i++)
                    if (llvm::CallInst const* c =
                            llvm::dyn_cast<llvm::CallInst const>(&*i))
                    {
                        std::vector<llvm::Value const*> G;
                        if (c->getCalledFunction() != 0)
                            G.push_back(c->getCalledFunction());
                        else
                        {
                            typename PointsToSets::PointsToSet const& S =
                                getPointsToSet(c->getCalledValue(),PS);
                            std::copy(S.begin(),S.end(),std::back_inserter(G));
                        }
                        for (std::vector<llvm::Value const*>::const_iterator g =
                                G.begin(); g != G.end(); ++g)
                        {
                            llvm::Function const* const h =
                                llvm::dyn_cast<llvm::Function>(*g);
                            if (!memoryManStuff(h) && !h->isDeclaration())
                                insertDirectCall(value_type(f,h));
                        }
                    }

        computeRemainingDictionaries();
    }

}}

#endif
