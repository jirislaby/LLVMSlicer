// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include "llvm/BasicBlock.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"

#include "RuleExpressions.h"

using namespace llvm::ptr;

void llvm::ptr::detail::buildCallMaps(llvm::Module const& M, FunctionsMap& F,
		CallsMap& C) {
    typedef llvm::Module::const_iterator FunctionsIter;
    for (FunctionsIter f = M.begin(); f != M.end(); ++f)
    {
	if (!f->isDeclaration())
	    F.insert(std::make_pair(f->getFunctionType(),&*f));
	typedef llvm::Function::const_iterator BasicBlocksIter;
	for (BasicBlocksIter b = f->begin(); b != f->end(); ++b)
	{
	    typedef llvm::BasicBlock::const_iterator InstructionsIter;
	    for (InstructionsIter i = b->begin(); i != b->end(); ++i)
		if (llvm::CallInst const* const c =
			llvm::dyn_cast<llvm::CallInst>(&*i))
		{
		    if (!callToMemoryManStuff(c))
			C.insert(std::make_pair(getCalleePrototype(c),c));
		}
		else if (i->getOpcode() == llvm::Instruction::Store)
		{
		    llvm::Value const* const r = i->getOperand(0);
		    if (hasExtraReference(r) && memoryManStuff(r))
		    {
			llvm::Function const* const fn =
				llvm::dyn_cast<llvm::Function>(r);
			F.insert(std::make_pair(fn->getFunctionType(),fn));
		    }
		}
	}
    }
}

RuleCode argPassRuleCode(llvm::Value const* const l,
			 llvm::Value const* const r)
{
    if (llvm::isa<llvm::ConstantPointerNull const>(r))
	return ruleCode(ruleVar(l) = ruleNull(r));
    if (hasExtraReference(l))
	if (hasExtraReference(r))
	    return ruleCode(ruleVar(l) = ruleVar(r));
	else
	    return ruleCode(ruleVar(l) = *ruleVar(r));
    else
	if (hasExtraReference(r))
	    return ruleCode(ruleVar(l) = &ruleVar(r));
	else
	    return ruleCode(ruleVar(l) = ruleVar(r));
}
