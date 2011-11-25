// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include "llvm/BasicBlock.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"

#include "RuleExpressions.h"

using namespace llvm;

namespace llvm { namespace ptr { namespace detail {

void buildCallMaps(Module const& M, FunctionsMap& F,
		CallsMap& C) {
    for (Module::const_iterator f = M.begin(); f != M.end(); ++f) {
	if (!f->isDeclaration())
	    F.insert(std::make_pair(f->getFunctionType(), &*f));
	for (Function::const_iterator b = f->begin(); b != f->end(); ++b) {
	    for (BasicBlock::const_iterator i = b->begin(); i != b->end(); ++i)
		if (const CallInst *CI = dyn_cast<CallInst>(&*i)) {
		    if (!isInlineAssembly(CI) && !callToMemoryManStuff(CI))
			C.insert(std::make_pair(getCalleePrototype(CI), CI));
		} else if (const StoreInst *SI = dyn_cast<StoreInst>(&*i)) {
		    const Value *r = SI->getValueOperand();
		    if (hasExtraReference(r) && memoryManStuff(r)) {
			const Function *fn = dyn_cast<Function>(r);
			F.insert(std::make_pair(fn->getFunctionType(), fn));
		    }
		}
	}
    }
}

RuleCode argPassRuleCode(Value const* const l,
			 Value const* const r)
{
    if (isa<ConstantPointerNull const>(r))
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

}}}
