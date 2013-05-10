// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include "../PointsTo/PointsTo.h"
#include "Callgraph.h"

using namespace llvm;
using namespace callgraph;

Callgraph::Callgraph(Module &M, ptr::PointsToSets const& PS) {
  typedef Module::iterator FunctionsIter;
  for (FunctionsIter f = M.begin(); f != M.end(); ++f)
    if (!f->isDeclaration() && !memoryManStuff(&*f))
      for (inst_iterator i = inst_begin(*f); i != inst_end(*f); i++)
	if (const CallInst *CI = dyn_cast<CallInst const>(&*i))
	  handleCall(&*f, CI, PS);

  detail::computeTransitiveClosure(directCallsMap, callsMap);
  for (const_iterator it = begin(); it != end(); ++it)
    directCalleesMap.insert(value_type(it->second,it->first));
  for (const_iterator it = callsMap.begin(); it != callsMap.end(); ++it)
    calleesMap.insert(value_type(it->second,it->first));
}

void Callgraph::handleCall(const Function *parent,
			   const CallInst *CI,
			   const ptr::PointsToSets &PS) {
  if (isInlineAssembly(CI))
    return;

  typedef SmallVector<const Value *, 10> CalledFunctions;
  CalledFunctions G;
  getCalledFunctions(CI, PS, std::back_inserter(G));

  for (CalledFunctions::const_iterator I = G.begin(), E = G.end();
       I != E; ++I) {
    const Function *called = dyn_cast<Function>(*I);
    if (!memoryManStuff(called) && !called->isDeclaration() &&
	!contains(parent, called))
      insertDirectCall(value_type(parent, called));
  }
}
