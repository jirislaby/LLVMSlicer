// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include <algorithm>
#include <iterator>

#include "llvm/IR/Constant.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"

#include "../Callgraph/Callgraph.h"
#include "../PointsTo/PointsTo.h"
#include "Modifies.h"

using namespace llvm;

namespace llvm { namespace mods {

  ProgramStructure::ProgramStructure(Module &M) {
    for (Module::iterator f = M.begin(); f != M.end(); ++f)
      if (!f->isDeclaration() && !memoryManStuff(&*f))
        for (inst_iterator i = inst_begin(*f); i != inst_end(*f); ++i)
          if (const StoreInst *s = dyn_cast<StoreInst>(&*i)) {
            const Value *l = elimConstExpr(s->getPointerOperand());
	    this->getContainer()[&*f].push_back(ProgramStructure::Command(
		  hasExtraReference(l) ? CMD_VAR : CMD_DREF_VAR, l));
          }
  }

  const Modifies::ModSet &getModSet(const llvm::Function *const &f,
	    const Modifies &S) {
    static const Modifies::ModSet empty;
    const Modifies::const_iterator it = S.find(f);

    return (it == S.end()) ? empty : it->second;
  }


  void computeModifies(const ProgramStructure &P,
	const callgraph::Callgraph &CG, const ptr::PointsToSets &PS,
	Modifies &MOD) {
    typedef ptr::PointsToSets::Pointee Pointee;

    for (ProgramStructure::const_iterator f = P.begin(); f != P.end(); ++f)
      for (ProgramStructure::mapped_type::const_iterator c = f->second.begin();
	   c != f->second.end(); ++c)
	if (c->getType() == CMD_VAR) {
	  if (!isLocalToFunction(c->getVar(), f->first))
	      MOD[f->first].insert(Pointee(c->getVar(), -1));
	} else if (c->getType() == CMD_DREF_VAR) {
	  typedef ptr::PointsToSets::PointsToSet PTSet;
	  const PTSet &S = ptr::getPointsToSet(c->getVar(), PS);

	  for (PTSet::const_iterator p = S.begin(); p != S.end(); ++p)
	    if (!isLocalToFunction(p->first, f->first) &&
			    !isConstantValue(p->first))
	      MOD[f->first].insert(*p);
	}

    typedef callgraph::Callgraph Callgraph;
    for (Callgraph::const_iterator i = CG.begin_closure();
	  i != CG.end_closure(); ++i) {
      const Modifies::mapped_type &src = MOD[i->second];
      typedef Modifies::mapped_type dst_t;
      dst_t &dst = MOD[i->first];

      std::copy(src.begin(), src.end(), std::inserter(dst, dst.end()));
#if 0 /* original boost+STL uncompilable crap */
      using std::tr1::bind;
      using std::tr1::placeholders::_1;
      using std::tr1::cref;
      dst.erase(std::remove_if(dst.begin(), dst.end(),
		bind(&ProgramStructure::isLocalToFunction, cref(P), _1, i->first)),
		dst.end());
#endif
      for (dst_t::iterator I = dst.begin(), E = dst.end(); I != E; ) {
	if (isLocalToFunction(I->first, i->first))
	  dst.erase(I++);
	else
	  ++I;
      }
    }

#ifdef DEBUG_DUMP
    errs() << "\n==== MODSET DUMP ====\n";
    for (ProgramStructure::const_iterator f = P.begin(); f != P.end(); ++f) {
	const Function *fun = f->first;
	const Modifies::ModSet &m = MOD[fun];

	errs() << fun->getName() << "\n";
	for (Modifies::ModSet::const_iterator I = m.begin(), E = m.end(); I != E; ++I) {
	    const Instruction *val = dyn_cast<Instruction>(I->first);
	    errs() << "\tFUN=" << val->getParent()->getParent()->getName() <<
		" OFF=" << I->second << " ";
	    val->dump();
	}
    }
    errs() << "==== MODSET END ====\n";
#endif
  }

}}
