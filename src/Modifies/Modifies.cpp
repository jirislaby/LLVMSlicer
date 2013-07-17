// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include <algorithm>
#include <iterator>

#include "llvm/Constant.h"
#include "llvm/Instruction.h"
#include "llvm/Instructions.h"
#include "llvm/Module.h"

#include "../Callgraph/Callgraph.h"
#include "../PointsTo/PointsTo.h"
#include "Modifies.h"

using namespace llvm;
using namespace llvm::mods;

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

}}

void llvm::mods::computeModifies(const ProgramStructure &P,
      const callgraph::Callgraph &CG, const ptr::PointsToSets &PS,
      Modifies &MOD) {

  for (ProgramStructure::const_iterator f = P.begin(); f != P.end(); ++f)
    for (ProgramStructure::mapped_type::const_iterator c = f->second.begin();
	 c != f->second.end(); ++c)
      if (c->getType() == CMD_VAR) {
	if (!isLocalToFunction(c->getVar(),f->first))
	    MOD[f->first].insert(c->getVar());
      } else if (c->getType() == CMD_DREF_VAR) {
	typedef ptr::PointsToSets::PointsToSet PTSet;
	const PTSet &S = ptr::getPointsToSet(c->getVar(), PS);
	for (PTSet::const_iterator p = S.begin(); p != S.end(); ++p)
	  if (p->second == -1 && !isLocalToFunction(p->first, f->first) &&
			  !isConstantValue(p->first))
	    MOD[f->first].insert(p->first);
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
      if (isLocalToFunction(*I, i->first))
	dst.erase(I++);
      else
	++I;
    }
  }
}
