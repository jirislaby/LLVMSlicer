// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef LANGUAGES_LLVMSUPPORT_H
#define LANGUAGES_LLVMSUPPORT_H

#include <llvm/Function.h>
#include <llvm/Instructions.h>

#include "../PointsTo/PointsTo.h"

namespace llvm {

  template<typename PointsToSets, typename OutIterator>
  void getCalledFunctions(const CallInst *CI, const PointsToSets &PS,
		  OutIterator out) {
    const Value *stripped = CI->getCalledValue()->stripPointerCasts();

    if (const Function *F = dyn_cast<Function>(stripped)) {
      *out++ = F;
    } else {
      typename PointsToSets::PointsToSet const& S =
	  getPointsToSet(stripped, PS);
      for (typename PointsToSets::PointsToSet::const_iterator
           I = S.begin(), E = S.end(); I != E; ++I)
        if (const Function *F = dyn_cast<Function>(*I))
	  *out++ = F;
    }
  }

}

#endif
