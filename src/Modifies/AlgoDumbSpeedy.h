// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef MODIFIES_DUMBSPEEDY_H
#define MODIFIES_DUMBSPEEDY_H

#include "Modifies.h"
#include "../Callgraph/Callgraph.h"
#include "../PointsTo/PointsTo.h"

namespace llvm { namespace mods {

  struct DUMB_SPEEDY {};

  void computeModifies(const ProgramStructure &P,
        const llvm::callgraph::Callgraph &CG,
        const llvm::ptr::PointsToSets &PS,
        typename Modifies<DUMB_SPEEDY>::Type& MOD, DUMB_SPEEDY);

}}

#endif
