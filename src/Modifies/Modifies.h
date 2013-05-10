// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef MODIFIES_MODIFIES_H
#define MODIFIES_MODIFIES_H

#include "../PointsTo/PointsTo.h"
#include "PredefContainers.h"

namespace llvm { namespace mods {

    void computeModifies(const ProgramStructure &P,
			 const callgraph::Callgraph &CG,
                         const llvm::ptr::PointsToSets &PS, Modifies &M);

}}

#endif
