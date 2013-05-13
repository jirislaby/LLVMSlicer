// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef POINTSTO_POINTSTO_H
#define POINTSTO_POINTSTO_H

#include <vector>

#include "llvm/Value.h"

#include "PredefContainers.h"

namespace llvm { namespace ptr {

  const PointsToSets::PointsToSet &
  getPointsToSet(const llvm::Value *const &memLoc, const PointsToSets &S);

  PointsToSets &computePointsToSets(const ProgramStructure &P, PointsToSets &S);

}}

#endif
