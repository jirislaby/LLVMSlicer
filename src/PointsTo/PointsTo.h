// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef POINTSTO_POINTSTO_H
#define POINTSTO_POINTSTO_H

#include <vector>

#include "llvm/Value.h"

#include "PredefContainers.h"
#include "RuleExpressions.h"

namespace llvm { namespace ptr {

  const PointsToSets::PointsToSet &
  getPointsToSet(const llvm::Value *const &memLoc, const PointsToSets &S);

  PointsToSets &computePointsToSets(const ProgramStructure &P, PointsToSets &S);

  struct RuleFunction {
      typedef std::function<bool(PointsToSets &)> Type;

      static inline bool identity(PointsToSets) { return false; }
  };

  class Rules {
  public:
      typedef std::vector<RuleFunction::Type> RuleFunctions;
      typedef typename RuleFunctions::const_iterator const_iterator;

      template<typename Sort>
      void insert(RuleExpression<Sort> const& E)
      {
          rules.push_back( getRuleFunction(E.getSort()) );
      }

      const_iterator begin() const { return rules.begin(); }
      const_iterator end() const { return rules.end(); }

  private:
      RuleFunctions rules;
  };

}}

#endif
