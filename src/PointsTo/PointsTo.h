// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef POINTSTO_POINTSTO_H
#define POINTSTO_POINTSTO_H

#include <map>
#include <set>
#include <vector>

#include <boost/tr1/functional.hpp>

#include "PredefContainers.h"
#include "RuleExpressions.h"

namespace llvm { namespace ptr {

  template<typename MemoryLocationType, typename PointsToSetsType>
  typename PointsToSetsType::PointsToSet const&
  getPointsToSet(MemoryLocationType memLoc, PointsToSetsType const& S) {
    return getPointsToSet(memLoc, S, typename PointsToSetsType::PointsToAlgorithm());
  }

  template<typename ProgramStructureType, typename PointsToSetsType>
  PointsToSetsType&
  computePointsToSets(ProgramStructureType const& P,PointsToSetsType& S)
  {
      return computePointsToSets(P, S,
		      typename PointsToSetsType::PointsToAlgorithm());
  }

  template<typename RawProgram, typename ProgramStructureType>
  void loadProgramStructure(RawProgram const& RP, ProgramStructureType& P)
  {
      loadProgramStructure(RP,P,
              typename ProgramStructureType::AnalysisProperties());
  }

  template<typename PointsToAlgorithm>
  struct RuleFunction {
      typedef std::tr1::function<bool(typename PointsToSets<PointsToAlgorithm>::Type&)>
              Type;

      static inline bool
      identity(typename PointsToSets<PointsToAlgorithm>::Type)
      { return false; }
  };

  template<typename ExprSort, typename PointsToAlgorithm>
  typename RuleFunction<PointsToAlgorithm>::Type
  getRuleFunction(ExprSort const& E, PointsToAlgorithm)
  {
      return typename RuleFunction<PointsToAlgorithm>::Type(
                  &RuleFunction<PointsToAlgorithm>::identity);
  }

  template<typename PointsToAlgorithm>
  class Rules {
  public:
      typedef std::vector<
                  typename RuleFunction<PointsToAlgorithm>::Type >
              RuleFunctions;
      typedef typename RuleFunctions::const_iterator const_iterator;

      template<typename Sort>
      void insert(RuleExpression<Sort> const& E)
      {
          rules.push_back( getRuleFunction(E.getSort(),
                                                     PointsToAlgorithm()) );
      }

      const_iterator begin() const { return rules.begin(); }
      const_iterator end() const { return rules.end(); }

  private:
      RuleFunctions rules;
  };

}}

namespace llvm { namespace ptr { namespace detail {

  template<typename PointsToAlgorithm>
  typename PointsToSets<PointsToAlgorithm>::Type&
  pruneByType(typename PointsToSets<PointsToAlgorithm>::Type& S)
  {
    typedef typename PointsToSets<PointsToAlgorithm>::Type PTSets;
    typedef typename PTSets::mapped_type PTSet;
    PTSet U;
    for (typename PTSets::iterator s = S.begin(); s != S.end(); )
        if (llvm::isa<llvm::Function>(s->first)) {
          typename PTSets::iterator const tmp = s++;
          S.getContainer().erase(tmp);
        } else {
          if (isPointerValue(s->first)) {
            for (typename PTSet::iterator v = s->second.begin();
                 v != s->second.end(); ) {
              if (getPointedType(s->first) != (*v)->getType()) {
                typename PTSet::iterator const tmp = v++;
                s->second.erase(tmp);
              } else
                ++v;
            }
          }
          ++s;
        }
    return S;
  }

}}}

#endif
