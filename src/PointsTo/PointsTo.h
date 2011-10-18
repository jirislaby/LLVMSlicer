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

  template<typename Language, typename PointsToAlgorithm>
  struct PointsToSets {
    typedef PointsToSetsAsMap<Language,PointsToAlgorithm> Type;
  };

  template<typename MemoryLocationType, typename PointsToSetsType>
  typename PointsToSetsType::PointsToSet const&
  getPointsToSet(MemoryLocationType memLoc, PointsToSetsType const& S) {
    return getPointsToSet<typename PointsToSetsType::Language>
        (memLoc, S, typename PointsToSetsType::PointsToAlgorithm());
  }

  template<typename ProgramStructureType, typename PointsToSetsType>
  PointsToSetsType&
  computePointsToSets(ProgramStructureType const& P,PointsToSetsType& S)
  {
      return computePointsToSets<typename PointsToSetsType::Language>
                  (P,S,typename PointsToSetsType::PointsToAlgorithm());
  }

  template<typename Language, typename AnalysisProperties>
  struct ProgramStructure;

  template<typename RawProgram, typename ProgramStructureType>
  void loadProgramStructure(RawProgram const& RP, ProgramStructureType& P)
  {
      loadProgramStructure(RP,P,
              typename ProgramStructureType::AnalysisProperties());
  }

  template<typename Language, typename PointsToAlgorithm>
  struct RuleFunction {
      typedef std::tr1::function<bool(typename PointsToSets<Language,
                                          PointsToAlgorithm>::Type&)>
              Type;

      static inline bool
      identity(typename PointsToSets<Language,PointsToAlgorithm>::Type)
      { return false; }
  };

  template<typename Language, typename ExprSort, typename PointsToAlgorithm>
  typename RuleFunction<Language,PointsToAlgorithm>::Type
  getRuleFunction(ExprSort const& E, PointsToAlgorithm)
  {
      return typename RuleFunction<Language,PointsToAlgorithm>::Type(
                  &RuleFunction<Language,PointsToAlgorithm>::identity);
  }

  template<typename Language, typename PointsToAlgorithm>
  class Rules {
  public:
      typedef std::vector<
                  typename RuleFunction<Language,PointsToAlgorithm>::Type >
              RuleFunctions;
      typedef typename RuleFunctions::const_iterator const_iterator;

      template<typename Sort>
      void insert(RuleExpression<Sort> const& E)
      {
          rules.push_back( getRuleFunction<Language>(E.getSort(),
                                                     PointsToAlgorithm()) );
      }

      const_iterator begin() const { return rules.begin(); }
      const_iterator end() const { return rules.end(); }

  private:
      RuleFunctions rules;
  };

}}

#endif
