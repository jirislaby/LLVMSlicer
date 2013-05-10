// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef POINTSTO_POINTSTO_H
#define POINTSTO_POINTSTO_H

#include <functional>
#include <map>
#include <set>
#include <vector>

#include "llvm/Value.h"

#include "PredefContainers.h"
#include "RuleExpressions.h"

namespace llvm { namespace ptr {

  template<typename PointsToSetsType>
  typename PointsToSetsType::PointsToSet const&
  getPointsToSet(const llvm::Value *const &memLoc, PointsToSetsType const& S) {
    return getPointsToSet(memLoc, S, typename PointsToSetsType::PointsToAlgorithm());
  }

  template<typename ProgramStructureType, typename PointsToSetsType>
  PointsToSetsType&
  computePointsToSets(ProgramStructureType const& P,PointsToSetsType& S)
  {
      return computePointsToSets(P, S,
		      typename PointsToSetsType::PointsToAlgorithm());
  }

  template<typename PointsToAlgorithm>
  struct RuleFunction {
      typedef std::function<bool(typename PointsToSets<PointsToAlgorithm>::Type&)>
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

/*
 * It does not really work -- it prunes too much. Like it does not take into
 * account bitcast instructions in the code.
 */
namespace llvm { namespace ptr { namespace detail {

  template<typename PointsToAlgorithm>
  typename PointsToSets<PointsToAlgorithm>::Type&
  pruneByType(typename PointsToSets<PointsToAlgorithm>::Type& S)
  {
    typedef typename PointsToSets<PointsToAlgorithm>::Type PTSets;
    typedef typename PTSets::mapped_type PTSet;
    for (typename PTSets::iterator s = S.begin(); s != S.end(); ) {
	const llvm::Value *first = s->first;
        if (llvm::isa<llvm::Function>(first)) {
          typename PTSets::iterator const tmp = s++;
          S.getContainer().erase(tmp);
        } else {
#if 0
          if (isPointerValue(first)) {
	    const llvm::Type *firstTy;
	    if (const llvm::BitCastInst *BC =
			llvm::dyn_cast<llvm::BitCastInst>(first))
	      firstTy = getPointedType(BC->getSrcTy());
	    else
	      firstTy = getPointedType(first);

            for (typename PTSet::const_iterator v = s->second.begin();
                 v != s->second.end(); ) {
	      const llvm::Value *second = *v;
	      const llvm::Type *secondTy = second->getType();

	      if (hasExtraReference(second))
		      secondTy = llvm::cast<llvm::PointerType>(secondTy)->
			      getElementType();
	      if (const llvm::ArrayType *AT =
			      llvm::dyn_cast<llvm::ArrayType>(secondTy))
		      secondTy = AT->getElementType();

              if (firstTy != secondTy) {
                typename PTSet::iterator const tmp = v++;
                s->second.erase(tmp);
              } else
                ++v;
            }
          }
#endif
          ++s;
        }
    }
    return S;
  }

}}}

#endif
