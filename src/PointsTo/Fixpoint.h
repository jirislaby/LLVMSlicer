// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef POINTSTO_FIXPOINT_H
#define POINTSTO_FIXPOINT_H

#include "PointsTo.h"

namespace llvm { namespace ptr {

    template<typename PointsToAlgorithm>
    bool executeRules(ProgramStructure const& P,
        typename PointsToSets<PointsToAlgorithm>::Type& S)
    {
        bool change = false;

        for (ProgramStructure::const_iterator i = P.begin(); i != P.end(); ++i) {
            Rules rules;
            getRulesOfCommand(*i, rules);
            for (Rules::const_iterator j = rules.begin(); j != rules.end(); ++j) {
                bool const modification = (*j)(S);
                change = change || modification;
            }
        }

        return change;
    }



    template<typename PointsToAlgorithm>
    typename PointsToSets<PointsToAlgorithm>::Type&
    fixpoint(ProgramStructure const& P,
             typename PointsToSets<PointsToAlgorithm>::Type& S)
    {
        while (executeRules<PointsToAlgorithm>(P,S))
            ;
        return S;
    }

}}

#endif
