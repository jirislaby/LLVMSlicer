// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef POINTSTO_FIXPOINT_H
#define POINTSTO_FIXPOINT_H

#include "PointsTo.h"

namespace llvm { namespace ptr {

    template<typename Language, typename PointsToAlgorithm>
    bool executeRules(
        typename ProgramStructure<Language>::Type const& P,
        typename PointsToSets<Language,PointsToAlgorithm>::Type& S)
    {
        bool change = false;

        typedef typename ProgramStructure<Language>::Type Program;
        for (typename Program::const_iterator i = P.begin(); i != P.end(); ++i) {
            Rules<Language,PointsToAlgorithm> rules;
            getRulesOfCommand(*i,rules);
            typedef typename Rules<Language,PointsToAlgorithm>::const_iterator
                    RulesIterConst;
            for (RulesIterConst j = rules.begin(); j != rules.end(); ++j) {
                bool const modification = (*j)(S);
                change = change || modification;
            }
        }

        return change;
    }



    template<typename Language, typename PointsToAlgorithm>
    typename PointsToSets<Language,PointsToAlgorithm>::Type&
    fixpoint(typename ProgramStructure<Language>::Type const& P,
             typename PointsToSets<Language,PointsToAlgorithm>::Type& S)
    {
        while (executeRules<Language,PointsToAlgorithm>(P,S))
            ;
        return S;
    }

}}

#endif
