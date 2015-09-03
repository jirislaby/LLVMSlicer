// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef CALLGRAPH_CALLGRAPH_H
#define CALLGRAPH_CALLGRAPH_H

#include <map>
#include <algorithm>
#include <iterator>
#include <utility>

#include "llvm/IR/Function.h"
#include "llvm/ADT/STLExtras.h" /* tie */

#include "../Languages/LLVM.h"
#include "../Languages/LLVMSupport.h"
#include "../PointsTo/PointsTo.h"

namespace llvm { namespace callgraph {

    struct Callgraph {
        typedef std::multimap<const llvm::Function *, const llvm::Function *>
		Container;
        typedef Container::key_type key_type;
        typedef Container::mapped_type mapped_type;
        typedef Container::value_type value_type;
        typedef Container::iterator iterator;
        typedef Container::const_iterator const_iterator;
        typedef std::pair<const_iterator,const_iterator> range_iterator;

        Callgraph(Module &M, const llvm::ptr::PointsToSets &PS);

        range_iterator directCalls(key_type const& key) const
        { return directCallsMap.equal_range(key); }

        range_iterator directCallees(key_type const& key) const
        { return directCalleesMap.equal_range(key); }

        range_iterator calls(key_type const& key) const
        { return callsMap.equal_range(key); }

        range_iterator callees(key_type const& key) const
        { return calleesMap.equal_range(key); }

        bool contains(key_type const key, mapped_type const value) const {
          range_iterator rng = directCalls(key);
          for (const_iterator it = rng.first; it != rng.second; ++it)
            if (it->second == value)
              return true;
          return false;
        }

        const_iterator begin() const { return directCallsMap.begin(); }
        iterator begin() { return directCallsMap.begin(); }
        const_iterator end() const { return directCallsMap.end(); }
        iterator end() { return directCallsMap.end(); }
        const_iterator begin_closure() const { return callsMap.begin(); }
        const_iterator end_closure() const { return callsMap.end(); }
        Container const& getContainer() const { return directCallsMap; }
        Container& getContainer() { return directCallsMap; }

    protected:
        iterator insertDirectCall(value_type const& val)
        { return directCallsMap.insert(val); }

    private:
        Container directCallsMap;
        Container directCalleesMap;
        Container callsMap;
        Container calleesMap;

        void handleCall(const llvm::Function *parent, const llvm::CallInst *CI,
                        const llvm::ptr::PointsToSets &PS);
    };
}}

namespace llvm { namespace callgraph { namespace detail {

  template<typename Relation>
  void computeTransitiveClosure(Relation const& R, Relation& TCR) {
    typedef std::set<typename Relation::value_type> Set;
    typedef std::multimap<typename Relation::value_type::first_type,
                          typename Relation::value_type::second_type> Dict;

    Set S;
    std::copy(R.begin(),R.end(),std::inserter(S,S.end()));

    Dict D;
    std::copy(R.begin(),R.end(),std::inserter(D,D.end()));

    while (true)
    {
      std::size_t const old_size = S.size();

      for (typename Set::const_iterator it = S.begin(); it != S.end(); ++it) {
        typename Dict::const_iterator b,e;
        llvm::tie(b,e) = D.equal_range(it->second);
        for ( ; b != e; ++b)
          S.insert(typename Set::value_type(it->first,b->second));
      }

      if (old_size == S.size())
          break;
    }

    std::copy(S.begin(),S.end(),std::inserter(TCR,TCR.end()));
  }

}}}

namespace llvm { namespace callgraph {

    static inline Callgraph::range_iterator
    getDirectCalls(Callgraph::key_type const& key, Callgraph const& CG) {
        return CG.directCalls(key);
    }

    static inline Callgraph::range_iterator
    getDirectCallees(Callgraph::key_type const& key, Callgraph const& CG) {
        return CG.directCallees(key);
    }

    static inline Callgraph::range_iterator
    getCalls(Callgraph::key_type const& key, Callgraph const& CG) {
        return CG.calls(key);
    }

    static inline Callgraph::range_iterator
    getCallees(Callgraph::key_type const& key, Callgraph const& CG) {
        return CG.callees(key);
    }

}}

#endif
