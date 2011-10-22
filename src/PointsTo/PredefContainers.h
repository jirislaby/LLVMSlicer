// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef POINTSTO_PREDEFCONTAINERS_H
#define POINTSTO_PREDEFCONTAINERS_H

#include <map>
#include <set>
#include <vector>

#include "RuleExpressions.h"

namespace llvm { namespace ptr {

  template<typename PointsToAlgorithmType>
  class PointsToSetsAsMap {
  public:
    typedef PointsToAlgorithmType PointsToAlgorithm;
    typedef const llvm::Value *MemoryLocation;
    typedef std::set<MemoryLocation> PointsToSet;

    typedef std::map<MemoryLocation,PointsToSet> Container;
    typedef typename Container::key_type key_type;
    typedef typename Container::mapped_type mapped_type;
    typedef typename Container::value_type value_type;
    typedef typename Container::iterator iterator;
    typedef typename Container::const_iterator const_iterator;
    typedef std::pair<iterator, bool> insert_retval;

    virtual ~PointsToSetsAsMap() {}

    insert_retval insert(value_type const& val) { return C.insert(val); }
    PointsToSet& operator[](key_type const& key) { return C[key]; }
    const_iterator find(key_type const& key) const { return C.find(key); }
    iterator find(key_type const& key) { return C.find(key); }
    const_iterator begin() const { return C.begin(); }
    iterator begin() { return C.begin(); }
    const_iterator end() const { return C.end(); }
    iterator end() { return C.end(); }
    Container const& getContainer() const { return C; }
    Container& getContainer() { return C; }
  private:
    Container C;
  };

  template<typename PointsToAlgorithm>
  typename PointsToSetsAsMap<PointsToAlgorithm>::PointsToSet const&
  getPointsToSet(const llvm::Value *const& memLoc,
      PointsToSetsAsMap<PointsToAlgorithm> const& S)
  {
      //static typename PointsToSets<Language,PointsToAlgorithm>
      //    ::Type::PointsToSet const emptySet;
      //typename PointsToSets<Language,PointsToAlgorithm>
      //    ::Type::const_iterator const it =
      //        S.find(memLoc);
      //return (it == S.end()) ? emptySet : it->second;
      return S.find(memLoc)->second;
  }
#if 0
  template<typename Language,typename PointsToAlgorithm,typename OutuptStream>
  OutuptStream& dump(OutuptStream& ostr,
                     PointsToSetsAsMap<Language,PointsToAlgorithm> const& S)
  {
      using monty::codespy::dump;

      typedef typename PointsToSetsAsMap<Language,PointsToAlgorithm>
                  ::const_iterator
              PointersIteratorConst;
      for (PointersIteratorConst i = S.begin(); i != S.end(); ++i)
      {
          if (i->second.empty())
              continue;
          ostr << "  ";
          dump(ostr,i->first);
          ostr << '\n';
          typedef typename PointsToSetsAsMap<Language,PointsToAlgorithm>
                      ::PointsToSet::const_iterator
                  PointeeIteratorConst;
          for (PointeeIteratorConst j = i->second.begin();
                  j != i->second.end(); ++j)
          {
              ostr << "    ";
              dump(ostr,*j);
              ostr << '\n';
          }
      }
      return ostr;
  }
#endif
}}

namespace llvm { namespace ptr {

  template<typename PointsToAlgorithmType>
  class PointsToSetsAsSet {
  public:
    typedef PointsToAlgorithmType PointsToAlgorithm;
    typedef const llvm::Value *MemoryLocation;
    typedef std::set<MemoryLocation> PointsToSet;

    typedef PointsToSet Container;
    typedef typename Container::value_type value_type;
    typedef typename Container::iterator iterator;
    typedef typename Container::const_iterator const_iterator;
    typedef std::pair<iterator, bool> insert_retval;

    virtual ~PointsToSetsAsSet() {}

    insert_retval insert(value_type const& val) { return C.insert(val); }
    const_iterator find(value_type const& val) const { return C.find(val); }
    iterator find(value_type const& val) { return C.find(val); }
    const_iterator begin() const { return C.begin(); }
    iterator begin() { return C.begin(); }
    const_iterator end() const { return C.end(); }
    iterator end() { return C.end(); }
    Container const& getContainer() const { return C; }
    Container& getContainer() { return C; }
  private:
    Container C;
  };

  template<typename PointsToAlgorithm>
  typename PointsToSetsAsSet<PointsToAlgorithm>::PointsToSet const&
  getPointsToSet(
                 const llvm::Value *,
                 PointsToSetsAsSet<PointsToAlgorithm> const& S) {
      return S.getContainer();
  }
#if 0
  template<typename Language,typename PointsToAlgorithm,typename OutuptStream>
  OutuptStream& dump(OutuptStream& ostr,
                     PointsToSetsAsSet<Language,PointsToAlgorithm> const& S)
  {
      using monty::codespy::dump;

      ostr << "  <ANYTHING>\n";
      typedef typename PointsToSetsAsSet<Language,PointsToAlgorithm>
                  ::const_iterator
              PointeeIteratorConst;
      for (PointeeIteratorConst i = S.begin(); i != S.end(); ++i)
      {
          ostr << "    ";
          dump(ostr,*i);
          ostr << '\n';
      }
      return ostr;
  }
#endif
}}

namespace llvm { namespace ptr {

    struct ProgramStructure
    {
        typedef RuleCode Command;
        typedef std::vector<Command> Container;
        typedef typename Container::value_type value_type;
        typedef typename Container::iterator iterator;
        typedef typename Container::const_iterator const_iterator;

	ProgramStructure(Module &M);

        void insert(iterator it, value_type const& val) { C.insert(it,val); }
        void push_back(value_type const& val) { return C.push_back(val); }
        const_iterator begin() const { return C.begin(); }
        iterator begin() { return C.begin(); }
        const_iterator end() const { return C.end(); }
        iterator end() { return C.end(); }
        Container const& getContainer() const { return C; }
        Container& getContainer() { return C; }
    private:
        Container C;
    };
#if 0
    template<typename Language, typename AnalysisProperties,
             typename CommandType, typename OutuptStream>
    OutuptStream& dump(OutuptStream& ostr,
                       ProgramStructureAsVector<Language,AnalysisProperties,
                                                CommandType> const& P)
    {
        using monty::codespy::dump;

        typedef typename ProgramStructureAsVector<Language,AnalysisProperties,
                                                  CommandType>::const_iterator
                IteratorConst;
        for (IteratorConst i = P.begin(); i != P.end(); ++i)
        {
            ostr << "  ";
            dump(ostr,*i);
            ostr << '\n';
        }
        return ostr;
    }
#endif
}}

#endif
