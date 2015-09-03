// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef POINTSTO_POINTSTO_H
#define POINTSTO_POINTSTO_H

#include <map>
#include <set>
#include <vector>

#include "llvm/IR/Value.h"

#include "RuleExpressions.h"

namespace llvm { namespace ptr {

  class PointsToSets {
  public:
    typedef const llvm::Value *MemoryLocation;
    /*
     * pointer is a pair <location, offset> such that the location is:
     * a) variable, offset is -1
     * b) alloc,    offset is <0,infty) -- structure members can point too
     *
     * Note that in LLVM, both a variable and an alloc (CallInst to malloc)
     * are llvm::Value.
     */
    typedef std::pair<MemoryLocation, int> Pointer;
    /*
     * Points-to set contains against pairs <location, offset>, where location
     * can be only an alloc companied by an offset (we can point to the
     * middle).
     */
    typedef std::pair<MemoryLocation, int> Pointee;
    typedef std::set<Pointee> PointsToSet;

    typedef std::map<Pointer, PointsToSet> Container;
    typedef Container::key_type key_type;
    typedef Container::mapped_type mapped_type;
    typedef Container::value_type value_type;
    typedef Container::iterator iterator;
    typedef Container::const_iterator const_iterator;
    typedef std::pair<iterator, bool> insert_retval;

    virtual ~PointsToSets() {}

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

}}

namespace llvm { namespace ptr {

    struct ProgramStructure
    {
        typedef RuleCode Command;
        typedef std::vector<Command> Container;
        typedef Container::value_type value_type;
        typedef Container::iterator iterator;
        typedef Container::const_iterator const_iterator;

        explicit ProgramStructure(Module &M);

        llvm::Module &getModule() const { return M; }

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
        llvm::Module &M;
    };

}}

namespace llvm { namespace ptr {

  const PointsToSets::PointsToSet &
  getPointsToSet(const llvm::Value *const &memLoc, const PointsToSets &S,
		  const int offset = -1);

  PointsToSets &computePointsToSets(const ProgramStructure &P, PointsToSets &S);

}}

#endif
