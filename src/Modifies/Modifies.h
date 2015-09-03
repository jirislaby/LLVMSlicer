// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef MODIFIES_MODIFIES_H
#define MODIFIES_MODIFIES_H

#include <map>
#include <set>
#include <vector>

#include "llvm/IR/Function.h"
#include "llvm/IR/Value.h"

#include "../Languages/LLVM.h"
#include "../PointsTo/PointsTo.h"
#include "../Callgraph/Callgraph.h"

namespace llvm { namespace mods {

    struct Modifies {
        typedef std::set<llvm::ptr::PointsToSets::Pointee> ModSet;
        typedef std::map<const llvm::Function *, ModSet> Container;
        typedef Container::key_type key_type;
        typedef Container::mapped_type mapped_type;
        typedef Container::value_type value_type;
        typedef Container::iterator iterator;
        typedef Container::const_iterator const_iterator;
        typedef std::pair<iterator, bool> insert_retval;

        virtual ~Modifies() {}

        insert_retval insert(value_type const& val) { return C.insert(val); }
        mapped_type& operator[](key_type const& key) { return C[key]; }
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

    const Modifies::ModSet &getModSet(const llvm::Function *const &f,
              const Modifies &S);

}}

namespace llvm { namespace mods {

    enum WriteType
    {
        CMD_UNKNOWN = 0,
        CMD_VAR,
        CMD_DREF_VAR
    };

    struct WriteCommand {
        WriteCommand()
            : type(CMD_UNKNOWN)
        {}

        WriteCommand(WriteType const t, const llvm::Value *v)
            : type(t)
            , var(v)
        {}

        virtual ~WriteCommand()
        {}

        WriteType getType() const { return type; }
        const llvm::Value *getVar() const { return var; }
    private:
        WriteType type;
        const llvm::Value *var;
    };
}}

namespace llvm { namespace mods {

  struct ProgramStructure {
      typedef WriteCommand Command;
      typedef std::vector<Command> Commands;
      typedef std::map<const llvm::Function *,Commands> Container;
      typedef Container::key_type key_type;
      typedef Container::mapped_type mapped_type;
      typedef Container::value_type value_type;
      typedef Container::iterator iterator;
      typedef Container::const_iterator const_iterator;
      typedef std::pair<iterator, bool> insert_retval;

      ProgramStructure(Module &M);

      Commands const &getFunctionCommands(const llvm::Function *const& f,
				  ProgramStructure const& PS) {
	  return PS.find(f)->second;
      }

      insert_retval insert(value_type const& val) { return C.insert(val); }
      mapped_type& operator[](key_type const& key) { return C[key]; }
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
namespace llvm { namespace mods {

    void computeModifies(const ProgramStructure &P,
			 const callgraph::Callgraph &CG,
                         const llvm::ptr::PointsToSets &PS, Modifies &M);

}}

#endif
