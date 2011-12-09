// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef MODIFIES_PREDEFCONTAINERS_H
#define MODIFIES_PREDEFCONTAINERS_H

#include <map>
#include <set>
#include <vector>
#include <utility>

#include "llvm/Function.h"
#include "llvm/Value.h"

#include "../Languages/LLVM.h"

namespace llvm { namespace mods {

    template<typename AlgorithmType>
    struct ModifiesAsMap {
        typedef AlgorithmType Algorithm;
        typedef std::set<const llvm::Value *> ModSet;
        typedef std::map<const llvm::Function *, ModSet> Container;
        typedef typename Container::key_type key_type;
        typedef typename Container::mapped_type mapped_type;
        typedef typename Container::value_type value_type;
        typedef typename Container::iterator iterator;
        typedef typename Container::const_iterator const_iterator;
        typedef std::pair<iterator, bool> insert_retval;

        virtual ~ModifiesAsMap() {}

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

    template<typename Algorithm>
    typename ModifiesAsMap<Algorithm>::ModSet const&
    getModSet(const llvm::Function *const& f,
              ModifiesAsMap<Algorithm> const& S)
    {
        static typename ModifiesAsMap<Algorithm>::ModSet const empty;
        typename ModifiesAsMap<Algorithm>::const_iterator const it =
            S.find(f);
        return (it == S.end()) ? empty : it->second;
    }
}}

namespace llvm { namespace mods {

    enum WriteType
    {
        CMD_UNKNOWN = 0,
        CMD_VAR,
        CMD_DREF_VAR
    };

    struct WriteCommand {
        typedef const llvm::Value *Variable;

        WriteCommand()
            : type(CMD_UNKNOWN)
        {}

        WriteCommand(WriteType const t, Variable const& v)
            : type(t)
            , var(v)
        {}

        virtual ~WriteCommand()
        {}

        WriteType getType() const { return type; }
        Variable const& getVar() const { return var; }
    private:
        WriteType type;
        Variable var;
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

#endif
