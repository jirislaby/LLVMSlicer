// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef POINTSTO_ALGOANDERSEN_H
#define POINTSTO_ALGOANDERSEN_H

#include "PredefContainers.h"
#include "PointsTo.h"

namespace llvm { namespace ptr {

  struct ANDERSEN {};

}}

namespace llvm { namespace ptr {

  PointsToSets<ANDERSEN>::Type&
  computePointsToSets(ProgramStructure const& P,
			PointsToSets<ANDERSEN>::Type& S,
			ANDERSEN);

  RuleFunction<ANDERSEN>::Type
  getRuleFunction(ASSIGNMENT<
		      VARIABLE<const llvm::Value *>,
		      VARIABLE<const llvm::Value *>
		      > const& E,
		  ANDERSEN);

  RuleFunction<ANDERSEN>::Type getRuleFunction(
	 ASSIGNMENT<
		      VARIABLE<const llvm::Value *>,
		      REFERENCE<
			  VARIABLE<const llvm::Value *> >
		      > const& E,
		  ANDERSEN);

  RuleFunction<ANDERSEN>::Type
  getRuleFunction(ASSIGNMENT<
		      VARIABLE<const llvm::Value *>,
		      DEREFERENCE< VARIABLE<const llvm::Value *> >
		      > const& E,
		  ANDERSEN);

  RuleFunction<ANDERSEN>::Type
  getRuleFunction(ASSIGNMENT<
		      DEREFERENCE< VARIABLE<const llvm::Value *> >,
		      VARIABLE<const llvm::Value *>
		      > const& E,
		  ANDERSEN);

  RuleFunction<ANDERSEN>::Type
  getRuleFunction(ASSIGNMENT<
		      DEREFERENCE<
			  VARIABLE<const llvm::Value *> >,
		      REFERENCE<
			  VARIABLE<const llvm::Value *> >
		      > const& E,
		  ANDERSEN);

  RuleFunction<ANDERSEN>::Type
  getRuleFunction(ASSIGNMENT<
		      DEREFERENCE<
			  VARIABLE<const llvm::Value *> >,
		      DEREFERENCE<
			  VARIABLE<const llvm::Value *> >
		      > const& E,
		  ANDERSEN);

  RuleFunction<ANDERSEN>::Type
  getRuleFunction(ASSIGNMENT<
		      VARIABLE<const llvm::Value *>,
		      ALLOC<const llvm::Value *>
		      > const& E,
		  ANDERSEN);

  RuleFunction<ANDERSEN>::Type
  getRuleFunction(ASSIGNMENT<
		      VARIABLE<const llvm::Value *>,
		      NULLPTR<const llvm::Value *>
		      > const& E,
		  ANDERSEN);

  RuleFunction<ANDERSEN>::Type
  getRuleFunction(ASSIGNMENT<
		      DEREFERENCE<
			  VARIABLE<const llvm::Value *> >,
		      NULLPTR<const llvm::Value *>
		      > const& E,
		  ANDERSEN);

  RuleFunction<ANDERSEN>::Type
  getRuleFunction(DEALLOC<const llvm::Value *>, ANDERSEN);

}}

#endif
