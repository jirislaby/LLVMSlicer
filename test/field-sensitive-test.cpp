#include <stdlib.h>
#include <utility>

#include <llvm/LLVMContext.h>
#include <llvm/Function.h>
#include <llvm/Module.h>
#include <llvm/Support/IRReader.h>
#include <llvm/Support/raw_ostream.h>

#include "../src/PointsTo/PointsTo.h"

#define DEBUG

using namespace llvm;

typedef ptr::PointsToSets::Pointer Ptr;
typedef ptr::PointsToSets::Pointee Ptee;
typedef ptr::PointsToSets::PointsToSet PTSet;
typedef std::pair<const Ptr, const Ptee> ToCheckEl;
typedef SmallVector<ToCheckEl, 20> ToCheck;

static void pointsTo(Module &M, const ToCheck &toCheck)
{
	ptr::PointsToSets PS;
	{
		ptr::ProgramStructure P(M);
		computePointsToSets(P, PS);
	}
#ifdef DEBUG
	for (ptr::PointsToSets::const_iterator I = PS.begin(), E = PS.end();
			I != E; ++I) {
		const Ptr &ptr = I->first;
		errs() << "OFF=" << ptr.second;
		ptr.first->dump();
		const PTSet &p = I->second;
		for (PTSet::const_iterator II = p.begin(), EE = p.end();
				II != EE; ++II) {
			const Ptee &ptee = *II;
			errs() << "\tOFF=" << ptee.second;
			ptee.first->dump();
		}
	}

	errs() << "======\n";
#endif
	for (ToCheck::const_iterator I = toCheck.begin(), E = toCheck.end();
			I != E; ++I) {
		const Ptr &ptr = I->first;
		const Ptee &ptee1 = I->second;
#ifdef DEBUG
		errs() << "Checking if OFF=" << ptr.second;
		ptr.first->dump();
		errs() << "\tpoints to: OFF=" << ptee1.second;
		ptee1.first->dump();
#endif
		const PTSet &S = ptr::getPointsToSet(ptr.first, PS, ptr.second);
		bool found = false;
		for (PTSet::const_iterator II = S.begin(), EE = S.end();
				II != EE; ++II) {
			const Ptee &ptee2 = *II;
			if (ptee1 == ptee2) {
				found = true;
				break;
			}
		}
		if (!found) {
			errs() << "Cannot find pointee for OFF=" <<
				ptr.second;
			ptr.first->dump();
			errs() << "\tshould point to OFF=" << ptee1.second;
			ptee1.first->dump();
			abort();
		}
	}
}

static void addCheck(ToCheck &toCheck, const Value *ptr1, const int off1,
		const Value *ptr2, const int off2) {
	toCheck.push_back(ToCheckEl(Ptr(ptr1, off1), Ptee(ptr2, off2)));
}

static Value *call_malloc(ToCheck &toCheck, BasicBlock *entry,
		Function *xmalloc, uint64_t size) {
	LLVMContext &C = xmalloc->getContext();
	Value *ret = CallInst::Create(xmalloc,
		ArrayRef<Value *>(ConstantInt::get(Type::getInt64Ty(C), size)),
		"", entry);

	addCheck(toCheck, ret, -1, ret, 0);

	return ret;
}

static std::unique_ptr<Module> build(LLVMContext &C, ToCheck &toCheck)
{
	Module *M = new Module("field-sensitivity", C);
	IntegerType *int32 = Type::getInt32Ty(C);

	Function *main = Function::Create(
			FunctionType::get(Type::getVoidTy(C), false),
			GlobalValue::InternalLinkage, "main", M);

	FunctionType *mallocTy = FunctionType::get(Type::getInt8PtrTy(C),
			ArrayRef<Type *>(Type::getInt64Ty(C)), false);
	Function *xmalloc = Function::Create(mallocTy,
			GlobalValue::ExternalLinkage, "malloc", M);

	GlobalVariable *glob = new GlobalVariable(*M, Type::getInt32Ty(C),
			false, GlobalValue::InternalLinkage, 0, "global");

	BasicBlock *entry = BasicBlock::Create(C, "entry", main);

	Value *malloc48 = call_malloc(toCheck, entry, xmalloc, 48);

	SmallVector<Type *, 10> structElems;
	structElems.push_back(Type::getInt64Ty(C));
	structElems.push_back(Type::getInt8PtrTy(C));
	Type *xstruct = StructType::create(structElems, "testing");

	Value *bcast = new BitCastInst(malloc48,
			PointerType::getUnqual(xstruct), "", entry);

	addCheck(toCheck, bcast, -1, malloc48, 0);

	SmallVector<Value *, 10> gepIdx;
	gepIdx.push_back(ConstantInt::get(int32, 0));
	gepIdx.push_back(ConstantInt::get(int32, 1));
	Value *gep = GetElementPtrInst::CreateInBounds(bcast, gepIdx, "",
			entry);

	addCheck(toCheck, gep, -1, malloc48, 8);

	Value *malloc24 = call_malloc(toCheck, entry, xmalloc, 24);

	new StoreInst(malloc24, gep, entry);

	addCheck(toCheck, malloc48, 8, malloc24, 0);

	Value *load = new LoadInst(gep, "", entry);

	addCheck(toCheck, load, -1, malloc24, 0);

	Value *i = new AllocaInst(xstruct, 0, "", entry);
	Value *pi = new AllocaInst(PointerType::getUnqual(xstruct), 0, "",
			entry);

	new StoreInst(i, pi, entry);

	addCheck(toCheck, pi, -1, i, 0);

	load = new LoadInst(pi, "", entry);

	addCheck(toCheck, load, -1, i, 0);

	gep = GetElementPtrInst::CreateInBounds(load, gepIdx, "",
			entry);

	addCheck(toCheck, gep, -1, i, 8);

#ifdef DEBUG
	errs() << "====== DUMP\n";
	M->dump();
	errs() << "====== EOD\n";
#endif
	return std::unique_ptr<Module>(M);
}

int main(int argc, char **argv)
{
	LLVMContext context;
	ToCheck toCheck;

	pointsTo(*build(context, toCheck), toCheck);

	return 0;
}
