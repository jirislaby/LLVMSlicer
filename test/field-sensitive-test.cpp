#include <llvm/LLVMContext.h>
#include <llvm/Function.h>
#include <llvm/Module.h>
#include <llvm/Support/IRReader.h>
#include <llvm/Support/raw_ostream.h>

#include "../src/PointsTo/PointsTo.h"

using namespace llvm;

template<typename PointsToSet>
static void pointsTo(const StoreInst &SI, PointsToSet const &PS)
{
	const Value *val = SI.getValueOperand();
	const Value *ptr = SI.getPointerOperand();

	if (isa<Constant>(val))
		return;

	errs() << "PTSTO ";
	SI.dump();
	errs() << "\tLEFT  ";
	ptr->dump();
	errs() << "\tRIGHT ";
	val->dump();

	typename PointsToSet::PointsToSet const &S =
		ptr::getPointsToSet(ptr, PS);
	for (typename PointsToSet::PointsToSet::const_iterator I = S.begin(),
			           E = S.end(); I != E; ++I) {
		errs() << "\t";
		(*I)->dump();
	}
}

static void pointsTo(Module &M)
{
	ptr::PointsToSets PS;
	{
		ptr::ProgramStructure P(M);
		computePointsToSets(P, PS);
	}

	const Function *mainFun = M.getFunction("main");
	mainFun->dump();
	errs() << "===========\n";
	for (const_inst_iterator I = inst_begin(*mainFun), E = inst_end(*mainFun);
			I != E; ++I) {
		const Instruction *ins = &*I;
		if (const StoreInst *SI = dyn_cast<StoreInst>(ins))
			pointsTo(*SI, PS);
	}
}

int main(int argc, char **argv)
{
	LLVMContext context;
	SMDiagnostic SMD;
	Module *M;

	M = ParseIRFile(argv[1], SMD, context);
	if (!M) {
		SMD.print(argv[0], errs());
		return 1;
	}

	pointsTo(*M);

	delete M;

	return 0;
}
