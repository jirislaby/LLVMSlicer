#include "StaticSlicer.h"

namespace llvm { namespace slicing { namespace detail {

    void fillParamsToArgs(llvm::CallInst const* const C,
                          llvm::Function const* const F,
                          ParamsToArgs& toArgs)
    {
        llvm::Function::const_arg_iterator p = F->arg_begin();
        std::size_t a = 0;
        for ( ; a < C->getNumArgOperands(); ++a, ++p)
        {
            llvm::Value const* const P = &*p;
            llvm::Value const* const A = C->getArgOperand(a);
            if (!isConstantValue(A))
                toArgs[P] = A;
        }
    }

}}}

namespace llvm { namespace slicing {

    void StaticSlicer::sliceModule()
    {
        for (Slicers::iterator s = slicers.begin(); s != slicers.end(); ++s)
            s->second->slice();
    }
#if 0
    void StaticSlicer::dump(std::ostream& ostr) const
    {
        using monty::codespy::dump;

        for (Slicers::const_iterator s = slicers.begin();
                s != slicers.end(); ++s)
        {
            ostr << "Function ";
            codespy::dump(ostr,s->first);
            ostr << std::endl;
            s->second->dump(ostr);
            ostr << std::endl;
        }
    }
#endif
}}
