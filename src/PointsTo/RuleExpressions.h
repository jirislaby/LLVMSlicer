// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#ifndef POINTSTO_RULEEXPRESSIONS_H
#define POINTSTO_RULEEXPRESSIONS_H

namespace llvm { namespace ptr {
    template<typename Language, typename PointsToAlgorithm>
    struct Rules;
}}

namespace llvm { namespace ptr {

    struct Expression
    {
        virtual ~Expression() {}
    };

    template<typename SubExpr>
    struct RuleUnaryExpression : public Expression
    {
        typedef SubExpr SubExpression;

        explicit RuleUnaryExpression(SubExpression const sub)
            : arg(sub)
        {}

        SubExpression getArgument() const
        { return arg; }


        SubExpression arg;
    };

    template<typename SubExpr1, typename SubExpr2>
    struct RuleBinaryExpression : public Expression
    {
        typedef SubExpr1 SubExpression1;
        typedef SubExpr2 SubExpression2;

        explicit RuleBinaryExpression(SubExpression1 const sub1,
                                      SubExpression2 const sub2)
            : arg1(sub1)
            , arg2(sub2)
        {}

        SubExpression1 getArgument1() const
        { return arg1; }

        SubExpression2 getArgument2() const
        { return arg2; }


        SubExpression1 arg1;
        SubExpression2 arg2;
    };

    template<typename MemLoc>
    struct VARIABLE : public RuleUnaryExpression<MemLoc> {
        VARIABLE(MemLoc const ml) : RuleUnaryExpression<MemLoc>(ml) {}
    };

    template<typename MemLoc>
    struct ALLOC : public RuleUnaryExpression<MemLoc> {
        ALLOC(MemLoc const ml) : RuleUnaryExpression<MemLoc>(ml) {}
    };

    template<typename MemLoc>
    struct DEALLOC : public RuleUnaryExpression<MemLoc> {
        DEALLOC(MemLoc const ml) : RuleUnaryExpression<MemLoc>(ml) {}
    };

    template<typename MemLoc>
    struct NULLPTR : public RuleUnaryExpression<MemLoc> {
        NULLPTR(MemLoc const ml) : RuleUnaryExpression<MemLoc>(ml) {}
    };

    template<typename SubExpr>
    struct REFERENCE : public RuleUnaryExpression<SubExpr> {
        REFERENCE(SubExpr const sub) : RuleUnaryExpression<SubExpr>(sub) {}
    };

    template<typename SubExpr>
    struct DEREFERENCE : public RuleUnaryExpression<SubExpr> {
        DEREFERENCE(SubExpr const sub) : RuleUnaryExpression<SubExpr>(sub) {}
    };

    template<typename LSubExpr, typename RSubExpr>
    struct ASSIGNMENT : public RuleBinaryExpression<LSubExpr,RSubExpr> {
        ASSIGNMENT(LSubExpr const lsub, RSubExpr const rsub)
            : RuleBinaryExpression<LSubExpr,RSubExpr>(lsub,rsub) {}
    };

    template<typename ExprSort>
    struct RuleExpression
    {
        typedef ExprSort Sort;

        explicit RuleExpression(Sort const& s)
            : sort(s)
        {}

        Sort getSort() const
        { return sort; }

        template<typename RSort>
        RuleExpression< ASSIGNMENT<Sort,RSort> >
        operator=(RuleExpression<RSort> const& r) const
        { return makeAssignment(r); }

        template<typename RSort>
        RuleExpression< ASSIGNMENT<Sort,RSort> >
        operator=(RuleExpression<RSort> const& r)
        { return makeAssignment(r); }

        RuleExpression< ASSIGNMENT<Sort,Sort> >
        operator=(RuleExpression<Sort> const& r)
        { return makeAssignment(r); }

        RuleExpression< REFERENCE<Sort> > operator&() const
        {
            return RuleExpression< REFERENCE<Sort> >(REFERENCE<Sort>(sort));
        }

        RuleExpression< DEREFERENCE<Sort> > operator*() const
        {
            return RuleExpression< DEREFERENCE<Sort> >(DEREFERENCE<Sort>(sort));
        }
    private:
        template<typename RSort>
        RuleExpression< ASSIGNMENT<Sort,RSort> >
        makeAssignment(RuleExpression<RSort> const& r)
        {
            return RuleExpression< ASSIGNMENT<Sort,RSort> >(
                        ASSIGNMENT<Sort,RSort>(sort,r.getSort()));
        }

        Sort sort;
    };

    template<typename MemLoc>
    RuleExpression< VARIABLE<MemLoc> > ruleVar(MemLoc const ml)
    {
        return RuleExpression< VARIABLE<MemLoc> >(VARIABLE<MemLoc>(ml));
    }

    template<typename MemLoc>
    RuleExpression< ALLOC<MemLoc> > ruleAllocSite(MemLoc const ml)
    {
        return RuleExpression< ALLOC<MemLoc> >(ALLOC<MemLoc>(ml));
    }

    template<typename MemLoc>
    RuleExpression< DEALLOC<MemLoc> > ruleDeallocSite(MemLoc const ml)
    {
        return RuleExpression< DEALLOC<MemLoc> >(DEALLOC<MemLoc>(ml));
    }

    template<typename MemLoc>
    RuleExpression< NULLPTR<MemLoc> > ruleNull(MemLoc const ml)
    {
        return RuleExpression< NULLPTR<MemLoc> >(NULLPTR<MemLoc>(ml));
    }

}}

namespace llvm { namespace ptr {

    enum RuleCodeType
    {
        RCT_UNKNOWN = 0,
        RCT_VAR_ASGN_ALLOC,
        RCT_VAR_ASGN_NULL,
        RCT_VAR_ASGN_VAR,
        RCT_VAR_ASGN_REF_VAR,
        RCT_VAR_ASGN_DREF_VAR,
        RCT_DREF_VAR_ASGN_NULL,
        RCT_DREF_VAR_ASGN_VAR,
        RCT_DREF_VAR_ASGN_REF_VAR,
        RCT_DREF_VAR_ASGN_DREF_VAR,
        RCT_DEALLOC,
    };

    template<typename MemLoc>
    struct RuleCode
    {
        typedef MemLoc MemoryLocation;

        RuleCode()
            : type(RCT_UNKNOWN)
        {}

        RuleCode(ASSIGNMENT<VARIABLE<MemoryLocation>,
                            ALLOC<MemoryLocation> > const& E)
            : type(RCT_VAR_ASGN_ALLOC)
            , lvalue(E.getArgument1().getArgument())
            , rvalue(E.getArgument2().getArgument())
        {}

        RuleCode(ASSIGNMENT<VARIABLE<MemoryLocation>,
                            NULLPTR<MemoryLocation> > const& E)
            : type(RCT_VAR_ASGN_NULL)
            , lvalue(E.getArgument1().getArgument())
            , rvalue(E.getArgument2().getArgument())
        {}

        RuleCode(ASSIGNMENT<VARIABLE<MemoryLocation>,
                            VARIABLE<MemoryLocation> > const& E)
            : type(RCT_VAR_ASGN_VAR)
            , lvalue(E.getArgument1().getArgument())
            , rvalue(E.getArgument2().getArgument())
        {}

        RuleCode(ASSIGNMENT<VARIABLE<MemoryLocation>,
                            REFERENCE<VARIABLE<MemoryLocation> > > const& E)
            : type(RCT_VAR_ASGN_REF_VAR)
            , lvalue(E.getArgument1().getArgument())
            , rvalue(E.getArgument2().getArgument().getArgument())
        {}

        RuleCode(ASSIGNMENT<VARIABLE<MemoryLocation>,
                            DEREFERENCE<VARIABLE<MemoryLocation> > > const& E)
            : type(RCT_VAR_ASGN_DREF_VAR)
            , lvalue(E.getArgument1().getArgument())
            , rvalue(E.getArgument2().getArgument().getArgument())
        {}

        RuleCode(ASSIGNMENT<DEREFERENCE<VARIABLE<MemoryLocation> >,
                            NULLPTR<MemoryLocation> > const& E)
            : type(RCT_DREF_VAR_ASGN_NULL)
            , lvalue(E.getArgument1().getArgument().getArgument())
            , rvalue(E.getArgument2().getArgument())
        {}

        RuleCode(ASSIGNMENT<DEREFERENCE<VARIABLE<MemoryLocation> >,
                            VARIABLE<MemoryLocation> > const& E)
            : type(RCT_DREF_VAR_ASGN_VAR)
            , lvalue(E.getArgument1().getArgument().getArgument())
            , rvalue(E.getArgument2().getArgument())
        {}

        RuleCode(ASSIGNMENT<DEREFERENCE<VARIABLE<MemoryLocation> >,
                            REFERENCE<VARIABLE<MemoryLocation> > > const& E)
            : type(RCT_DREF_VAR_ASGN_REF_VAR)
            , lvalue(E.getArgument1().getArgument().getArgument())
            , rvalue(E.getArgument2().getArgument().getArgument())
        {}

        RuleCode(ASSIGNMENT<DEREFERENCE<VARIABLE<MemoryLocation> >,
                            DEREFERENCE<VARIABLE<MemoryLocation> > > const& E)
            : type(RCT_DREF_VAR_ASGN_DREF_VAR)
            , lvalue(E.getArgument1().getArgument().getArgument())
            , rvalue(E.getArgument2().getArgument().getArgument())
        {}

        RuleCode(DEALLOC<MemoryLocation> const& E)
            : type(RCT_DEALLOC)
            , lvalue(E.getArgument())
            , rvalue()
        {}

        RuleCodeType getType() const { return type; }
        MemoryLocation const& getLvalue() const { return lvalue; }
        MemoryLocation const& getRvalue() const { return rvalue; }
        MemoryLocation const& getValue() const { return getLvalue(); }

    private:
        RuleCodeType type;
        MemoryLocation lvalue;
        MemoryLocation rvalue;
    };

    template<typename Language, typename ExprSort>
    RuleCode<const llvm::Value *>
    ruleCode(RuleExpression<ExprSort> const& E)
    {
        return RuleCode<const llvm::Value *>(E.getSort());
    }

    template<typename Language, typename PointsToAlgorithm>
    void getRulesOfCommand(
        RuleCode<const llvm::Value *> const& RC,
        Rules<Language,PointsToAlgorithm>& R)
    {
        switch (RC.getType())
        {
            case RCT_VAR_ASGN_ALLOC:
                R.insert(ruleVar(RC.getLvalue())=ruleAllocSite(RC.getRvalue()));
                break;
            case RCT_VAR_ASGN_NULL:
                R.insert(ruleVar(RC.getLvalue()) = ruleNull(RC.getRvalue()));
                break;
            case RCT_VAR_ASGN_VAR:
                R.insert(ruleVar(RC.getLvalue()) = ruleVar(RC.getRvalue()));
                break;
            case RCT_VAR_ASGN_REF_VAR:
                R.insert(ruleVar(RC.getLvalue()) = &ruleVar(RC.getRvalue()));
                break;
            case RCT_VAR_ASGN_DREF_VAR:
                R.insert(ruleVar(RC.getLvalue()) = *ruleVar(RC.getRvalue()));
                break;
            case RCT_DREF_VAR_ASGN_NULL:
                R.insert(*ruleVar(RC.getLvalue()) = ruleNull(RC.getRvalue()));
                break;
            case RCT_DREF_VAR_ASGN_VAR:
                R.insert(*ruleVar(RC.getLvalue()) = ruleVar(RC.getRvalue()));
                break;
            case RCT_DREF_VAR_ASGN_REF_VAR:
                R.insert(*ruleVar(RC.getLvalue()) = &ruleVar(RC.getRvalue()));
                break;
            case RCT_DREF_VAR_ASGN_DREF_VAR:
                R.insert(*ruleVar(RC.getLvalue()) = *ruleVar(RC.getRvalue()));
                break;
            case RCT_DEALLOC:
                R.insert(ruleDeallocSite(RC.getValue()));
                break;
            default:
                break;
        }
    }

}}

namespace llvm { namespace ptr {

    template<typename OutputStream, typename ExprSort>
    OutputStream& dump(OutputStream& ostr, RuleExpression<ExprSort> const& E)
    {
        return dump(ostr,E.getSort());
    }

    template<typename OutputStream, typename LSubExpr, typename RSubExpr>
    OutputStream& dump(OutputStream& ostr,
                       ASSIGNMENT<LSubExpr,RSubExpr> const& E)
    {
        dump(ostr,E.getArgument1());
        ostr << " = ";
        dump(ostr,E.getArgument2());
        return ostr;
    }

    template<typename OutputStream, typename SubExpr>
    OutputStream& dump(OutputStream& ostr, REFERENCE<SubExpr> const& E)
    {
        ostr << '&';
        dump(ostr,E.getArgument());
        return ostr;
    }

    template<typename OutputStream, typename SubExpr>
    OutputStream& dump(OutputStream& ostr, DEREFERENCE<SubExpr> const& E)
    {
        ostr << '*';
        dump(ostr,E.getArgument());
        return ostr;
    }
#if 0
    template<typename OutputStream, typename MemLoc>
    OutputStream& dump(OutputStream& ostr, VARIABLE<MemLoc> const& E)
    {
        using monty::codespy::dump;
        dump(ostr,E.getArgument());
        return ostr;
    }

    template<typename OutputStream, typename MemLoc>
    OutputStream& dump(OutputStream& ostr, ALLOC<MemLoc> const& E)
    {
        using monty::codespy::dump;
        ostr << "ALLOC@";
        dump(ostr,E.getArgument());
        return ostr;
    }

    template<typename OutputStream, typename MemLoc>
    OutputStream& dump(OutputStream& ostr, DEALLOC<MemLoc> const& E)
    {
        using monty::codespy::dump;
        ostr << "DEALLOC@";
        dump(ostr,E.getArgument());
        return ostr;
    }

    template<typename OutputStream, typename MemLoc>
    OutputStream& dump(OutputStream& ostr, NULLPTR<MemLoc> const& E)
    {
        using monty::codespy::dump;
        ostr << "NULL@";
        dump(ostr,E.getArgument());
        return ostr;
    }

    template<typename OutputStream, typename MemLoc>
    OutputStream& dump(OutputStream& ostr, RuleCode<MemLoc> const& RC)
    {
        MONTY_TMPROF_BLOCK_LVL1();

        switch (RC.getType())
        {
            case RCT_VAR_ASGN_ALLOC:
                dump(ostr,ruleVar(RC.getLvalue())=ruleAllocSite(RC.getRvalue()));
                break;
            case RCT_VAR_ASGN_NULL:
                dump(ostr,ruleVar(RC.getLvalue())=ruleNull(RC.getRvalue()));
                break;
            case RCT_VAR_ASGN_VAR:
                dump(ostr,ruleVar(RC.getLvalue()) = ruleVar(RC.getRvalue()));
                break;
            case RCT_VAR_ASGN_REF_VAR:
                dump(ostr,ruleVar(RC.getLvalue()) = &ruleVar(RC.getRvalue()));
                break;
            case RCT_VAR_ASGN_DREF_VAR:
                dump(ostr,ruleVar(RC.getLvalue()) = *ruleVar(RC.getRvalue()));
                break;
            case RCT_DREF_VAR_ASGN_NULL:
                dump(ostr,*ruleVar(RC.getLvalue()) = ruleNull(RC.getRvalue()));
                break;
            case RCT_DREF_VAR_ASGN_VAR:
                dump(ostr,*ruleVar(RC.getLvalue()) = ruleVar(RC.getRvalue()));
                break;
            case RCT_DREF_VAR_ASGN_REF_VAR:
                dump(ostr,*ruleVar(RC.getLvalue()) = &ruleVar(RC.getRvalue()));
                break;
            case RCT_DREF_VAR_ASGN_DREF_VAR:
                dump(ostr,*ruleVar(RC.getLvalue()) = *ruleVar(RC.getRvalue()));
                break;
            case RCT_DEALLOC:
                dump(ostr,ruleDeallocSite(RC.getValue()));
                break;
            default:
                ostr << "<UNKNOWN_RULE_CODE>";
                break;
        }
        return ostr;
    }
#endif
}}

#endif
