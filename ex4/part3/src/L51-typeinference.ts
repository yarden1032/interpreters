// L5-typeinference

import * as R from "ramda";
import * as A from "./L51-ast";
import * as TC from "./L51-typecheck";
import * as V from "../imp/L5-value";
import * as E from "../imp/TEnv";
import * as T from "./TExp51";
import { allT, first, rest, isEmpty } from "../shared/list";
import { isNumber, isString } from '../shared/type-predicates';
import { Result, makeFailure, makeOk, bind, safe2, zipWithResult, mapResult } from "../shared/result";

// Purpose: Make type expressions equivalent by deriving a unifier
// Return an error if the types are not unifiable.
// Exp is only passed for documentation purposes.
// te1 can be undefined when it is retrieved from a type variable which is not yet bound.
const checkEqualType = (te1: T.TExp | undefined, te2: T.TExp, exp: A.Exp): Result<true> =>
    te1 === undefined ? bind(T.unparseTExp(te2), (texp: string) => makeFailure(`Incompatible types: undefined - ${texp}`)) :
    T.isTVar(te1) && T.isTVar(te2) ? ((T.eqTVar(te1, te2) ? makeOk(true) : checkTVarEqualTypes(te1, te2, exp))) :
    T.isTVar(te1) ? checkTVarEqualTypes(te1, te2, exp) :
    T.isTVar(te2) ? checkTVarEqualTypes(te2, te1, exp) :
    T.isAtomicTExp(te1) && T.isAtomicTExp(te2) ?
        T.eqAtomicTExp(te1, te2) ? makeOk(true) : safe2((te1: string, te2: string) => makeFailure<true>(`Incompatible atomic types ${te1} - ${te2}`))
                                                    (T.unparseTExp(te1), T.unparseTExp(te2)) :
    T.isProcTExp(te1) && T.isProcTExp(te2) ? checkProcEqualTypes(te1, te2, exp) :
    // L51
    T.isClassTExp(te1) && T.isClassTExp(te2) ? checkClassEqualTypes(te1, te2, exp) :
    // A class can match a proc (symbol -> T) if the symbol is bound and is in the interface of the class
    T.isClassTExp(te1) && T.isProcTExp(te2) ? checkClassProcEqualTypes(te1, te2, exp) :
    T.isClassTExp(te2) && T.isProcTExp(te1) ? checkClassProcEqualTypes(te2, te1, exp) :
    safe2((te1: string, te2: string) => makeFailure<true>(`Incompatible types structure: ${te1} - ${te2}`))
        (T.unparseTExp(te1), T.unparseTExp(te2));

// Purpose: make two lists of equal length of type expressions equal
// Return an error if one of the pair of TExps are not compatible - true otherwise.
// Exp is only passed for documentation purposes.
const checkEqualTypes = (tes1: T.TExp[], tes2: T.TExp[], exp: A.Exp): Result<true> => {
    const checks = zipWithResult((te1, te2) => checkEqualType(te1, te2, exp), tes1, tes2);
    return bind(checks, _ => makeOk(true));
}

const checkProcEqualTypes = (te1: T.ProcTExp, te2: T.ProcTExp, exp: A.Exp): Result<true> =>
    te1.paramTEs.length !== te2.paramTEs.length ? safe2((te1: string, te2: string) => makeFailure<true>(`Wrong number of args ${te1} - ${te2}`))
                                                    (T.unparseTExp(te1), T.unparseTExp(te2)) :
    checkEqualTypes(T.procTExpComponents(te1), T.procTExpComponents(te2), exp);

// L51
const checkClassEqualTypes = (te1: T.ClassTExp, te2: T.ClassTExp, exp: A.Exp): Result<true> => {
    if (te1.methods.length != te2.methods.length) {
        return safe2((te1: string, te2: string) => makeFailure<true>(`Wrong number of methods ${te1} - ${te2}`))
                     (T.unparseTExp(te1), T.unparseTExp(te2));
    } else {
        const checks = mapResult((method) => safe2((me1: T.TExp, me2: T.TExp) => checkEqualType(me1, me2, exp))
                                                (T.classTExpMethodTExp(te1, method), T.classTExpMethodTExp(te2, method)), 
                                 T.classTExpMethods(te1));
        return bind(checks, _ => makeOk(true));
    }
}

// L51
// A class can match a proc (symbol -> T) if the symbol is bound and is in the interface of the class
// In this case T = typeOf(method)
const checkClassProcEqualTypes = (ct: T.ClassTExp, pt: T.ProcTExp, exp: A.Exp): Result<true> => {
    if ((pt.paramTEs.length != 1) || (! T.isSymbolTExp(pt.paramTEs[0])) || (! pt.paramTEs[0].val))
        return makeFailure<true>('A class can only match a proc of 1 ground symbol param');
    const method = pt.paramTEs[0].val.val;
    const constraint = bind(T.classTExpMethodTExp(ct, method), methodTExp => checkEqualType(methodTExp, pt.returnTE, exp));
    return bind(constraint, _ => makeOk(true));
}


// Purpose: check that a type variable matches a type expression
// Updates the var is needed to refer to te.
// Exp is only passed for documentation purposes.
const checkTVarEqualTypes = (tvar: T.TVar, te: T.TExp, exp: A.Exp): Result<true> =>
    T.tvarIsNonEmpty(tvar) ? checkEqualType(T.tvarContents(tvar), te, exp) :
    bind(checkNoOccurrence(tvar, te, exp), _ => { T.tvarSetContents(tvar, te); return makeOk(true); });

// Purpose: when attempting to bind tvar to te - check whether tvar occurs in te.
// Throws error if a circular reference is found.
// Exp is only passed for documentation purposes.
// Pre-conditions: Tvar is not bound
const checkNoOccurrence = (tvar: T.TVar, te: T.TExp, exp: A.Exp): Result<true> => {
    const checkList = (tes: T.TExp[]): Result<true> =>
        bind(mapResult(loop, tes), _ => makeOk(true));

    const loop = (te1: T.TExp): Result<true> =>
        T.isAtomicTExp(te1) ? makeOk(true) :
        T.isProcTExp(te1) ? checkList(T.procTExpComponents(te1)) :
        T.isClassTExp(te1) ? makeOk(true) : // L51
        T.isTVar(te1) ? (T.eqTVar(te1, tvar) ? bind(A.unparse(exp), (exp: string) => makeFailure(`Occur check error - ${te1.var} - ${tvar.var} in ${exp}`)) : makeOk(true)) :
        bind(A.unparse(exp), (exp: string) => makeFailure(`Bad type expression - ${JSON.stringify(te1)} in ${exp}`));

    return loop(te);
}

// Compute the type of Typed-AST exps to TE
// ========================================
// Compute a Typed-AST exp to a Texp on the basis of its structure and the annotations it contains.

// Initialize the TEnv with all defined classes 
// so that the user defined types are known to the type inference system.
// For each class (class : typename ...) add a pair <class.typename classTExp> to TEnv
export const makeTEnvFromClasses = (parsed: A.Parsed): E.TEnv => {
    // TODO makeTEnvFromClasses
    return E.makeEmptyTEnv();
}

// Purpose: Compute the type of a concrete expression
export const inferTypeOf = (concreteExp: string): Result<string> =>
    bind(A.parse(concreteExp), 
         parsed => {
            const tenv =  makeTEnvFromClasses(parsed);  // L51
            // console.log(`tenv = ${tenv}`);
            return bind(typeofExp(parsed, tenv),
                        T.unparseTExp);
         });

// Purpose: Compute the type of an expression
// Traverse the AST and check the type according to the exp type.
export const typeofExp = (exp: A.Parsed, tenv: E.TEnv): Result<T.TExp> =>
    A.isNumExp(exp) ? makeOk(T.makeNumTExp()) :
    A.isBoolExp(exp) ? makeOk(T.makeBoolTExp()) :
    A.isStrExp(exp) ? makeOk(T.makeStrTExp()) :
    A.isPrimOp(exp) ? TC.typeofPrim(exp) :
    A.isVarRef(exp) ? E.applyTEnv(tenv, exp.var) :
    A.isIfExp(exp) ? typeofIf(exp, tenv) :
    A.isProcExp(exp) ? typeofProc(exp, tenv) :
    A.isAppExp(exp) ? typeofApp(exp, tenv) :
    A.isLetExp(exp) ? typeofLet(exp, tenv) :
    A.isLetrecExp(exp) ? typeofLetrec(exp, tenv) :
    A.isDefineExp(exp) ? typeofDefine(exp, tenv) :
    A.isProgram(exp) ? typeofProgram(exp, tenv) :
    // L51
    A.isClassExp(exp) ? typeofClass(exp, tenv) : 
    A.isLitExp(exp) ? typeofLit(exp) :
    A.isSetExp(exp) ? typeofSet(exp, tenv) :
    exp;

// Purpose: Compute the type of a sequence of expressions
// Check all the exps in a sequence - return type of last.
// Pre-conditions: exps is not empty.
const typeofExps = (exps: A.Exp[], tenv: E.TEnv): Result<T.TExp> =>
    isEmpty(rest(exps)) ? typeofExp(first(exps), tenv) :
    bind(typeofExp(first(exps), tenv), _ => typeofExps(rest(exps), tenv));

// Purpose: compute the type of an if-exp
// Typing rule:
//   if type<test>(tenv) = boolean
//      type<then>(tenv) = t1
//      type<else>(tenv) = t1
// then type<(if test then else)>(tenv) = t1
const typeofIf = (ifExp: A.IfExp, tenv: E.TEnv): Result<T.TExp> => {
    const testTE = typeofExp(ifExp.test, tenv);
    const thenTE = typeofExp(ifExp.then, tenv);
    const altTE = typeofExp(ifExp.alt, tenv);
    const constraint1 = bind(testTE, (testTE: T.TExp) => checkEqualType(testTE, T.makeBoolTExp(), ifExp));
    const constraint2 = safe2((thenTE: T.TExp, altTE: T.TExp) => checkEqualType(thenTE, altTE, ifExp))(thenTE, altTE);
    return safe2((_c1: true, _c2: true) => thenTE)(constraint1, constraint2);
};

// Purpose: compute the type of a proc-exp
// Typing rule:
// If   type<body>(extend-tenv(x1=t1,...,xn=tn; tenv)) = t
// then type<lambda (x1:t1,...,xn:tn) : t exp)>(tenv) = (t1 * ... * tn -> t)
export const typeofProc = (proc: A.ProcExp, tenv: E.TEnv): Result<T.TExp> => {
    const argsTEs = R.map((vd) => vd.texp, proc.args);
    const extTEnv = E.makeExtendTEnv(R.map((vd) => vd.var, proc.args), argsTEs, tenv);
    const constraint1 = bind(typeofExps(proc.body, extTEnv), (bodyTE: T.TExp) => checkEqualType(bodyTE, proc.returnTE, proc));
    return bind(constraint1, _ => makeOk(T.makeProcTExp(argsTEs, proc.returnTE)));
};


// Purpose: compute the type of an app-exp
// Typing rule:
// If   type<rator>(tenv) = (t1*..*tn -> t)
//      type<rand1>(tenv) = t1
//      ...
//      type<randn>(tenv) = tn
// then type<(rator rand1...randn)>(tenv) = t
// NOTE: This procedure is different from the one in L5-typecheck
export const typeofApp = (app: A.AppExp, tenv: E.TEnv): Result<T.TExp> => {
    const ratorTE = typeofExp(app.rator, tenv);
    const randsTE = mapResult((rand) => typeofExp(rand, tenv), app.rands);
    const returnTE = T.makeFreshTVar();
    const constraint = safe2((ratorTE: T.TExp, randsTE: T.TExp[]) => checkEqualType(ratorTE, T.makeProcTExp(randsTE, returnTE), app))(ratorTE, randsTE);
    return bind(constraint, _ => makeOk(returnTE));
};

// Purpose: compute the type of a let-exp
// Typing rule:
// If   type<val1>(tenv) = t1
//      ...
//      type<valn>(tenv) = tn
//      type<body>(extend-tenv(var1=t1,..,varn=tn; tenv)) = t
// then type<let ((var1 val1) .. (varn valn)) body>(tenv) = t
export const typeofLet = (exp: A.LetExp, tenv: E.TEnv): Result<T.TExp> => {
    const vars = R.map((b) => b.var.var, exp.bindings);
    const vals = R.map((b) => b.val, exp.bindings);
    const varTEs = R.map((b) => b.var.texp, exp.bindings);
    const constraints = zipWithResult((varTE, val) => bind(typeofExp(val, tenv),
                                                           (valTE: T.TExp) => checkEqualType(varTE, valTE, exp)),
                                      varTEs, vals);
    return bind(constraints, _ => typeofExps(exp.body, E.makeExtendTEnv(vars, varTEs, tenv)));
};

// Purpose: compute the type of a letrec-exp
// We make the same assumption as in L4 that letrec only binds proc values.
// Typing rule:
//   (letrec((p1 (lambda (x11 ... x1n1) body1)) ...) body)
//   tenv-body = extend-tenv(p1=(t11*..*t1n1->t1)....; tenv)
//   tenvi = extend-tenv(xi1=ti1,..,xini=tini; tenv-body)
// If   type<body1>(tenv1) = t1
//      ...
//      type<bodyn>(tenvn) = tn
//      type<body>(tenv-body) = t
// then type<(letrec((p1 (lambda (x11 ... x1n1) body1)) ...) body)>(tenv-body) = t
export const typeofLetrec = (exp: A.LetrecExp, tenv: E.TEnv): Result<T.TExp> => {
    const ps = R.map((b) => b.var.var, exp.bindings);
    const procs = R.map((b) => b.val, exp.bindings);
    if (! allT(A.isProcExp, procs)) {
        return bind(A.unparse(exp), (exp: string) => makeFailure(`letrec - only support binding of procedures - ${exp}`));
    }
    const paramss = R.map((p) => p.args, procs);
    const bodies = R.map((p) => p.body, procs);
    const tijs = R.map((params) => R.map((p) => p.texp, params), paramss);
    const tis = R.map((proc) => proc.returnTE, procs);
    const tenvBody = E.makeExtendTEnv(ps, R.zipWith((tij, ti) => T.makeProcTExp(tij, ti), tijs, tis), tenv);
    const tenvIs = R.zipWith((params, tij) => E.makeExtendTEnv(R.map((p) => p.var, params), tij, tenvBody),
                             paramss, tijs);
    // Unfortunately ramda.zipWith does not work with 3 params
    const types = zipWithResult((bodyI, tenvI) => typeofExps(bodyI, tenvI), bodies, tenvIs)
    const constraints = bind(types, (types: T.TExp[]) => zipWithResult((typeI, ti) => checkEqualType(typeI, ti, exp), types, tis))
    return bind(constraints, _ => typeofExps(exp.body, tenvBody));
};


// Purpose: compute the type of a define
// Typing rule:
//   (define (var : texp) val)
// TODO - write the typing rule for define-exp
export const typeofDefine = (exp: A.DefineExp, tenv: E.TEnv): Result<T.VoidTExp> => {
    return makeFailure('TODO typeofDefine');
};

// Purpose: compute the type of a program
// Typing rule:
//   (L5 <exp>+)
export const typeofProgram = (exp: A.Program, tenv: E.TEnv): Result<T.TExp> =>
    // similar to typeofExps but threads variables into tenv after define-exps
    isEmpty(exp.exps) ? makeFailure("Empty program") :
    typeofProgramExps(first(exp.exps), rest(exp.exps), tenv);

const typeofProgramExps = (exp: A.Exp, exps: A.Exp[], tenv: E.TEnv): Result<T.TExp> => 
    makeFailure('TODO typeofProgramExps');


// Purpose: compute the type of a literal expression
//      - Only need to cover the case of Symbol and Pair
//      - for a symbol - record the value of the symbol in the SymbolTExp
//        so that precise type checking can be made on ground symbol values.
export const typeofLit = (exp: A.LitExp): Result<T.TExp> =>
    makeFailure(`TODO typeofLit`);

// Purpose: compute the type of a set! expression
// Typing rule:
//   (set! var val)
// TODO - write the typing rule for set-exp
export const typeofSet = (exp: A.SetExp, tenv: E.TEnv): Result<T.VoidTExp> => {
    return makeFailure('TODO typeofSet');
};

// Purpose: compute the type of a class-exp(type fields methods)
// Typing rule:
// let class-tenv = extend-tenv(field1=t1,...,fieldn=tn)
// If   type<method_1>(class-tenv) = m1
//      ...
//      type<method_k>(class-tenv) = mk
// Then type<class(type fields methods)>(tend) = = [t1 * ... * tn -> type]
export const typeofClass = (exp: A.ClassExp, tenv: E.TEnv): Result<T.TExp> => {
    return makeFailure("TODO typeofClass");
};
