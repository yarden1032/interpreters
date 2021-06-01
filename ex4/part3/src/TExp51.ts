/*
;; TExp AST
;; ========
;; Type language
;; Syntax with optional type annotations for var declarations and function return types.
;; L51 supports user defined types for classes @@@

;; Type language
;; <texp>         ::= <atomic-te> | <compound-te> | <tvar>
;; <atomic-te>    ::= <num-te> | <bool-te> | <void-te>
;; <num-te>       ::= number   // num-te()
;; <bool-te>      ::= boolean  // bool-te()
;; <str-te>       ::= string   // str-te()
;; <void-te>      ::= void     // void-te()
;; <compound-te>  ::= <proc-te> | <tuple-te>
;; <non-tuple-te> ::= <atomic-te> | <proc-te> | <tvar>
;; <proc-te>      ::= [ <tuple-te> -> <non-tuple-te> ] // proc-te(param-tes: list(te), return-te: te)
;; <tuple-te>     ::= <non-empty-tuple-te> | <empty-te>
;; <non-empty-tuple-te> ::= ( <non-tuple-te> *)* <non-tuple-te> // tuple-te(tes: list(te))
;; <empty-te>     ::= Empty
;; <tvar>         ::= a symbol starting with T // tvar(id: Symbol, contents; Box(string|boolean))
;; <class-te>     ::= (class typename (method : <texp>) ...) // class-te((id, proc-te)[]) @@@

;; Examples of type expressions
;; number
;; boolean
;; void
;; [number -> boolean]
;; [number * number -> boolean]
;; [number -> [number -> boolean]]
;; [Empty -> number]
;; [Empty -> void]
;; (class pair (first : (Empty -> number)) (rest : (Empty -> number))) // @@@L51
*/
import { chain, concat, equals, find, join, map, startsWith, split, uniq } from "ramda";
import { Sexp } from "s-expression";
import { isEmpty } from "../shared/list";
import { isArray, isBoolean, isString } from '../shared/type-predicates';
import { makeBox, setBox, unbox, Box } from '../shared/box';
import { cons, first, rest } from '../shared/list';
import { Result, bind, makeOk, makeFailure, safe2, mapResult } from "../shared/result";
import { isToken, parse as p } from "../shared/parser";
import { makeSymbolSExp, SymbolSExp } from "../imp/L5-value";

export type TExp =  AtomicTExp | CompoundTExp | TVar;
export const isTExp = (x: any): x is TExp => isAtomicTExp(x) || isCompoundTExp(x) || isTVar(x);

// L51
export type AtomicTExp = NumTExp | BoolTExp | StrTExp | VoidTExp | SymbolTExp | PairTExp;
export const isAtomicTExp = (x: any): x is AtomicTExp =>
    isNumTExp(x) || isBoolTExp(x) || isStrTExp(x) || isVoidTExp(x) || isSymbolTExp(x) || isPairTExp(x);
export type LitTExp = SymbolTExp | PairTExp;
export const isLitTExp = (x: any): x is LitTExp => isSymbolTExp(x) || isPairTExp(x);

export type CompoundTExp = ProcTExp | TupleTExp | ClassTExp;
export const isCompoundTExp = (x: any): x is CompoundTExp => isProcTExp(x) || isTupleTExp(x) || isClassTExp(x); // L51

export type NonTupleTExp = AtomicTExp | ProcTExp | TVar | ClassTExp; // L51
export const isNonTupleTExp = (x: any): x is NonTupleTExp =>
    isAtomicTExp(x) || isProcTExp(x) || isTVar(x) || isClassTExp(x); // L51

export type NumTExp = { tag: "NumTExp" };
export const makeNumTExp = (): NumTExp => ({tag: "NumTExp"});
export const isNumTExp = (x: any): x is NumTExp => x.tag === "NumTExp";

export type BoolTExp = { tag: "BoolTExp" };
export const makeBoolTExp = (): BoolTExp => ({tag: "BoolTExp"});
export const isBoolTExp = (x: any): x is BoolTExp => x.tag === "BoolTExp";

export type StrTExp = { tag: "StrTExp" };
export const makeStrTExp = (): StrTExp => ({tag: "StrTExp"});
export const isStrTExp = (x: any): x is StrTExp => x.tag === "StrTExp";

export type VoidTExp = { tag: "VoidTExp" };
export const makeVoidTExp = (): VoidTExp => ({tag: "VoidTExp"});
export const isVoidTExp = (x: any): x is VoidTExp => x.tag === "VoidTExp";

// L51
export type SymbolTExp = { tag: "SymbolTExp", val?: SymbolSExp };
export const makeSymbolTExp = (s?: SymbolSExp): SymbolTExp => ({tag: "SymbolTExp", val: s});
export const isSymbolTExp = (x: any): x is SymbolTExp => x.tag === "SymbolTExp";

// L51
export type PairTExp = { tag: "PairTExp" };
export const makePairTExp = (): PairTExp => ({tag: "PairTExp"});
export const isPairTExp = (x: any): x is PairTExp => x.tag === "PairTExp";


// proc-te(param-tes: list(te), return-te: te)
export type ProcTExp = { tag: "ProcTExp"; paramTEs: TExp[]; returnTE: TExp; };
export const makeProcTExp = (paramTEs: TExp[], returnTE: TExp): ProcTExp =>
    ({tag: "ProcTExp", paramTEs: paramTEs, returnTE: returnTE});
export const isProcTExp = (x: any): x is ProcTExp => x.tag === "ProcTExp";
// Uniform access to all components of a ProcTExp
export const procTExpComponents = (pt: ProcTExp): TExp[] =>
    [...pt.paramTEs, pt.returnTE];

export type TupleTExp = NonEmptyTupleTExp | EmptyTupleTExp;
export const isTupleTExp = (x: any): x is TupleTExp =>
    isNonEmptyTupleTExp(x) || isEmptyTupleTExp(x);

export interface EmptyTupleTExp { tag: "EmptyTupleTExp" }
export const makeEmptyTupleTExp = (): EmptyTupleTExp => ({tag: "EmptyTupleTExp"});
export const isEmptyTupleTExp = (x: any): x is EmptyTupleTExp => x.tag === "EmptyTupleTExp";

// NonEmptyTupleTExp(TEs: NonTupleTExp[])
export interface NonEmptyTupleTExp { tag: "NonEmptyTupleTExp"; TEs: NonTupleTExp[]; }
export const makeNonEmptyTupleTExp = (tes: NonTupleTExp[]): NonEmptyTupleTExp =>
    ({tag: "NonEmptyTupleTExp", TEs: tes});
export const isNonEmptyTupleTExp = (x: any): x is NonEmptyTupleTExp => x.tag === "NonEmptyTupleTExp";

// L51
export interface ClassTExp { tag: "ClassTExp", typename: string, methods: [string, TExp][]};
// sort to make it easy to compare classTExps
export const makeClassTExp = (typename: string, methods: [string, TExp][]): ClassTExp => 
    ({tag: "ClassTExp", typename: typename, methods: methods.sort()});
export const isClassTExp = (x: any): x is ClassTExp => x.tag === "ClassTExp";

export const classTExpMethods = (ct: ClassTExp): string[] => 
    map((pair: [string, TExp]) => pair[0], ct.methods);

// Method type for a class given its name
export const classTExpMethodTExp = (ct: ClassTExp, method: string): Result<TExp> => {
    const pair = find((pair: [string, TExp]) => pair[0] === method, ct.methods);
    return pair ? makeOk(pair[1]) : makeFailure(`method not found ${method}`);
}

// Uniform access to all components of a ClassTExp
export const classTExpComponents = (ct: ClassTExp): TExp[] =>
    map(p => p[1], ct.methods);

// TVar: Type Variable with support for dereferencing (TVar -> TVar)
export type TVar = { tag: "TVar"; var: string; contents: Box<undefined | TExp>; };
export const isEmptyTVar = (x: any): x is TVar =>
    (x.tag === "TVar") && unbox(x.contents) === undefined;
export const makeTVar = (v: string): TVar =>
    ({tag: "TVar", var: v, contents: makeBox(undefined)});
const makeTVarGen = (): () => TVar => {
    let count: number = 0;
    return () => {
        count++;
        return makeTVar(`T_${count}`);
    }
}
export const makeFreshTVar = makeTVarGen();
export const isTVar = (x: any): x is TVar => x.tag === "TVar";
export const eqTVar = (tv1: TVar, tv2: TVar): boolean => tv1.var === tv2.var;
export const tvarContents = (tv: TVar): undefined | TExp => unbox(tv.contents);
export const tvarSetContents = (tv: TVar, val: TExp): void =>
    setBox(tv.contents, val);
export const tvarIsNonEmpty = (tv: TVar): boolean => tvarContents(tv) !== undefined;
export const tvarDeref = (te: TExp): TExp => {
    if (! isTVar(te)) return te;
    const contents = tvarContents(te);
    if (contents === undefined)
        return te;
    else if (isTVar(contents))
        return tvarDeref(contents);
    else
        return contents;
}

// ========================================================
// TExp Utilities

// Purpose: uniform access to atomic types
export const atomicTExpName = (te: AtomicTExp): string => te.tag;

export const eqAtomicTExp = (te1: AtomicTExp, te2: AtomicTExp): boolean =>
    atomicTExpName(te1) === atomicTExpName(te2);


// ========================================================
// TExp parser

export const parseTE = (t: string): Result<TExp> => 
    bind(p(t), parseTExp);

/*
;; Purpose: Parse a type expression
;; Type: [SExp -> TExp[]]
;; Example:
;; parseTExp("number") => 'num-te
;; parseTExp('boolean') => 'bool-te
;; parseTExp('T1') => '(tvar T1)
;; parseTExp('(T * T -> boolean)') => '(proc-te ((tvar T) (tvar T)) bool-te)
;; parseTExp('(number -> (number -> number)') => '(proc-te (num-te) (proc-te (num-te) num-te))
;; parseTExp('symbol') => 'symbol-te'
;; parseTExp('cons') => 'pair-te'
;; parseTExp('symbol-a-b') => '(symbol-te a-b)'
*/
export const parseTExp = (texp: Sexp): Result<TExp> => 
    (texp === "number") ? makeOk(makeNumTExp()) :
    (texp === "boolean") ? makeOk(makeBoolTExp()) :
    (texp === "void") ? makeOk(makeVoidTExp()) :
    (texp === "string") ? makeOk(makeStrTExp()) :
    (texp === "cons") ? makeOk(makePairTExp()) :
    (texp === "symbol") ? makeOk(makeSymbolTExp()) :
    // parse symbol-a as SymbolTExp(Symbol(a))
    (isString(texp) && startsWith("symbol-", texp)) ? makeOk(makeSymbolTExp(makeSymbolSExp(split("symbol-", texp)[1]))) :
    isString(texp) ? makeOk(makeTVar(texp)) : 
    isArray(texp) ? parseCompoundTExp(texp) :
    makeFailure(`Unexpected TExp - ${texp}`);

/*
;; expected structure: (<params> -> <returnte>) or [class [method : texp],...] or proc (t1 -> t2)
;; expected exactly one -> in the list
;; We do not accept (a -> b -> c) - must parenthesize
*/
const parseCompoundTExp = (texps: Sexp[]): Result<CompoundTExp> =>
    texps[0] === 'class' ? parseClassTExp(texps) :
    parseProcTExp(texps);

// L51
// Example: (class typename (first : (Empty -> number)) (rest : (Empty -> number)))
const parseClassTExp = (texps: Sexp[]): Result<ClassTExp> => {
    const parseGoodMethod = (method: Sexp, texp: Sexp) : Result<[string, TExp]> => 
        !isString(method) ? makeFailure(`Method must be of shape (method : texp) - got ${method}`) :
        bind(parseTExp(texp), te => makeOk([method, te]));

    const methods : Result<[string, TExp][]> = mapResult((sexp: Sexp) : Result<[string, TExp]> => 
        isToken(sexp) ? makeFailure(`Method must be of shape (method : texp) - got ${sexp}`) :
        sexp.length != 3 ? makeFailure(`Method must be of shape (method : texp) - got ${sexp}`) :
        sexp[1] != ":" ? makeFailure(`Method must be of shape (method : texp) - got ${sexp}`) :
        parseGoodMethod(sexp[0], sexp[2]),
    rest(rest(texps)));

    const typename = texps[1];
    if (!isString(typename))
        return makeFailure(`Class name must be a string - got ${texps[1]}`);
    else
        return bind(methods, ms => makeOk(makeClassTExp(typename, ms)));
}

const parseProcTExp = (texps: Sexp[]): Result<ProcTExp> => {
    const pos = texps.indexOf('->');
    return (pos === -1)  ? makeFailure(`Procedure type expression without -> - ${texps}`) :
           (pos === 0) ? makeFailure(`No param types in proc texp - ${texps}`) :
           (pos === texps.length - 1) ? makeFailure(`No return type in proc texp - ${texps}`) :
           (texps.slice(pos + 1).indexOf('->') > -1) ? makeFailure(`Only one -> allowed in a procexp - ${texps}`) :
           safe2((args: TExp[], returnTE: TExp) => makeOk(makeProcTExp(args, returnTE)))
                (parseTupleTExp(texps.slice(0, pos)), parseTExp(texps[pos + 1]));
};

/*
;; Expected structure: <te1> [* <te2> ... * <ten>]?
;; Or: Empty
*/
const parseTupleTExp = (texps: Sexp[]): Result<TExp[]> => {
    const isEmptyTuple = (texps: Sexp[]): boolean =>
        (texps.length === 1) && (texps[0] === 'Empty');
    // [x1 * x2 * ... * xn] => [x1,...,xn]
    const splitEvenOdds = (texps: Sexp[]): Result<Sexp[]> =>
        isEmpty(texps) ? makeOk([]) :
        isEmpty(rest(texps)) ? makeOk(texps) :
        texps[1] !== '*' ? makeFailure(`Parameters of procedure type must be separated by '*': ${texps}`) :
        bind(splitEvenOdds(texps.slice(2)), (sexps: Sexp[]) => makeOk([texps[0], ...sexps]));

    return isEmptyTuple(texps) ? makeOk([]) : bind(splitEvenOdds(texps),
                                                   (argTEs: Sexp[]) => mapResult(parseTExp, argTEs));
}

/*
;; Purpose: Unparse a type expression Texp into its concrete form
*/
export const unparseTExp = (te: TExp): Result<string> => {
    const unparseTuple = (paramTes: TExp[]): Result<string[]> =>
        isEmpty(paramTes) ? makeOk(["Empty"]) :
        safe2((paramTE: string, paramTEs: string[]) => makeOk(cons(paramTE, chain(te => ['*', te], paramTEs))))
            (unparseTExp(first(paramTes)), mapResult(unparseTExp, rest(paramTes)));

    const unparseTVar = (varName: string, contents?: TExp): Result<string | string[]> =>
        !contents ? makeOk(varName) :
        isClassTExp(contents) ? makeOk(contents.typename) :
        up(contents);

    const up = (x?: TExp): Result<string | string[]> =>
        isNumTExp(x) ? makeOk('number') :
        isBoolTExp(x) ? makeOk('boolean') :
        isStrTExp(x) ? makeOk('string') :
        isVoidTExp(x) ? makeOk('void') :
        isSymbolTExp(x) ? x.val ? makeOk(`symbol-${x.val.val}`) : makeOk('symbol') :
        isPairTExp(x) ? makeOk('cons') : 
        isTVar(x) ? unparseTVar(x.var, tvarContents(x)) :
        isProcTExp(x) ? safe2((paramTEs: string[], returnTE: string) => makeOk([...paramTEs, '->', returnTE]))
                            (unparseTuple(x.paramTEs), unparseTExp(x.returnTE)) :
        isEmptyTupleTExp(x) ? makeOk("Empty") :
        isNonEmptyTupleTExp(x) ? unparseTuple(x.TEs) :
        x === undefined ? makeFailure("Undefined TVar") :
        // L51
        isClassTExp(x) ? bind(mapResult((method: [string, TExp]) => bind(unparseTExp(method[1]), texp => makeOk(`(${method[0]} : ${texp})`)), x.methods),
                              s => makeOk(`(class ${x.typename} ${join(" ", s)})`)) :
        x;

    const unparsed = up(te);
    return bind(unparsed,
                (x: string | string[]) => isString(x) ? makeOk(x) :
                                          isArray(x) ? makeOk(`(${x.join(' ')})`) :
                                          x);
}

// ============================================================
// equivalentTEs: 2 TEs are equivalent up to variable renaming.
// For example:
// equivalentTEs(parseTExp('(T1 -> T2)'), parseTExp('(T3 -> T4)'))


// Signature: matchTVarsInTE(te1, te2, succ, fail)
// Type: [Texp * Texp * [List(Pair(Tvar, Tvar)) -> T1] * [Empty -> T2]] |
//       [List(Texp) * List(Texp) * ...]
// Purpose:   Receives two type expressions or list(texps) plus continuation procedures
//            and, in case they are equivalent, pass a mapping between
//            type variable they include to succ. Otherwise, invoke fail.
// Examples:
// matchTVarsInTE(parseTExp('(Number * T1 -> T1)',
//                parseTExp('(Number * T7 -> T5)'),
//                (x) => x,
//                () => false) ==> [[T1, T7], [T1, T5]]
// matchTVarsInTE(parseTExp('(Boolean * T1 -> T1)'),
//                parseTExp('(Number * T7 -> T5)'),
//                (x) => x,
//                () => false)) ==> false

type Pair<T1, T2> = {left: T1; right: T2};

const matchTVarsInTE = <T1, T2>(te1: TExp, te2: TExp,
                                succ: (mapping: Array<Pair<TVar, TVar>>) => T1,
                                fail: () => T2): T1 | T2 => {

    // console.log("Comparing 2 Texps: %j - %j", te1, te2);

    return (isTVar(te1) || isTVar(te2)) ? matchTVarsinTVars(tvarDeref(te1), tvarDeref(te2), succ, fail) :
    (isAtomicTExp(te1) || isAtomicTExp(te2)) ?
        ((isAtomicTExp(te1) && isAtomicTExp(te2) && eqAtomicTExp(te1, te2)) ? succ([]) : fail()) :
    (isProcTExp(te1) && isProcTExp(te2)) ? matchTVarsInTProcs(te1, te2, succ, fail) : 
    // L51
    (isClassTExp(te1) && isClassTExp(te2)) ? matchTVarsInClasses(te1, te2, succ, fail) :
    fail();
}
// te1 and te2 are the result of tvarDeref
const matchTVarsinTVars = <T1, T2>(te1: TExp, te2: TExp,
                                    succ: (mapping: Array<Pair<TVar, TVar>>) => T1,
                                    fail: () => T2): T1 | T2 =>
    (isTVar(te1) && isTVar(te2)) ? (eqTVar(te1, te2) ? succ([]) : succ([{left: te1, right: te2}])) :
    (isTVar(te1) || isTVar(te2)) ? fail() :
    matchTVarsInTE(te1, te2, succ, fail);

const matchTVarsInTProcs = <T1, T2>(te1: ProcTExp, te2: ProcTExp,
        succ: (mapping: Array<Pair<TVar, TVar>>) => T1,
        fail: () => T2): T1 | T2 => {

    // safe2((t1, t2) => makeOk(console.log("Comparing 2 proc-texps: %j - %j", t1, t2)))
    //  (unparseTExp(te1), unparseTExp(te2));

    return matchTVarsInTEs(procTExpComponents(te1), procTExpComponents(te2), succ, fail);
}

// L51
const matchTVarsInClasses = <T1, T2>(te1: ClassTExp, te2: ClassTExp,
        succ: (mapping: Array<Pair<TVar, TVar>>) => T1,
        fail: () => T2): T1 | T2 => {

    // console.log("Comparing 2 class-texps: %j - %j", te1.typename, te2.typename);
    // console.log("Methods: %j - %j", classTExpMethods(te1), classTExpMethods(te2));

    return !equals(classTExpMethods(te1), classTExpMethods(te2)) ? fail() :
    te1.typename != te2.typename ? succ([{left: makeTVar(te1.typename), right: makeTVar(te2.typename)}]) : 
    succ([]);
}

const matchTVarsInTEs = <T1, T2>(te1: TExp[], te2: TExp[],
                                    succ: (mapping: Array<Pair<TVar, TVar>>) => T1,
                                    fail: () => T2): T1 | T2 =>
    (isEmpty(te1) && isEmpty(te2)) ? succ([]) :
    (isEmpty(te1) || isEmpty(te2)) ? fail() :
    // Match first then continue on rest
    matchTVarsInTE(first(te1), first(te2),
                    (subFirst) => matchTVarsInTEs(rest(te1), rest(te2), 
                                        (subRest) => succ(concat(subFirst, subRest)), 
                                        fail),
                    fail);

// Signature: equivalent-tes?(te1, te2)
// Purpose:   Check whether 2 type expressions are equivalent up to
//            type variable renaming.
// Example:  equivalentTEs(parseTExp('(T1 * (Number -> T2) -> T3))',
//                         parseTExp('(T4 * (Number -> T5) -> T6))') => #t
export const equivalentTEs = (te1: TExp, te2: TExp): boolean => {
    // console.log(`EqTEs ${JSON.stringify(te1, null, 4)} - ${JSON.stringify(te2, null, 4)}`);
    const tvarsPairs = matchTVarsInTE(te1, te2, (x) => x, () => false);
    // console.log(`EqTEs pairs = %j`, tvarsPairs);
    if (isBoolean(tvarsPairs))
        return false;
    else {
        return (uniq(map((p) => p.left.var, tvarsPairs)).length === uniq(map((p) => p.right.var, tvarsPairs)).length);
    }
};
