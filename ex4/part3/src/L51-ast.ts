// ===========================================================
// AST type models for L51
// L5 extends L4 with:
// optional type annotations
// L51 extends L5 with:
// typed class construct

import { concat, chain, join, map, zipWith } from "ramda";
import { Sexp, Token } from 's-expression';
import { isCompoundSExp, isEmptySExp, isSymbolSExp, makeCompoundSExp, makeEmptySExp, makeSymbolSExp, SExpValue, valueToString } from '../imp/L5-value';
import { allT, first, rest, second, isEmpty } from '../shared/list';
import { parse as p, isToken, isSexpString } from "../shared/parser";
import { Result, bind, makeFailure, mapResult, makeOk, safe2, safe3 } from "../shared/result";
import { isArray, isString, isNumericString, isIdentifier } from "../shared/type-predicates";
import { isTVar, makeFreshTVar, makeTVar, parseTExp, unparseTExp, TVar, TExp } from './TExp51';
import { makeClassTExp, ClassTExp } from "./TExp51";

/*
// =============================================================================
// Examples of type annotated programs
// (define [x : number] 5)
// (define [f : [number -> number]] (lambda ([x : number]) : number (* x x))
// (define [f : [number * number -> number]] (lambda ([x : number] [y : number]) : number (* x x))
// (define f (lambda ([x : number]) (* x x)))  // no type annotation on f and on return value of lambda
// (let (([a : number] 1)
//       ([b : boolean] #t))
//   (if b a (+ a 1)))
// (define [id : (T1 -> T1)] (lambda ([x : T1]) : T1 x))
;;
// The only changes in the syntax of L5 are optional type annotations in var-decl and proc-exp
;;
// <program> ::= (L5 <exp>+)                  / Program(exps:List(exp))
// <exp> ::= <define> | <cexp>                / DefExp | CExp
// <define> ::= ( define <var-decl> <cexp> )  / DefExp(var:VarDecl, val:CExp)
// <var> ::= <identifier>                     / VarRef(var:string)
// <cexp> ::= <number>                        / NumExp(val:number)
//         |  <boolean>                       / BoolExp(val:boolean)
//         |  <string>                        / StrExp(val:string)
//         |  <var-ref>
//         |  ( lambda ( <var-decl>* ) <TExp>? <cexp>+ ) / ProcExp(args:VarDecl[], body:CExp[], returnTE: TExp))
//         |  ( if <cexp> <cexp> <cexp> )     / IfExp(test: CExp, then: CExp, alt: CExp)
//         |  ( quote <sexp> )                / LitExp(val:SExp)
//         |  ( <cexp> <cexp>* )              / AppExp(operator:CExp, operands:CExp[]))
//         |  ( let ( <binding>* ) <cexp>+ )  / LetExp(bindings:Binding[], body:CExp[]))
//         |  ( letrec ( binding*) <cexp>+ )  / LetrecExp(bindings:Bindings[], body: CExp)
//         |  ( set! <var> <cexp>)            / SetExp(var: varRef, val: CExp)
//         |  ( class [: <typeVar>]? ( <var>+ ) ( <binding>+ ) ClassExp(typeName: TVar, args:VarDecl[],bindings:Binding[])) //L51
// <binding>  ::= ( <var-decl> <cexp> )            / Binding(var:VarDecl, val:Cexp)
// <prim-op>  ::= + | - | * | / | < | > | = | not |  eq? | string=?
//                  | cons | car | cdr | list? | number?
//                  | boolean? | symbol? | string?
//                  | display | newline
// <num-exp>  ::= a number token
// <bool-exp> ::= #t | #f
// <var-ref>  ::= an identifier token         / VarRef(var)
// <var-decl> ::= an identifier token | (var : TExp) / VarRef(var, TE: TExp) ##### L5
// <sexp>     ::= symbol | number | bool | string | ( <sexp>* )              ##### L3
*/

// A value returned by parseL5
export type Parsed = Exp | Program;

export type Exp = DefineExp | CExp;
export const isExp = (x: any): x is Exp => isDefineExp(x) || isCExp(x);

export type CExp =  AtomicExp | CompoundExp;
export const isCExp = (x: any): x is CExp => isAtomicExp(x) || isCompoundExp(x);

export type AtomicExp = NumExp | BoolExp | StrExp | PrimOp | VarRef;
export const isAtomicExp = (x: any): x is AtomicExp =>
    isNumExp(x) || isBoolExp(x) || isStrExp(x) ||
    isPrimOp(x) || isVarRef(x);

export type CompoundExp = AppExp | IfExp | ProcExp | LetExp | LitExp | LetrecExp | SetExp | ClassExp;  // L51
export const isCompoundExp = (x: any): x is CompoundExp =>
    isAppExp(x) || isIfExp(x) || isProcExp(x) || isLitExp(x) || isLetExp(x) || isLetrecExp(x) || isSetExp(x) || isClassExp(x); // L51
export const expComponents = (e: Exp): CExp[] =>
    isIfExp(e) ? [e.test, e.then, e.alt] :
    isProcExp(e) ? e.body :
    isLetExp(e) ? [...e.body, ...map((b) => b.val, e.bindings)] :
    isLetrecExp(e) ? [...e.body, ...map((b) => b.val, e.bindings)] :
    isAppExp(e) ? [e.rator, ...e.rands] :
    isSetExp(e) ? [e.val] :
    isDefineExp(e) ? [e.val] :
    isClassExp(e) ? map((b) => b.val, e.methods) : // L51
    isAtomicExp(e) ? [] :
    isLitExp(e) ? [] :
    e; 

// Type definitions
export interface Program {tag: "Program"; exps: Exp[]; }
export const makeProgram = (exps: Exp[]): Program => ({tag: "Program", exps: exps});
export const isProgram = (x: any): x is Program => x.tag === "Program";

export interface DefineExp {tag: "DefineExp"; var: VarDecl; val: CExp; }
export const makeDefineExp = (v: VarDecl, val: CExp): DefineExp =>
    ({tag: "DefineExp", var: v, val: val});
export const isDefineExp = (x: any): x is DefineExp => x.tag === "DefineExp";

export interface NumExp {tag: "NumExp"; val: number; }
export const makeNumExp = (n: number): NumExp => ({tag: "NumExp", val: n});
export const isNumExp = (x: any): x is NumExp => x.tag === "NumExp";

export interface BoolExp {tag: "BoolExp"; val: boolean; }
export const makeBoolExp = (b: boolean): BoolExp => ({tag: "BoolExp", val: b});
export const isBoolExp = (x: any): x is BoolExp => x.tag === "BoolExp";

export interface StrExp {tag: "StrExp"; val: string; }
export const makeStrExp = (s: string): StrExp => ({tag: "StrExp", val: s});
export const isStrExp = (x: any): x is StrExp => x.tag === "StrExp";

export interface PrimOp {tag: "PrimOp"; op: PrimOpKeyword; }
export const makePrimOp = (op: PrimOpKeyword): PrimOp => ({tag: "PrimOp", op: op});
export const isPrimOp = (x: any): x is PrimOp => x.tag === "PrimOp";

export interface VarRef {tag: "VarRef"; var: string; }
export const makeVarRef = (v: string): VarRef => ({tag: "VarRef", var: v});
export const isVarRef = (x: any): x is VarRef => x.tag === "VarRef";

export interface VarDecl {tag: "VarDecl"; var: string; texp: TExp}
export const makeVarDecl = (v: string, te: TExp): VarDecl => ({tag: "VarDecl", var: v, texp: te});
export const isVarDecl = (x: any): x is VarDecl => x.tag === "VarDecl";

export interface AppExp {tag: "AppExp"; rator: CExp; rands: CExp[]; }
export const makeAppExp = (rator: CExp, rands: CExp[]): AppExp =>
    ({tag: "AppExp", rator: rator, rands: rands});
export const isAppExp = (x: any): x is AppExp => x.tag === "AppExp";

export interface IfExp {tag: "IfExp"; test: CExp; then: CExp; alt: CExp; }
export const makeIfExp = (test: CExp, then: CExp, alt: CExp): IfExp =>
    ({tag: "IfExp", test: test, then: then, alt: alt});
export const isIfExp = (x: any): x is IfExp => x.tag === "IfExp";

export interface ProcExp {tag: "ProcExp"; args: VarDecl[], body: CExp[]; returnTE: TExp }
export const makeProcExp = (args: VarDecl[], body: CExp[], returnTE: TExp): ProcExp =>
    ({tag: "ProcExp", args: args, body: body, returnTE: returnTE});
export const isProcExp = (x: any): x is ProcExp => x.tag === "ProcExp";

export interface Binding {tag: "Binding"; var: VarDecl; val: CExp; }
export const makeBinding = (v: VarDecl, val: CExp): Binding =>
    ({tag: "Binding", var: v, val: val});
export const isBinding = (x: any): x is Binding => x.tag === "Binding";

export interface LetExp {tag: "LetExp"; bindings: Binding[]; body: CExp[]; }
export const makeLetExp = (bindings: Binding[], body: CExp[]): LetExp =>
    ({tag: "LetExp", bindings: bindings, body: body});
export const isLetExp = (x: any): x is LetExp => x.tag === "LetExp";

export interface LitExp {tag: "LitExp"; val: SExpValue; }
export const makeLitExp = (val: SExpValue): LitExp => ({tag: "LitExp", val: val});
export const isLitExp = (x: any): x is LitExp => x.tag === "LitExp";

export interface LetrecExp {tag: "LetrecExp"; bindings: Binding[]; body: CExp[]; }
export const makeLetrecExp = (bindings: Binding[], body: CExp[]): LetrecExp =>
    ({tag: "LetrecExp", bindings: bindings, body: body});
export const isLetrecExp = (x: any): x is LetrecExp => x.tag === "LetrecExp";

export interface SetExp {tag: "SetExp"; var: VarRef; val: CExp; }
export const makeSetExp = (v: VarRef, val: CExp): SetExp =>
    ({tag: "SetExp", var: v, val: val});
export const isSetExp = (x: any): x is SetExp => x.tag === "SetExp";

// L51
export interface ClassExp {tag: "ClassExp"; typeName: TVar, fields : VarDecl[]; methods: Binding[]; }
export const makeClassExp = (typeName: TVar, fields : VarDecl[], methods : Binding[]): ClassExp =>
    ({tag: "ClassExp", typeName: typeName, fields : fields, methods : methods});
export const isClassExp = (x: any): x is ClassExp => x.tag === "ClassExp";


// To help parser - define a type for reserved key words.
export type SpecialFormKeyword = "lambda" | "let" | "letrec" | "if" | "set!" | "quote" | "class"; // L51
const isSpecialFormKeyword = (x: string): x is SpecialFormKeyword =>
    ["if", "lambda", "let", "quote", "letrec", "set!", "class"].includes(x);

/*
    ;; <prim-op>  ::= + | - | * | / | < | > | = | not | and | or | eq? | string=?
    ;;                  | cons | car | cdr | pair? | number? | list
    ;;                  | boolean? | symbol? | string?      ##### L3
*/
export type PrimOpKeyword = "+" | "-" | "*" | "/" | ">" | "<" | "=" | "not" | "and" | "or" | "eq?" | "string=?" | 
        "cons" | "car" | "cdr" | "list" | "pair?" | "list?" | "number?" | "boolean?" | "symbol?" | "string?" |
        "display" | "newline";
const isPrimOpKeyword = (x: string): x is PrimOpKeyword =>
    ["+", "-", "*", "/", ">", "<", "=", "not", "and", "or", 
     "eq?", "string=?", "cons", "car", "cdr", "list", "pair?",
     "list?", "number?", "boolean?", "symbol?", "string?", "display", "newline"].includes(x);

// ========================================================
// Parsing

export const parse = (x: string): Result<Parsed> =>
    x.startsWith('(L5') ? parseL5(x) :
    bind(p(x), parseL5Exp);

export const parseL5 = (x: string): Result<Program> =>
    bind(p(x), parseL5Program);

export const parseL5Program = (sexp: Sexp): Result<Program> =>
    sexp === "" || isEmpty(sexp) ? makeFailure("Unexpected empty program") :
    isToken(sexp) ? makeFailure("Program cannot be a single token") :
    isArray(sexp) ? parseL5GoodProgram(first(sexp), rest(sexp)) :
    sexp;

const parseL5GoodProgram = (keyword: Sexp, body: Sexp[]): Result<Program> =>
    keyword === "L5" && !isEmpty(body) ? bind(mapResult(parseL5Exp, body),
                                              (exps: Exp[]) => makeOk(makeProgram(exps))) :
    makeFailure("Program must be of the form (L5 <exp>+)");

export const parseL5Exp = (sexp: Sexp): Result<Exp> =>
    isEmpty(sexp) ? makeFailure("Exp cannot be an empty list") :
    isArray(sexp) ? parseL5CompoundExp(first(sexp), rest(sexp)) :
    isToken(sexp) ? parseL5Atomic(sexp) :
    sexp;

export const parseL5CompoundExp = (op: Sexp, params: Sexp[]): Result<Exp> =>
    op === "define" ? parseDefine(params) :
    parseL5CompoundCExp(op, params);

export const parseL5CompoundCExp = (op: Sexp, params: Sexp[]): Result<CExp> =>
    isString(op) && isSpecialFormKeyword(op) ? parseL5SpecialForm(op, params) :
    parseAppExp(op, params);

export const parseL5SpecialForm = (op: SpecialFormKeyword, params: Sexp[]): Result<CExp> =>
    isEmpty(params) ? makeFailure("Empty args for special form") :
    op === "if" ? parseIfExp(params) :
    op === "lambda" ? parseProcExp(first(params), rest(params)) :
    op === "let" ? parseLetExp(first(params), rest(params)) :
    op === "quote" ? parseLitExp(first(params)) :
    op === "letrec" ? parseLetrecExp(first(params), rest(params)) :
    op === "set!" ? parseSetExp(params) :
    op === "class" ? parseClassExp(params) : // L51
    op;

export const parseDefine = (params: Sexp[]): Result<DefineExp> =>
    isEmpty(params) ? makeFailure("define missing 2 arguments") :
    isEmpty(rest(params)) ? makeFailure("define missing 1 arguments") :
    ! isEmpty(rest(rest(params))) ? makeFailure("define has too many arguments") :
    parseGoodDefine(first(params), second(params));

const parseGoodDefine = (variable: Sexp, val: Sexp): Result<DefineExp> =>
    ! isConcreteVarDecl(variable) ? makeFailure("First arg of define must be an identifier") :
    safe2((varDecl: VarDecl, val: CExp) => makeOk(makeDefineExp(varDecl, val)))
        (parseVarDecl(variable), parseL5CExp(val));

export const parseL5Atomic = (token: Token): Result<AtomicExp> =>
    token === "#t" ? makeOk(makeBoolExp(true)) :
    token === "#f" ? makeOk(makeBoolExp(false)) :
    isString(token) && isNumericString(token) ? makeOk(makeNumExp(+token)) :
    isString(token) && isPrimOpKeyword(token) ? makeOk(makePrimOp(token)) :
    isString(token) ? makeOk(makeVarRef(token)) :
    makeOk(makeStrExp(token.toString()));

export const parseL5CExp = (sexp: Sexp): Result<CExp> =>
    isEmpty(sexp) ? makeFailure("CExp cannot be an empty list") :
    isArray(sexp) ? parseL5CompoundCExp(first(sexp), rest(sexp)) :
    isToken(sexp) ? parseL5Atomic(sexp) :
    sexp;

const parseAppExp = (op: Sexp, params: Sexp[]): Result<AppExp> =>
    safe2((rator: CExp, rands: CExp[]) => makeOk(makeAppExp(rator, rands)))
        (parseL5CExp(op), mapResult(parseL5CExp, params));

const parseIfExp = (params: Sexp[]): Result<IfExp> =>
    params.length !== 3 ? makeFailure("Expression not of the form (if <cexp> <cexp> <cexp>)") :
    bind(mapResult(parseL5CExp, params),
         (cexps: CExp[]) => makeOk(makeIfExp(cexps[0], cexps[1], cexps[2])));

// (lambda (<vardecl>*) [: returnTE]? <CExp>+)
const parseProcExp = (vars: Sexp, rest: Sexp[]): Result<ProcExp> => {
    if (isArray(vars)) {
        const args = mapResult(parseVarDecl, vars);
        const body = mapResult(parseL5CExp, rest[0] === ":" ? rest.slice(2) : rest);
        const returnTE = rest[0] === ":" ? parseTExp(rest[1]) : makeOk(makeFreshTVar());
        return safe3((args: VarDecl[], body: CExp[], returnTE: TExp) => makeOk(makeProcExp(args, body, returnTE)))
                (args, body, returnTE);
    } else {
        return makeFailure(`Invalid args ${JSON.stringify(vars)}`)
    }
}

const isGoodBindings = (bindings: Sexp): bindings is [Sexp, Sexp][] =>
    isArray(bindings) && allT(isArray, bindings);

const parseLetExp = (bindings: Sexp, body: Sexp[]): Result<LetExp> =>
    isEmpty(body) ? makeFailure('Body of "let" cannot be empty') :
    ! isGoodBindings(bindings) ? makeFailure(`Invalid bindings: ${JSON.stringify(bindings)}`) :
    safe2((bdgs: Binding[], body: CExp[]) => makeOk(makeLetExp(bdgs, body)))
        (parseBindings(bindings), mapResult(parseL5CExp, body));

const isConcreteVarDecl = (sexp: Sexp): boolean =>
    isIdentifier(sexp) ||
    (isArray(sexp) && sexp.length > 2 && isIdentifier(sexp[0]) && (sexp[1] === ':'));

export const parseVarDecl = (sexp: Sexp): Result<VarDecl> => {
    if (isString(sexp)) {
        return makeOk(makeVarDecl(sexp, makeFreshTVar()));
    } else if (isArray(sexp)) {
        const v = sexp[0];
        if (isString(v)) {
            return bind(parseTExp(sexp[2]), (te: TExp) => makeOk(makeVarDecl(v, te)));
        } else {
            return makeFailure(`Invalid var ${sexp[0]}`);
        }
    } else {
        return makeFailure("Var cannot be a SexpString");
    }
}

const parseBindings = (bindings: [Sexp, Sexp][]): Result<Binding[]> =>
    safe2((vds: VarDecl[], vals: CExp[]) => makeOk(zipWith(makeBinding, vds, vals)))
        (mapResult(parseVarDecl, map(b => b[0], bindings)), 
         mapResult(parseL5CExp, map(b => b[1], bindings)));

const parseLetrecExp = (bindings: Sexp, body: Sexp[]): Result<LetrecExp> =>
    isEmpty(body) ? makeFailure('Body of "letrec" cannot be empty') :
    ! isGoodBindings(bindings) ? makeFailure(`Invalid bindings: ${JSON.stringify(bindings)}`) :
    safe2((bdgs: Binding[], body: CExp[]) => makeOk(makeLetrecExp(bdgs, body)))
        (parseBindings(bindings), mapResult(parseL5CExp, body));

const parseSetExp = (params: Sexp[]): Result<SetExp> =>
    isEmpty(params) ? makeFailure("set! missing 2 arguments") :
    isEmpty(rest(params)) ? makeFailure("set! missing 1 argument") :
    ! isEmpty(rest(rest(params))) ? makeFailure("set! has too many arguments") :
    parseGoodSetExp(first(params), second(params));
    
const parseGoodSetExp = (variable: Sexp, val: Sexp): Result<SetExp> =>
    ! isIdentifier(variable) ? makeFailure("First arg of set! must be an identifier") :
    bind(parseL5CExp(val), (val: CExp) => makeOk(makeSetExp(makeVarRef(variable), val)));


// L51
// classexp has the shape: (class [: typeName]? (varDecl...) (binding...))
const parseClassExp = (params: Sexp[]): Result<ClassExp> => 
    params.length == 2 ? parseGoodClassExp(makeFreshTVar().var, params[0], params[1]) :
    (params.length != 4) || (params[0] != ':') ? makeFailure(`class must have shape (class [: <type>]? <fields> <methods>) - got ${params.length} params instead`) :
    parseGoodClassExp(params[1], params[2], params[3]);

const parseGoodClassExp = (typeName: Sexp, varDecls: Sexp, bindings: Sexp): Result<ClassExp> =>
    makeFailure("TODO parseGoodClassExp");

// sexps has the shape (quote <sexp>)
export const parseLitExp = (param: Sexp): Result<LitExp> =>
    bind(parseSExp(param), (sexp: SExpValue) => makeOk(makeLitExp(sexp)));

export const isDottedPair = (sexps: Sexp[]): boolean =>
    sexps.length === 3 && 
    sexps[1] === "."

export const makeDottedPair = (sexps : Sexp[]): Result<SExpValue> =>
    safe2((val1: SExpValue, val2: SExpValue) => makeOk(makeCompoundSExp(val1, val2)))
        (parseSExp(sexps[0]), parseSExp(sexps[2]));

// sexp is the output of p (sexp parser)
export const parseSExp = (sexp: Sexp): Result<SExpValue> =>
    sexp === "#t" ? makeOk(true) :
    sexp === "#f" ? makeOk(false) :
    isString(sexp) && isNumericString(sexp) ? makeOk(+sexp) :
    isSexpString(sexp) ? makeOk(sexp.toString()) :
    isString(sexp) ? makeOk(makeSymbolSExp(sexp)) :
    sexp.length === 0 ? makeOk(makeEmptySExp()) :
    isArray(sexp) && isDottedPair(sexp) ? makeDottedPair(sexp) :
    isArray(sexp) ? (
        // fail on (x . y z)
        sexp[0] === '.' ? makeFailure("Bad dotted sexp: " + sexp) : 
        safe2((val1: SExpValue, val2: SExpValue) => makeOk(makeCompoundSExp(val1, val2)))
            (parseSExp(first(sexp)), parseSExp(rest(sexp)))) :
    sexp;

// ==========================================================================
// Unparse: Map an AST to a concrete syntax string.

export const unparse = (e: Parsed, unparseWithTVars?: boolean): Result<string> =>
    // NumExp | StrExp | BoolExp | PrimOp | VarRef
    isNumExp(e) ? makeOk(`${e.val}`) :
    isStrExp(e) ? makeOk(`"${e.val}"`) :
    isBoolExp(e) ? makeOk(e.val ? "#t" : "#f") :
    isPrimOp(e) ? makeOk(e.op) :
    isVarRef(e) ? makeOk(e.var) :
    // AppExp | IfExp | ProcExp | LetExp | LitExp | LetrecExp | SetExp
    isAppExp(e) ? safe2((rator: string, rands: string[]) => isEmpty(rands) ? makeOk(`(${rator})`) : makeOk(`(${rator} ${join(" ", rands)})`))
                    (unparse(e.rator, unparseWithTVars), mapResult(r => unparse(r, unparseWithTVars), e.rands)) :
    isIfExp(e) ? safe3((test: string, then: string, alt: string) => makeOk(`(if ${test} ${then} ${alt})`))
                    (unparse(e.test, unparseWithTVars), unparse(e.then, unparseWithTVars), unparse(e.alt, unparseWithTVars)) :
    isLetExp(e) ? unparseLetExp(e, unparseWithTVars) :
    isLetrecExp(e) ? unparseLetrecExp(e, unparseWithTVars) :
    isProcExp(e) ? unparseProcExp(e, unparseWithTVars) :
    isLitExp(e) ? makeOk(unparseLitExp(e)) :
    isSetExp(e) ? unparseSetExp(e, unparseWithTVars) :
    isClassExp(e) ? unparseClassExp(e, unparseWithTVars) :
    // DefineExp | Program
    isDefineExp(e) ? safe2((vd: string, val: string) => makeOk(`(define ${vd} ${val})`))
                        (unparseVarDecl(e.var, unparseWithTVars), unparse(e.val, unparseWithTVars)) :
    isProgram(e) ? bind(unparseLExps(e.exps, unparseWithTVars), (exps: string) => makeOk(`(L5 ${exps})`)) :
    e;

const unparseReturn = (te: TExp, unparseWithTVars?: boolean): Result<string> =>
    isTVar(te) && !unparseWithTVars ? makeOk("") :
    bind(unparseTExp(te), (te: string) => makeOk(` : ${te}`));

const unparseBindings = (bindings: Binding[], unparseWithTVars?: boolean): Result<string> =>
    bind(mapResult(bdg => safe2((vd: string, val: string) => makeOk(`(${vd} ${val})`))(unparseVarDecl(bdg.var, unparseWithTVars), unparse(bdg.val, unparseWithTVars)), bindings),
         (bdgs: string[]) => makeOk(join(" ", bdgs)));

// For debugging - unparse with TVar else only if variable is annotated by programmer.
const unparseVarDecl = (vd: VarDecl, unparseWithTVars?: boolean): Result<string> =>
    isTVar(vd.texp) && !unparseWithTVars ? makeOk(vd.var) :
    bind(unparseTExp(vd.texp), te => makeOk(`(${vd.var} : ${te})`));

// Add a quote for symbols, empty and compound sexp - strings and numbers are not quoted.
const unparseLitExp = (le: LitExp): string =>
    isEmptySExp(le.val) ? `'()` :
    isSymbolSExp(le.val) ? `'${valueToString(le.val)}` :
    isCompoundSExp(le.val) ? `'${valueToString(le.val)}` :
    `${le.val}`;

const unparseLExps = (les: Exp[], unparseWithTVars?: boolean): Result<string> =>
    bind(mapResult(le => unparse(le, unparseWithTVars), les), (les: string[]) => makeOk(join(" ", les)));

const unparseProcExp = (pe: ProcExp, unparseWithTVars?: boolean): Result<string> =>
    safe3((vds: string[], ret: string, body: string) => makeOk(`(lambda (${join(" ", vds)})${ret} ${body})`))
        (mapResult(arg => unparseVarDecl(arg, unparseWithTVars), pe.args),
         unparseReturn(pe.returnTE, unparseWithTVars), 
         unparseLExps(pe.body, unparseWithTVars));

const unparseLetExp = (le: LetExp, unparseWithTVars?: boolean) : Result<string> => 
    safe2((bdgs: string, body: string) => makeOk(`(let (${bdgs}) ${body})`))
        (unparseBindings(le.bindings, unparseWithTVars), unparseLExps(le.body, unparseWithTVars));

const unparseLetrecExp = (le: LetrecExp, unparseWithTVars?: boolean): Result<string> =>
    safe2((bdgs: string, body: string) => makeOk(`(letrec (${bdgs}) ${body})`))
        (unparseBindings(le.bindings, unparseWithTVars), unparseLExps(le.body, unparseWithTVars));

const unparseSetExp = (se: SetExp, unparseWithTVars?: boolean): Result<string> =>
    bind(unparse(se.val, unparseWithTVars), (val: string) => makeOk(`(set! ${se.var.var} ${val})`));

const unparseFields = (fields: VarDecl[], unparseWithTVars?: boolean): Result<string> =>
    bind(mapResult(field => unparseVarDecl(field, unparseWithTVars), fields), 
         (fields: string[]) => makeOk(join(" ", fields)));

const unparseClassExp = (ce: ClassExp, unparseWithTVars?: boolean): Result<string> => 
    bind(unparseFields(ce.fields), 
         (fields: string) => 
           bind(unparseBindings(ce.methods, unparseWithTVars), 
                (methods: string) => 
                  ce.typeName.var.startsWith("T") && !unparseWithTVars ? makeOk(`(class (${fields}) (${methods}))`) :
                  makeOk(`(class : ${ce.typeName.var} (${fields}) (${methods}))`)));

// L51: Collect named types in AST
// Collect class expressions in parsed AST so that they can be passed to the type inference module

export const parsedToClassExps = (p: Parsed): ClassExp[] => 
    // TODO parsedToClassExps
    [];

// L51 
export const classExpToClassTExp = (ce: ClassExp): ClassTExp => 
    makeClassTExp(ce.typeName.var, map((binding: Binding) => [binding.var.var, binding.var.texp], ce.methods));
    
