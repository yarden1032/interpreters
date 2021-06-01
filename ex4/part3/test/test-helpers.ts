import { bind, map } from 'ramda';
import { parse, parseL5Exp, Exp, Parsed } from '../src/L51-ast';
import { unparseTExp, TExp, parseTE, makeTVar, equivalentTEs } from '../src/TExp51';
import { makeSub, Sub} from '../imp/L5-substitution-adt';
import { Result, bind as bindResult, mapResult, zipWithResult, makeOk, safe2, makeFailure } from '../shared/result';
import { typeofExp, makeTEnvFromClasses } from '../src/L51-typeinference';

// Sub constructor from concrete syntax
export const sub = (vars: string[], tes: string[]): Result<Sub> =>
    bindResult(mapResult(parseTE, tes),
               (texps: TExp[]) => makeSub(map(makeTVar, vars), texps));

export const subToStr = (sub: Sub): Result<string> =>
    bindResult(zipWithResult((v, t) => bindResult(unparseTExp(t), up => makeOk(`${v.var}:${up}`)), sub.vars, sub.tes),
               (vts: string[]) => makeOk(vts.sort().join(", ")));

export const verifyTeOfExprWithInference = (exp: string, texp: string): Result<boolean> => {
    const e : Result<Parsed> = parse(exp);
    const expectedType = parseTE(texp);
    const computedType = bindResult(e, (exp: Parsed) => typeofExp(exp, makeTEnvFromClasses(exp)));
    return safe2((ct: TExp, et: TExp) => {
            const test = equivalentTEs(ct, et);
            return test ? makeOk(true) : 
                bindResult(computedType, 
                    ct => bindResult(unparseTExp(ct), 
                            cts => makeFailure<boolean>(`Expected ${texp} - Got ${cts}`)));
    })(computedType, expectedType);
};
