import * as A from './L51-ast';
import * as T from './TExp51';
import * as I from './L51-typeinference';
import * as Res from '../shared/result';
import {parse as parseSexp} from '../shared/parser';

const program0 = 
`(L5 (define cell (class : Tcell
                    ((c : T))
                    ((get (lambda () c)))))
     (define (c : Tcell) (cell 'a))
     c)`;
const expected0 = 
`(class Tcell (get : (Empty -> symbol)))`

const program1 = `(L5 (define pair (class : pair 
                                    ((f : T) 
                                     (r : T))
                                    ((first (lambda () f)) 
                                     (rest (lambda () r)))))
                     (pair 1 2)
                     pair)`
const expected1 = '(number * number -> (class pair (first : (Empty -> number)) (rest : (Empty -> number))))';


const program2 = 
`(L5 (define pair (class : pair 
                    ((f : T) 
                     (r : T))
                    ((first (lambda () f)) 
                     (rest (lambda () r))
                     (scale (lambda (k) (pair (* k f) (* k r)))))))
     (pair 1 2)
     pair)`;
const expected2 = '(number * number -> (class pair (first : (Empty -> number)) (rest : (Empty -> number)) (scale : (number -> pair))))';

const program3 = 
`(L5 (define pair (class : pair 
                    ((f : T) 
                     (r : T))
                    ((first (lambda () f)) 
                     (rest (lambda () r))
                     (scale (lambda (k) (pair (* k f) (* k r)))))))
     (define p12 (pair 1 2))
     ((p12 'scale) 3))`;
const expected3 = '(class pair (first : (Empty -> number)) (rest : (Empty -> number)) (scale : (number -> pair)))';
     
const program4 = 
`(L5 (define pair (class : pair 
                    ((f : T) 
                     (r : T))
                    ((first (lambda () f)) 
                     (rest (lambda () r))
                     (scale (lambda (k) (pair (* k f) (* k r)))))))
     (define p12 (pair 1 2))
     (define p36 ((p12 'scale) 3))
     ((p36 'rest)))`;
const expected4 = 'number';

const program5 = 
`(L5 (define pair (class : pair 
                    ((f : T) 
                     (r : T))
                    ((first (lambda () f)) 
                     (rest (lambda () r))
                     (scale (lambda (k) (pair (* k f) (* k r)))))))
     (define p12 (pair 1 2))
     (define f (lambda ((p : pair)) ((p 'first))))
     (f p12))`;
const expected5 = 'number';

const program6 = 
`(L5 (define pair (class : pair 
                    ((f : T) 
                     (r : T))
                    ((first (lambda () f)) 
                     (rest (lambda () r))
                     (scale (lambda (k) (pair (* k f) (* k r)))))))
     (define p12 (pair 1 2))
     (define f (lambda ((p : pair)) (p 'scale)))
     (f p12))`;
const expected6 = '(number -> (class pair (first : (Empty -> number)) (rest : (Empty -> number)) (scale : (number -> pair))))';

const program7 = 
`(L5 (define pair (class : pair 
                    ((f : T) 
                     (r : T))
                    ((first (lambda () f)) 
                     (rest (lambda () r))
                     (scale (lambda (k) (pair (* k f) (* k r)))))))
     (define p12 (pair 1 2))
     (define f (lambda ((p : (class pair (first : (Empty -> T)) (rest : (Empty -> T)) (scale : (number -> pair))))) (p 'scale)))
     f)`;
const expected7 = '((class pair (first : (Empty -> T)) (rest : (Empty -> T)) (scale : (number -> pair))) -> (number -> pair))';

const program8 = 
`(L5 (define cell (class : cell ((c : T)) ((get (lambda () c)))))
     (define f (lambda ((c : (class cell (get : (Empty -> T))))) ((c 'get))))
     f)`;
const expected8 = '((class cell (get : (Empty -> T))) -> T)';

const program9 = 
`(L5 (define cell (class : cell ((c : T)) ((get (lambda () c)))))
     (define f (lambda ((c : (class cell (get : (Empty -> T))))) (c 'get)))
     f)`;
const expected9 = '((class cell (get : (Empty -> T))) -> (Empty -> T))';


const doIt = (program: string, expected: string) => {
    console.log('----------------------');
    const p = A.parse(program);
    const t = Res.bind(p, p => {
        const tenv = I.makeTEnvFromClasses(p);
        // console.log(`tenv = `); 
        // console.log(JSON.stringify(tenv, null, 4));
        return I.typeofExp(p, tenv);
    });

    // JSON object is circular - cannot be serialized
    // Res.bind(t, t => Res.makeOk(console.log('Raw computed: ', JSON.stringify(t, null, 4))));

    Res.bind(t, t => Res.bind(T.unparseTExp(t), 
                            (t: string) => Res.makeOk(console.log('Computed: %j', t))));
    console.log('Expected: %j', expected);
    const texpected = Res.bind(parseSexp(expected), T.parseTExp);
    const teqv = Res.bind(texpected, texp => 
            Res.bind(t, t => T.equivalentTEs(t, texp) ? Res.makeOk("Good") : Res.makeFailure("equivalentTEs failed")));
    console.log(teqv);
}

doIt(program0, expected0);
doIt(program1, expected1);
doIt(program2, expected2);
doIt(program3, expected3);
doIt(program4, expected4);
doIt(program5, expected5);
doIt(program6, expected6);
doIt(program7, expected7);
doIt(program8, expected8);
doIt(program9, expected9);

const define4 = "(define (a : symbol-a) 'a)";
Res.bind(A.parse(define4), 
     (def4 : A.Parsed) => 
          A.isDefineExp(def4) ? Res.makeOk(console.log(`def4 = %j`, def4)) :
          Res.makeFailure(`Expected define`));

