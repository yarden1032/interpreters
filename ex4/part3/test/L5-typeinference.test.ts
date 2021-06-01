import { expect } from 'chai';
import { inferTypeOf, typeofExp } from "../src/L51-typeinference";
import { parseL5Exp, Exp } from "../src/L51-ast";
import { makeExtendTEnv, makeEmptyTEnv } from "../imp/TEnv";
import { makeNumTExp } from "../src/TExp51";
import { verifyTeOfExprWithInference } from "./test-helpers";
import { makeOk, bind, isFailure } from '../shared/result';
import { parse as p } from "../shared/parser";

describe('L5 Type Inference', () => {
    describe('inferTypeOf', () => {
        it('infers the type of atoms', () => {
            expect(inferTypeOf("5")).to.deep.equal(makeOk("number"));
            expect(inferTypeOf("#t")).to.deep.equal(makeOk("boolean"));
        });

        it('infers the type of primitive procedures', () => {
            expect(inferTypeOf("+")).to.deep.equal(makeOk("(number * number -> number)"));
            expect(inferTypeOf("-")).to.deep.equal(makeOk("(number * number -> number)"));
            expect(inferTypeOf("*")).to.deep.equal(makeOk("(number * number -> number)"));
            expect(inferTypeOf("/")).to.deep.equal(makeOk("(number * number -> number)"));
            expect(inferTypeOf("=")).to.deep.equal(makeOk("(number * number -> boolean)"));
            expect(inferTypeOf("<")).to.deep.equal(makeOk("(number * number -> boolean)"));
            expect(inferTypeOf(">")).to.deep.equal(makeOk("(number * number -> boolean)"));
            expect(inferTypeOf("not")).to.deep.equal(makeOk("(boolean -> boolean)"));
        });

        it("infers the type of primitive op applications", () => {
            expect(inferTypeOf("(+ 1 2)")).to.deep.equal(makeOk("number"));
            expect(inferTypeOf("(- 1 2)")).to.deep.equal(makeOk("number"));
            expect(inferTypeOf("(* 1 2)")).to.deep.equal(makeOk("number"));
            expect(inferTypeOf("(/ 1 2)")).to.deep.equal(makeOk("number"));

            expect(inferTypeOf("(= 1 2)")).to.deep.equal(makeOk("boolean"));
            expect(inferTypeOf("(< 1 2)")).to.deep.equal(makeOk("boolean"));
            expect(inferTypeOf("(> 1 2)")).to.deep.equal(makeOk("boolean"));

            expect(inferTypeOf("(not (< 1 2))")).to.deep.equal(makeOk("boolean"));
        });

        it('infers the type of generic primitive op application', () => {
            expect(inferTypeOf("(eq? 1 2)")).to.deep.equal(makeOk("boolean"));
            expect(inferTypeOf('(string=? "a" "b")')).to.deep.equal(makeOk("boolean"));
            expect(inferTypeOf('(number? 1)')).to.deep.equal(makeOk("boolean"));
            expect(inferTypeOf('(boolean? "a")')).to.deep.equal(makeOk("boolean"));
            expect(inferTypeOf('(string? "a")')).to.deep.equal(makeOk("boolean"));
            expect(inferTypeOf('(symbol? "a")')).to.deep.equal(makeOk("boolean"));
            expect(inferTypeOf('(list? "a")')).to.deep.equal(makeOk("boolean"));
            expect(inferTypeOf('(pair? "a")')).to.deep.equal(makeOk("boolean"));
        });

        it('infers the type of a VarRef in a given TEnv', () => {
            expect(bind(bind(p("x"), parseL5Exp), (exp: Exp) => typeofExp(exp, makeExtendTEnv(["x"], [makeNumTExp()], makeEmptyTEnv())))).to.deep.equal(makeOk(makeNumTExp()));
        });

        it('infers the type of "if" expressions', () => {
            expect(inferTypeOf("(if (> 1 2) 1 2)")).to.deep.equal(makeOk("number"));
            expect(inferTypeOf("(if (= 1 2) #t #f)")).to.deep.equal(makeOk("boolean"));
        });

        it('infers the type of procedures', () => {
            expect(inferTypeOf("(lambda ((x : number)) : number x)")).to.deep.equal(makeOk("(number -> number)"));
            expect(inferTypeOf("(lambda ((x : number)) : boolean (> x 1))")).to.deep.equal(makeOk("(number -> boolean)"));
            expect(inferTypeOf("(lambda((x : number)) : (number -> number) (lambda((y : number)) : number (* y x)))")).to.deep.equal(makeOk("(number -> (number -> number))"));
            expect(inferTypeOf("(lambda((f : (number -> number))) : number (f 2))")).to.deep.equal(makeOk("((number -> number) -> number)"));
            expect(inferTypeOf("(lambda((x : number)) : number (let (((y : number) x)) (+ x y)))")).to.deep.equal(makeOk("(number -> number)"));
        });

        it('infers the type of "let" expressions', () => {
            expect(inferTypeOf("(let (((x : number) 1)) (* x 2))")).to.deep.equal(makeOk("number"));
            expect(inferTypeOf(`(let (((x : number) 1)
                                      ((y : number) 2))
                                  (lambda((a : number)) : number (+ (* x a) y)))`)).to.deep.equal(makeOk("(number -> number)"));
        });

        it('infers the type of "letrec" expressions', () => {
            expect(inferTypeOf("(letrec (((p1 : (number -> number)) (lambda((x : number)) : number (* x x)))) p1)")).to.deep.equal(makeOk("(number -> number)"));
            expect(inferTypeOf("(letrec (((p1 : (number -> number)) (lambda((x : number)) : number (* x x)))) (p1 2))")).to.deep.equal(makeOk("number"));
            expect(inferTypeOf(`(letrec (((odd? : (number -> boolean)) (lambda((n : number)) : boolean (if (= n 0) #f (even? (- n 1)))))
                                         ((even? : (number -> boolean)) (lambda((n : number)) : boolean (if (= n 0) #t (odd? (- n 1))))))
                                  (odd? 12))`)).to.deep.equal(makeOk("boolean"));
        });

        it('infers the type of "define" expressions as "void"', () => {
            expect(inferTypeOf("(define (foo : number) 5)")).to.deep.equal(makeOk("void"));
            expect(inferTypeOf("(define (foo : (number * number -> number)) (lambda((x : number) (y : number)) : number (+ x y)))")).to.deep.equal(makeOk("void"));
            expect(inferTypeOf("(define (x : (Empty -> number)) (lambda () : number 1))")).to.deep.equal(makeOk("void"));
            expect(inferTypeOf(`(define (x : (T1 -> (T1 -> number))) (lambda ((x : T1)) : (T1 -> number) (lambda((y : T1)) : number 5)))`)).to.deep.equal(makeOk("void"));
        });

        it('infers the type of polymorphic functions', () => {
            expect(inferTypeOf("(lambda((x : T1)) : T1 x)")).to.deep.equal(makeOk("(T1 -> T1)"));
            expect(inferTypeOf(`(let (((x : number) 1))
                                  (lambda((y : T) (z : T)) : T
                                    (if (> x 2) y z)))`)).to.deep.equal(makeOk("(T * T -> T)"));
        });

        it('infers the type of parameter-less procedures', () => {
            expect(inferTypeOf("(lambda () : number 1)")).to.deep.equal(makeOk("(Empty -> number)"));
        });
    });

    describe('typeOfExp', () => {
        it('infers return type', () => {
            expect(verifyTeOfExprWithInference("(lambda ((x : number)) x)", "(number -> number)")).to.deep.equal(makeOk(true));
        });

        it('infers parameter type', () => {
            expect(verifyTeOfExprWithInference('(lambda (x) : number x)', "(number -> number)")).to.deep.equal(makeOk(true));
        });

        it('infers both parameter and return types', () => {
            expect(verifyTeOfExprWithInference('(lambda (x) (> x 1))', "(number -> boolean)")).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference('(lambda (x) (lambda (y) (* x y)))', "(number -> (number -> number))")).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference('(let ((x 1)) (* x 2))', "number")).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference(`
                (let ((x 1)
                      (y 2))
                  (lambda (a) (+ (* x a) y)))`, "(number -> number)")).to.deep.equal(makeOk(true));

            expect(verifyTeOfExprWithInference(`
                (lambda (x)
                  (let ((y x)) (+ x y)))`, "(number -> number)")).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference('(letrec ((p1 (lambda (x) (* x x)))) p1)', '(number -> number)')).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference('(letrec ((p1 (lambda (x) (* x x)))) (p1 2))', 'number')).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference('(lambda () 1)', "(Empty -> number)")).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference(`
                (letrec ((odd? (lambda (n) (if (= n 0) #f (even? (- n 1)))))
                         (even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))))
                  (odd? 12))`, 'boolean')).to.deep.equal(makeOk(true));
        });

        it('infers unannotated polymorphic functions', () => {
            expect(verifyTeOfExprWithInference(`(lambda (x) x)`, '(T1 -> T1)')).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference(`(lambda (f) (f 2))`, '((number -> T) -> T)')).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference(`(let ((x 1)) (lambda (y z) (if (> x 2) y z)))`, '(T * T -> T)')).to.deep.equal(makeOk(true));
        });

        it('returns an error when the generic type should take two concrete types', () => {
            expect(verifyTeOfExprWithInference(`
                (letrec ((id (lambda (x) x)))
                  (if (id #t) (id 1) (id 2)))`, "Error")).to.satisfy(isFailure);
        });

        it('infers the type of let expressions with lambda', () => {
            expect(verifyTeOfExprWithInference("(let ((x 1)) (+ x x))", "number")).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference("(let ((x1 1) (y1 (lambda (z1) (+ 1 z1)))) y1)", "(number -> number)")).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference("(let ((x1 1) (y1 (lambda (z1) (+ 1 z1)))) (lambda (u1) y1))", "(T1 -> (number -> number))")).to.deep.equal(makeOk(true));
        });
    
        it('infers the type of a program with define and recursion', () => {
            expect(verifyTeOfExprWithInference(`(L5 (define n 1) (if #t n (- n 1)))`, 'number')).to.deep.equal(makeOk(true));
            expect(verifyTeOfExprWithInference(`(L5 (define fact (lambda (n) (if (= n 1) 1 (* n (fact (- n 1)))))) (fact 2))`, 'number')).to.deep.equal(makeOk(true));
        });

        it('infers the type of set! expressions', () => {
            const program1 = 
            `(L5 (define x 1)
                 (set! x 2))`;
            expect(verifyTeOfExprWithInference(program1, `void`)).to.deep.equal(makeOk(true));
            
            const program2 = 
            `(L5 (define x 1)
                 (set! x #t))`;
            expect(verifyTeOfExprWithInference(program2, `Error`)).to.satisfy(isFailure);
        });

        it('infers the type of quoted literal expressions', () => {
            const program1 = 
            `(L5 (define x 'a)
                 x)`;
            expect(verifyTeOfExprWithInference(program1, `symbol`)).to.deep.equal(makeOk(true));
            
            const program2 = 
            `(L5 (define f (lambda ((x : symbol)) x))
                 f)`;
            expect(verifyTeOfExprWithInference(program2, `(symbol -> symbol)`)).to.deep.equal(makeOk(true));

            const program3 = 
            `(L5 (define f (lambda ((x : cons)) '(a b)))
                 f)`;
            expect(verifyTeOfExprWithInference(program3, `(cons -> cons)`)).to.deep.equal(makeOk(true));

        });

        it('infers the type of primitives car, cdr, cons', () => {
            const program1 = 
            `(L5 (define x 'a)
                 (cons x x))`;
            expect(verifyTeOfExprWithInference(program1, `cons`)).to.deep.equal(makeOk(true));

            const program2 = 
            `(L5 (define x '(a b))
                 (car x))`;
            expect(verifyTeOfExprWithInference(program2, `T`)).to.deep.equal(makeOk(true));

            const program3 = 
            `(L5 (define x '(a b))
                 (cdr x))`;
            expect(verifyTeOfExprWithInference(program3, `T`)).to.deep.equal(makeOk(true));

            const program4 = 
            `(L5 (define x 'a)
                 (car x))`;
            expect(verifyTeOfExprWithInference(program4, `T`)).to.satisfy(isFailure);

            const program5 = 
            `(L5 (define x 'a)
                 (cdr x))`;
            expect(verifyTeOfExprWithInference(program5, `T`)).to.satisfy(isFailure);
        });

        it('infers the type of class', () => {
            expect(verifyTeOfExprWithInference("(class : c1 ((field1 : number)) ((get (lambda () : number field1))))", 
                                               "(number -> (class cell (get : (Empty -> number))))")).to.deep.equal(makeOk(true));
        });

        it('infers the type of a class constructor', () => {
            const program1 = 
            `(L5 (define pair (class : pair 
                                ((f : T) 
                                 (r : T))
                                ((first (lambda () f)) 
                                 (rest (lambda () r)))))
                 (pair 1 2)
                 pair)`;
            expect(verifyTeOfExprWithInference(program1, `(number * number -> (class pair (first : (Empty -> number)) (rest : (Empty -> number))))`)).to.deep.equal(makeOk(true));

            const program2 = 
            `(L5 (define pair (class : pair 
                                ((f : T) 
                                 (r : T))
                                ((first (lambda () f)) 
                                 (rest (lambda () r))
                                 (scale (lambda (k) (pair (* k f) (* k r)))))))
                 (pair 1 2)
                 pair)`;
            expect(verifyTeOfExprWithInference(program2, `(number * number -> (class pair (first : (Empty -> number)) (rest : (Empty -> number)) (scale : (number -> pair))))`)).to.deep.equal(makeOk(true));
        });

        it('infers the type of a class with typed class define', () => {
            const program1 = 
            `(L5 (define cell (class : cell
                                ((c : T))
                                ((get (lambda () c)))))
                 (define (c : cell) (cell 'a))
                 c)`;
            const expected1 = 
            `(class cell (get : (Empty -> symbol)))`
            expect(verifyTeOfExprWithInference(program1, expected1)).to.deep.equal(makeOk(true));
        });

        it('infers the type of a class method invocation', () => {
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
            expect(verifyTeOfExprWithInference(program3, expected3)).to.deep.equal(makeOk(true));

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
            expect(verifyTeOfExprWithInference(program4, expected4)).to.deep.equal(makeOk(true));

        });

        it('returns an error when a message is passed to a class that is not in its interface', () => {
            const program5 = 
            `(L5 (define pair (class : pair 
                                ((f : T) 
                                 (r : T))
                                ((first (lambda () f)) 
                                 (rest (lambda () r))
                                 (scale (lambda (k) (pair (* k f) (* k r)))))))
                 (define p12 (pair 1 2))
                 ((p12 'scale1) 3))`;
            expect(verifyTeOfExprWithInference(program5, "Error")).to.satisfy(isFailure);

            const program6 = 
            `(L5 (define pair (class : pair 
                                ((f : T) 
                                 (r : T))
                                ((first (lambda () f)) 
                                 (rest (lambda () r))
                                 (scale (lambda (k) (pair (* k f) (* k r)))))))
                 (define p12 (pair 1 2))
                 (p12 'scale 3))`;
            expect(verifyTeOfExprWithInference(program6, "Error")).to.satisfy(isFailure);

            const program7 = 
            `(L5 (define pair (class : pair 
                                ((f : T) 
                                 (r : T))
                                ((first (lambda () f)) 
                                 (rest (lambda () r))
                                 (scale (lambda (k) (pair (* k f) (* k r)))))))
                 (define p12 (pair 1 2))
                 ((p12 'scale))`;
            expect(verifyTeOfExprWithInference(program7, "Error")).to.satisfy(isFailure);
          
        });

        it('infers the type of functions that receive class as parameters or return methods', () => {
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
            expect(verifyTeOfExprWithInference(program5, expected5)).to.deep.equal(makeOk(true));

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
            expect(verifyTeOfExprWithInference(program6, expected6)).to.deep.equal(makeOk(true));

            const program7 = 
            `(L5 (define pair (class : pair 
                                ((f : T) 
                                 (r : T))
                                ((first (lambda () f)) 
                                 (rest (lambda () r))
                                 (scale (lambda (k) (pair (* k f) (* k r)))))))
                 (define p12 (pair 1 2))
                 (define f (lambda ((p : (class pair (first : (Empty -> T)) (rest : (Empty -> T)) (scale : (number -> pair))))) ((p 'first))))
                 f)`;
            const expected7 = '((class pair (first : (Empty -> T)) (rest : (Empty -> T)) (scale : (number -> pair))) -> T)';
            expect(verifyTeOfExprWithInference(program7, expected7)).to.deep.equal(makeOk(true));

            const program8 = 
            `(L5 (define cell (class : cell ((c : T)) ((get (lambda () c)))))
                 (define f (lambda ((c : (class cell (get : (Empty -> T))))) ((c 'get))))
                 f)`;
            const expected8 = '((class cell (get : (Empty -> T))) -> T)';
            expect(verifyTeOfExprWithInference(program8, expected8)).to.deep.equal(makeOk(true));

            const program9 = 
            `(L5 (define cell (class : cell ((c : T)) ((get (lambda () c)))))
                 (define f (lambda ((c : (class cell (get : (Empty -> T))))) (c 'get)))
                 f)`;
            const expected9 = '((class cell (get : (Empty -> T))) -> (Empty -> T))';
            expect(verifyTeOfExprWithInference(program9, expected9)).to.deep.equal(makeOk(true));
        });
    });
});
