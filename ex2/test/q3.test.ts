import { expect } from 'chai';
import { unparseL31, parseL31, parseL31Exp } from '../src/L31-ast';
import { L31ToL3 } from '../src/q3';
import { makeOk, bind, isFailure } from '../shared/result';
import { parse as p } from "../shared/parser";

describe('Q3 Tests', () => {
     it('test parse/unparse class', () => {
          expect(bind(bind(p(`(class (a b) ((first (lambda () a)) (second (lambda () b)) (sum (lambda () (+ a b)))))`),parseL31Exp), x=>makeOk(unparseL31(x)))).to.deep.equal(makeOk(`(class (a b) ((first (lambda () a)) (second (lambda () b)) (sum (lambda () (+ a b)))))`));
         });

     it('test parse wrong class', () => {
          expect(bind(p(`(class ((first (lambda () a)) (second (lambda () b)) (sum (lambda () (+ a b)))))`),parseL31Exp)).is.satisfy(isFailure);
     });


     it('test parse/unparse program', () => {
          expect(bind(parseL31(`(L31 (define pair (class (a b) ((first (lambda () a)) (second (lambda () b)) (sum (lambda () (+ a b)))))) (let ((p12 (pair 1 2)) (p34 (pair 3 4))) (if (> (p12 'first) (p34 'second)) #t #f)))`), x=>makeOk(unparseL31(x)))).to.deep.equal(makeOk(`(L31 (define pair (class (a b) ((first (lambda () a)) (second (lambda () b)) (sum (lambda () (+ a b)))))) (let ((p12 (pair 1 2)) (p34 (pair 3 4))) (if (> (p12 'first) (p34 'second)) #t #f)))`));
         });
     
     
     it('trnasform class-exp in to proc-exp', () => {
          expect(bind(bind(bind(p(`(class (a b) ((first (lambda () a)) (second (lambda () b)) (sum (lambda () (+ a b)))))`), parseL31Exp), L31ToL3),  x=>makeOk(unparseL31(x)))).to.deep.equal(makeOk(`(lambda (a b) (lambda (msg) (if (eq? msg 'first) ((lambda () a) ) (if (eq? msg 'second) ((lambda () b) ) (if (eq? msg 'sum) ((lambda () (+ a b)) ) #f)))))`));
     });

     it('trnasform class-exp program in to proc-exp', () => {
          expect(bind(bind(parseL31(`(L31 (define pair (class (a b) ((first (lambda () a)) (second (lambda () b)) (sum (lambda () (+ a b)))))) (let ((p12 (pair 1 2)) (p34 (pair 3 4))) (if (> (p12 'first) (p34 'second)) #t #f)))`), L31ToL3),  x=>makeOk(unparseL31(x)))).to.deep.equal(makeOk(`(L31 (define pair (lambda (a b) (lambda (msg) (if (eq? msg 'first) ((lambda () a) ) (if (eq? msg 'second) ((lambda () b) ) (if (eq? msg 'sum) ((lambda () (+ a b)) ) #f)))))) (let ((p12 (pair 1 2)) (p34 (pair 3 4))) (if (> (p12 'first) (p34 'second)) #t #f)))`));
     });
     
});