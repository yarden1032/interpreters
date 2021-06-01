import { expect } from 'chai';
import { Exp, Program, unparse, parseL5, parseL5Exp } from '../src/L51-ast';
import { makeOk, bind, isFailure } from '../shared/result';
import { parse as p } from "../shared/parser";

describe('Q5 Tests', () => {
     it('test parse/unparse pair class', () => {
          expect(
            bind(
              bind(p(`(class (a b) ((first (lambda () a)) (second (lambda () b)) (sum (lambda () (+ a b)))))`), parseL5Exp), 
                   (x: Exp) => unparse(x))).to.deep.equal(makeOk(`(class (a b) ((first (lambda () a)) (second (lambda () b)) (sum (lambda () (+ a b)))))`));
         });

     it('test parse wrong class1', () => {
          expect(bind(p(`(class ((first (lambda () a)) (second (lambda () b)) (sum (lambda () (+ a b)))))`),parseL5Exp)).is.satisfy(isFailure);
     });

     it('test parse wrong class2', () => {
          expect(bind(p(`(class (a b) (sum (lambda () (+ a b)))))`),parseL5Exp)).is.satisfy(isFailure);
     });

     it('test parse/unparse pair program', () => {
          expect(
            bind(
              parseL5(`(L5 (define pair (class (a b) ((first (lambda () a)) (second (lambda () b)) (sum (lambda () (+ a b)))))) (let ((p12 (pair 1 2)) (p34 (pair 3 4))) (if (> (p12 'first) (p34 'second)) #t #f)))`), 
              (x: Program) => unparse(x))).to.deep.equal(
               makeOk(`(L5 (define pair (class (a b) ((first (lambda () a)) (second (lambda () b)) (sum (lambda () (+ a b)))))) (let ((p12 (pair 1 2)) (p34 (pair 3 4))) (if (> (p12 'first) (p34 'second)) #t #f)))`));
         });

     it('test parse/unparse bool program', () => {
     expect(
       bind(
         parseL5(`(L5 (define not (class (a) ((var (lambda () a)) (op (lambda () (not a)))))) (let ((p#f (not #f)) (p#t (not #t))) (if (eq? (p#f 'var) (p#t 'var)) #t #f)))`), 
         (x: Program) => unparse(x))).to.deep.equal(
          makeOk(`(L5 (define not (class (a) ((var (lambda () a)) (op (lambda () (not a)))))) (let ((p#f (not #f)) (p#t (not #t))) (if (eq? (p#f 'var) (p#t 'var)) #t #f)))`));
     });

     it('test parse/unparse program with classes and types', () => {
          expect(
            bind(
              parseL5(`(L5 (define pair (class : pair ((a : number) (b : number)) ((first (lambda () : number a)) (second (lambda () : number b)) (sum (lambda () : number (+ a b)))))) (let ((p12 (pair 1 2)) (p34 (pair 3 4))) (if (> ((p12 'first)) ((p34 'second))) #t #f)))`), 
              (x: Program) => unparse(x))).to.deep.equal(
               makeOk(`(L5 (define pair (class : pair ((a : number) (b : number)) ((first (lambda () : number a)) (second (lambda () : number b)) (sum (lambda () : number (+ a b)))))) (let ((p12 (pair 1 2)) (p34 (pair 3 4))) (if (> ((p12 'first)) ((p34 'second))) #t #f)))`));
         });


});

