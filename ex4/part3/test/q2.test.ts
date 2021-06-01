import fs from "fs";
import { expect } from 'chai';
import { evalProgram } from '../imp/L5-eval';
import { Value } from "../imp/L5-value";
import { Result, bind, makeOk } from "../shared/result";
import { parseL5 } from "../src/L51-ast";
import { listPrim} from "../imp/evalPrimitive";

const str:string = "(L5 (list 1 2 3))"

const evalP = (x: string): Result<Value> =>
    bind(parseL5(x), evalProgram)
// console.log(evalP(str));

const q2: string = fs.readFileSync('./src/q2.l5', { encoding: 'utf-8' });
describe('Q2 Tests', () => {    
    it('append tests', () => {
        expect(evalP(`(L5 ` + q2 + ` (append (list 1) (list 2 3)))`)).to.deep.equal(makeOk(listPrim([1,2,3])));
        expect(evalP(`(L5 ` + q2 + ` (append (list 1 2 3) (list 4 5)))`)).to.deep.equal(makeOk(listPrim([1,2,3,4,5])));
        expect(evalP(`(L5 ` + q2 + ` (append (list) (list 2 3)))`)).to.deep.equal(makeOk(listPrim([2,3])));
        expect(evalP(`(L5 ` + q2 + ` (append (list 3 4) (list )))`)).to.deep.equal(makeOk(listPrim([3,4])));        
        expect(evalP(`(L5 ` + q2 + ` (append (list) (list)))`)).to.deep.equal(makeOk(listPrim([])));

    });
    it(`reverse tests`,()=>
    {
        expect(evalP(`(L5 ` + q2 + ` (reverse  (list 1 2 3)))`)).to.deep.equal(makeOk(listPrim([3,2,1])));
        expect(evalP(`(L5 ` + q2 + ` (reverse  (list 1)))`)).to.deep.equal(makeOk(listPrim([1])));
        expect(evalP(`(L5 ` + q2 + ` (reverse  '()))`)).to.deep.equal(makeOk(listPrim([])));
        expect(evalP(`(L5 ` + q2 + ` (reverse  (list 3 6 2 0)))`)).to.deep.equal(makeOk(listPrim([0,2,6,3])));
        expect(evalP(`(L5 ` + q2 + ` (reverse  (list 10 10 10 10)))`)).to.deep.equal(makeOk(listPrim([10,10,10,10])));

    });
    it(`duplicate-items tests`,()=>
    {
        expect(evalP(`(L5`+q2+`(duplicate-items (list 1 2 3) (list 1 0) ))`)).to.deep.equal(makeOk(listPrim([1,3])));
        expect(evalP(`(L5`+q2+`(duplicate-items (list 1 2) (list 1 1 2 4) ))`)).to.deep.equal(makeOk(listPrim([1,2])));
        // expect(evalP(`(L5`+q2+`(duplicate-items (list 1 2 3 4) (list 0 1) ))`)).to.deep.equal(makeOk(listPrim([2,4])));
        expect(evalP(`(L5`+q2+`(duplicate-items (list 1 2 3 4) (list 2 3) ))`)).to.deep.equal(makeOk(listPrim([1,1,2,2,2,3,3,4,4,4])));
        expect(evalP(`(L5`+q2+`(duplicate-items (list 1 2 3 4) (list 0 0) ))`)).to.deep.equal(makeOk(listPrim([])));
        expect(evalP(`(L5`+q2+`(duplicate-items (list) (list 2 1) ))`)).to.deep.equal(makeOk(listPrim([])));

    }
    );
    it(`payment`,()=>
    {
        expect(evalP(`(L5`+q2+`(payment 10 (list 5 5 10)))`)).to.deep.equal(makeOk(2));
        expect(evalP(`(L5`+q2+`(payment 5 (list 1 1 1 1 1 2 2)))`)).to.deep.equal(makeOk(3));
        expect(evalP(`(L5`+q2+`(payment 5 '()))`)).to.deep.equal(makeOk(0));                
        expect(evalP(`(L5`+q2+`(payment 3 (list 5 5 10)))`)).to.deep.equal(makeOk(0)); 
        expect(evalP(`(L5`+q2+`(payment 10 (list 1 2 10)))`)).to.deep.equal(makeOk(1));
     }
    );
    it(`"compose-n`,()=>
    {
        expect(evalP(`(L5`+q2+`((compose-n (lambda(x) (+ 2 x))2) 3))`)).to.deep.equal(makeOk(7));
        expect(evalP(`(L5`+q2+`((compose-n (lambda(x) (* 2 x))2) 3))`)).to.deep.equal(makeOk(12));
        expect(evalP(`(L5`+q2+`((compose-n (lambda(x) (* 2 x))1) 4))`)).to.deep.equal(makeOk(8));
        expect(evalP(`(L5`+q2+`((compose-n (lambda(x) (- 2 x))1) 7))`)).to.deep.equal(makeOk(-5)); 
        expect(evalP(`(L5`+q2+`((compose-n (lambda(x) (/ 2 x))1) 4))`)).to.deep.equal(makeOk(0.5));
        
    }
    );
});

