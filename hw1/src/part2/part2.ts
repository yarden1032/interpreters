import * as R from "ramda";


const stringToArray = R.split("");


/* Question 1 */
const vowels: string[]=['a','e','i','o','u','A','E','I','O','U'];
const checkIfVowel: (c: string)=>boolean=c=>vowels.indexOf(c)!==-1;

export const countVowels: (x:string)=>number = x=>{
    const arr: string[]=stringToArray(x);
    return arr.reduce((acc,n)=>checkIfVowel(n)? acc+1: acc ,0)
};



/* Question 2 */
const helper:(i:number,arr:string[],val:string)=>number=(i,arr,val)=>
{
    return (i<arr.length&&arr[i] == val) ?  helper(i+1, arr, val):i;
}

const encoding:(total:string,currentVal:string,index:number,array:string[])=>any=(total,currentVal,index,array)=>
{
    const i:number=helper(index,array,currentVal)
    const calcu:number=i-index;
    const temp:string=( calcu>1)?
      currentVal+calcu: currentVal;
      
    return(i<array.length)?  temp+encoding(total,array[i],i,array):temp;
}

export const runLengthEncoding: (x:string)=>string=x=>
{
    const arr:string[]=stringToArray(x);
    const z:string=encoding("",arr[0],0,arr);
    return z;
}


/* Question 3 */
interface res
{
arrOfBools:boolean[];
arrOfOp: string[];
}

const op: string[]=['{','(','['];
const cl: string[]=['}',')',']'];

const checkIfP: (c: string)=>boolean=c=>op.indexOf(c)!==-1;
const checkIfC: (c: string)=>boolean=c=>cl.indexOf(c)!==-1;

const pMaker:(arr: string[])=>res=arr=>{
    const output: res = arr.reduce((acc: res, n: string) => {
        if (checkIfP(n)) {
            return { arrOfBools: acc.arrOfBools, arrOfOp: acc.arrOfOp.concat([n]) };
        }
        else {
            if (checkIfC(n)) {
                if (acc.arrOfOp.length>0&&(Math.abs(acc.arrOfOp[acc.arrOfOp.length - 1].charCodeAt(0) - n.charCodeAt(0)) < 3)) {
                    return { arrOfBools: acc.arrOfBools.concat([true]), arrOfOp: acc.arrOfOp.slice(0, acc.arrOfOp.length - 1) };
                }
                else 
                {
                    return { arrOfBools: [false], arrOfOp: [""] };
                }
            }
            return acc;
        }
    },{arrOfBools:[], arrOfOp:[]});
    return output;
}

export const isPaired:(x: string)=>boolean=x=>{
    const arr:string[]=stringToArray(x);
    const calc:res=pMaker(arr);
    return (calc.arrOfBools.every(cur=>cur==true)&&calc.arrOfOp.length==0);

}