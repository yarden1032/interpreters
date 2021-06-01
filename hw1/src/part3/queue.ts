import { concat } from "ramda";
import { State, bind } from "./state";

export type Queue = number[];

export const enqueue : (x:number|undefined)=>State<Queue,(number|undefined)> = x=>
{
    if(x!=undefined){
 const  st:State <Queue,(number|undefined)> = k=>[k.concat([x]),undefined];
    return st;
}
    else
    return  k=>[k,undefined];
}

export const dequeue:(q:Queue)=>[Queue,(number|undefined)] = q=>
{
    const val:number|undefined=q[0];
    const q1:Queue=q.slice(1);
    return [q1,val];
}

export const queueManip:(q:Queue)=>[Queue,number|undefined]= q=>
{

const s1:State<Queue,(number|undefined)>=bind(q1=>dequeue(q1),(n:number|undefined)=>bind(q1=>enqueue(n!=undefined?(n*2):undefined)(q1),(n:number|undefined)=>bind(q1=>enqueue(n!=undefined?(n/3):undefined)(q1),q1=>q1=>dequeue(q1))))



return s1(q);


}

