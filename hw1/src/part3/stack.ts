import { State, bind } from "./state";

export type Stack = number[];

export const push : (x:number)=>State<Stack,undefined> = x=>
{
    return k=>[[x].concat(k),undefined];
}


export const pop:(q:Stack)=>[Stack,number] = q=>
{
    const val:number=q[0];
    const q1:Stack=q.slice(1);
    return [q1,val];
}


export const stackManip = undefined;