export type State<S, A> = (initialState: S) => [S, A];

export const bind = <S, A, B>(state: State<S, A>, f: (x: A) => State<S, B>)  =>
{
    return ((s:S):[S,B]=>f(state(s)[1])(s));
}
