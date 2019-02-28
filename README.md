I wanted to remain as close to the paper (which was written for Gofer) as possible and so I didn't define an instance of Monad, but did my own functions (**(|-)** for bind and **result** for return)

The grammar that the parser recognizes is something like this:

    A -> charA

    A -> number<A>A

    A -> _

Some examples, and how they'd be parsed:

    abc3<def> --> abcdefdefdef

    pq5<a2<b>>op -> pqabbabbabbabbabbop

    3<a4<b5<c>>> -> abbbbcccccabbbbcccccabbbbccccc
    
The embedding is deep (it parses it to a **Compressed** value, which splits the string to a list of normal strings and repeat blocks that can be nested). One transformation that could be done before expanding is to merge two repeat blocks together if one is immediately nested inside the other and nothing follows it (e.g. 3<2<kl>> ==> 6<kl> or 2<2<3<xx>>> ==> 12<xx>).
