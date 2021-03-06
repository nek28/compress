I wanted to remain as close to the paper (which was written for Gofer) as possible and so I didn't define an instance of Monad, but did my own functions (**(|-)** for bind and **result** for return)

The grammar that the parser recognizes is something like this:

    A -> charA

    A -> number<A>A

    A -> _

Some examples, and how they'd be parsed:

    abc3<def> -> abcdefdefdef

    pq5<a2<b>>op -> pqabbabbabbabbabbop

    3<a4<b5<c>>> -> abbbbcccccabbbbcccccabbbbccccc

The input string is parsed into a **Compressed** value, which is then passed through the **transform** function. It looks for repeat blocks that are nested in each other without any text before or after them, and fuses them together.

    ...3<ab4<5<pq>>>... ==> ...3<ab20<pq>>...
    