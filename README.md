I wanted to remain as close to the paper (which was written for Gofer) as possible and so I didn't define an instance of Monad, but did my own functions (**(|-)** for bind and **result** for return)

The grammar that the parser recognizes is something like this:
    A -> \[char\]A
    A -> \[number\](A)
    A -> _
