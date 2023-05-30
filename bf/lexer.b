Load the input byte at $1
> ,

Set the eating flag at $6 to true
>>>>> + <<<<<

State machine loop [

    Set the continue flag at $0 to true
    < +


    Check if left paren
        Clone the input byte at $1 to $2 and advance to $3
        > [>+>+<<-]>>[<<+>>-]
        
        Set $3 to left paren
        ++++++++++++++++++++++++++++++++++++++++
        
        Set $3 to equality
        [-<->]+<[>-<[-]]>
        
        If they are equal [
            [-]
            <<< [-] >>>
            +.
            [-]
        ] <<<
    
    Check if right paren [
        Clone the input byte at $1 to $2 and advance to $3
        > [>+>+<<-]>>[<<+>>-]

        Set $3 to right paren
        +++++++++++++++++++++++++++++++++++++++++

        Set $3 to equality
        [-<->]+<[>-<[-]]>
        
        If they are equal [
            [-]
            <<< [-] >>>
            ++.
            [-]
        ] <<<

    Check if left curly [
        Clone the input byte at $1 to $2 and advance to $3
        > [>+>+<<-]>>[<<+>>-]

        Set $3 to left curly
        +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        Set $3 to equality
        [-<->]+<[>-<[-]]>
        
        If they are equal [
            [-]
            <<< [-] >>>
            +++.
            [-]
        ] <<<

    Check if right curly [
        Clone the input byte at $1 to $2 and advance to $3
        > [>+>+<<-]>>[<<+>>-]

        Set $3 to right curly
        +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        Set $3 to equality
        [-<->]+<[>-<[-]]>
        
        If they are equal [
            [-]
            <<< [-] >>>
            ++++.
            [-]
        ] <<<

    Check if comma [
        Clone the input byte at $1 to $2 and advance to $3
        > [>+>+<<-]>>[<<+>>-]

        Set $3 to comma
        ++++++++++++++++++++++++++++++++++++++++++++

        Set $3 to equality
        [-<->]+<[>-<[-]]>
        
        If they are equal [
            [-]
            <<< [-] >>>
            +++++.
            [-]
        ] <<<

    Check if semi colon [
        Clone the input byte at $1 to $2 and advance to $3
        > [>+>+<<-]>>[<<+>>-]

        Set $3 to semi colon
        +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        Set $3 to equality
        [-<->]+<[>-<[-]]>
        
        If they are equal [
            [-]
            <<< [-] >>>
            ++++++.
            [-]
        ] <<<

    Check if plus [
        Clone the input byte at $1 to $2 and advance to $3
        > [>+>+<<-]>>[<<+>>-]

        Set $3 to plus
        +++++++++++++++++++++++++++++++++++++++++++

        Set $3 to equality
        [-<->]+<[>-<[-]]>
        
        If they are equal [
            [-]
            <<< [-] >>>
            +++++++.
            [-]
        ] <<<

    Check if equal [
        Clone the input byte at $1 to $2 and advance to $3
        > [>+>+<<-]>>[<<+>>-]

        Set $3 to equal
        +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        Set $3 to equality
        [-<->]+<[>-<[-]]>
        
        If they are equal [
            [-]
            <<< [-] >>>
            ++++++++.
            [-]
        ] <<<

    Skip space characters [
        Clone the input byte at $1 to $2 and advance to $3
        > [>+>+<<-]>>[<<+>>-]

        Set $3 to space
        ++++++++++++++++++++++++++++++++

        Set $3 to equality
        [-<->]+<[>-<[-]]>
        
        If they are equal [
            Reset continue flag
            <<< [-] >>>
            [-]
        ] <<<

    Skip tab characters [
        Clone the input byte at $1 to $2 and advance to $3
        > [>+>+<<-]>>[<<+>>-]

        Set $3 to tab
        +++++++++

        Set $3 to equality
        [-<->]+<[>-<[-]]>
        
        If they are equal [
            Reset continue flag
            <<< [-] >>>
            [-]
        ] <<<

    Skip newline characters [
        Clone the input byte at $1 to $2 and advance to $3
        > [>+>+<<-]>>[<<+>>-]

        Set $3 to newline
        ++++++++++

        Set $3 to equality
        [-<->]+<[>-<[-]]>
        
        If they are equal [
            Reset continue flag
            <<< [-] >>>
            [-]
        ] <<<

    Check if digit [
        This is a custom algorithm I designed which basically subtracts the
        lower bounds digit '0' from the number and checks if it is equal to zero
        ten times which iterates all digit characters

        Store digit counter in $4
        >>>> +++++++++

        Loop through digits using counter and equality [
			Clone the input byte at $1 to $2 and advance to $3
            <<< [>+>+<<-]>>[<<+>>-]

            Subtract the 0 minus 1 character code from $2
            < -----------------------------------------------

			Move to digit counter at $4
			>>

            Reset $5 used as clobber
            > [-] <
            
            Reset $3 used as clobber
            < [-] >

            Copy counter at $4 to $3 and move to $3
            [-<+>>+<]>[-<+>]<<

            Set $3 to equality
            [-<->]+<[>-<[-]]>

            If they are equal [
                [-]
                <<< [-] >>>
                > [-] ++++++++++ <

                If eating is true >>> [
                    Emit digit start
                    < [-] ++++++++++. [-] >

                    Set eating to false
                    [-]
                ] <<<

                << ., >>

                [-]
            ]

            Move to and decrement digit counter at $4
            > -
        ] <<<<

    Check if ident [
        This is based off of the number algorithm but composed to match through
		upper and lower letters along with digits

        Store letter counter in $4
        >>>> +++++++++++++++++++++++++

        Loop through lower letters using counter and equality [
			Clone the input byte at $1 to $2 and advance to $3
            <<< [>+>+<<-]>>[<<+>>-]

            Subtract the 'a' character code from $2
            < ------------------------------------------------------------------------------------------------

			Move to letter counter at $4
			>>

            Reset $5 used as clobber
            > [-] <
            
            Reset $3 used as clobber
            < [-] >

            Copy counter at $4 to $3 and move to $3
            [-<+>>+<]>[-<+>]<<

            Set $3 to equality
            [-<->]+<[>-<[-]]>

            If they are equal [
                [-]
                <<< [-] >>>
                > [-] +++++++++++++++++++++++++ <

                If eating is true >>> [
                    Emit ident start
                    < [-] +++++++++. [-] >

                    Set eating to false
                    [-]
                ] <<<

                << ., >>

                [-]
            ]

            Move to and decrement digit counter at $4
            > -
        ] <<<<
        
		Store letter counter in $4
        >>>> +++++++++++++++++++++++++

        Loop through upper letters using counter and equality [
			Clone the input byte at $1 to $2 and advance to $3
            <<< [>+>+<<-]>>[<<+>>-]

            Subtract the 'A' character code from $2
            < ----------------------------------------------------------------

			Move to letter counter at $4
			>>

            Reset $5 used as clobber
            > [-] <
            
            Reset $3 used as clobber
            < [-] >

            Copy counter at $4 to $3 and move to $3
            [-<+>>+<]>[-<+>]<<

            Set $3 to equality
            [-<->]+<[>-<[-]]>

            If they are equal [
                [-]
                <<< [-] >>>
                > [-] +++++++++++++++++++++++++ <

                If eating is true >>> [
                    Emit ident start
                    < [-] +++++++++. [-] >

                    Set eating to false
                    [-]
                ] <<<

                << ., >>

                [-]
            ]

            Move to and decrement digit counter at $4
            > -
        ] <<<<

    If continue flag still set emit illegal token [
        [-]
        Emit illegal token
        +++++++++++.
        [-]
    ]

    ]]]]]]]]]]]]

    =========

    Reset the continue flag at $0 to false
    [-]

    Reset the clone byte at $2 to 0
    >> [-]

    Set $5 as else boolean with true
    >>> [-] +

    If the eating flag at $6 is set eat forwards > [
        Reset eating flag
        [-]

        Set else to false
        < - >

        Load the next input byte at $1
        <<<<< , >>>>>
    ]

    If not eating flag at $5 < [
        Emit null terminator for numbers or identifiers
        [-] .
    ]

    Set eating flag at $6 to true
    > +

    Move back to input at $1
    <<<<<
]

Decrement saved memory and output eof
[-] .
