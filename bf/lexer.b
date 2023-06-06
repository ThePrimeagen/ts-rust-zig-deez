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
			Reset continue flag at $3
            <<< [-] >>>
            
			Emit left paren token
			[-] +.
			
			Reset equality
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
			Reset continue flag at $3
            <<< [-] >>>
            
			Emit right paren token
			[-] ++.
			
			Reset equality
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
			Reset continue flag at $3
            <<< [-] >>>
            
			Emit left curly token
			[-] +++.
			
			Reset equality
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
			Reset continue flag at $3
            <<< [-] >>>
            
			Emit right curly token
			[-] ++++.
			
			Reset equality
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
			Reset continue flag at $3
            <<< [-] >>>
            
			Emit comma token
			[-] +++++.
			
			Reset equality
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
			Reset continue flag at $3
            <<< [-] >>>
            
			Emit semi colon token
			[-] ++++++.
			
			Reset equality
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
			Reset continue flag at $3
            <<< [-] >>>
            
			Emit plus token
			[-] +++++++.
			
			Reset equality
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
			Reset continue flag at $3
            <<< [-] >>>
            
			Emit equal token
			[-] ++++++++.
			
			Reset equality
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
			
			Reset equality
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
			
			Reset equality
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
			
			Reset equality
            [-]
        ] <<<

    Check if digit [
        This is a custom algorithm I designed which basically subtracts the
        lower bounds digit '0' from the number and checks if it is equal to zero
        ten times which iterates all digit characters

        Store digit counter in $4
        >>>> +++++++++++

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
                Reset equality
                [-]

                Set continue at $0 to false
                <<< [-] >>>

                Reset digit counter at $4
                > [-] +++++++++++ <

                If eating is true >>> [
                    Emit digit start
                    < [-] ++++++++++. [-] >

                    Set eating to false
                    [-]
                ] <<<

                Emit current input byte and load next at $1
                << ., >>
            ]

            Move to and decrement digit counter at $4
            > -
        ] <<<<

    Check if ident [

	The following algorithm is inspired by the number algorithm but it requires
	creating another loop to ensure that we capture all characters

	Move to $7 at ident continue and increment it
	>>>>>>> [-] + [ <<<<<<<
        Store letter counter in $4
        >>>> ++++++++++++++++++++++++++

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
                Reset continue flag at $0
                [-]

				Increment ident counter at $7
				>>>> + <<<<

                Reset continue flag at $0
                <<< [-] >>>

                Reset letter counter at $4
                > [-] ++++++++++++++++++++++++++ <

                If eating is true >>> [
                    Emit ident start
                    < [-] +++++++++. [-] >

                    Set eating to false
                    [-]
                ] <<<

                Emit current input byte and load next at $1
                << ., >>
            ]

            Move to and decrement digit counter at $4
            > -
        ] <<<<
        
        Store letter counter in $4
        >>>> ++++++++++++++++++++++++++

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
                Reset continue flag at $0
                <<< [-] >>>

				Increment ident counter at $7
				>>>> + <<<<

                Reset letter counter at $4
                > [-] ++++++++++++++++++++++++++ <

                If eating is true >>> [
                    Emit ident start
                    < [-] +++++++++. [-] >

                    Set eating to false
                    [-]
                ] <<<

                Emit current input byte and load next at $1
                << ., >>

                Reset equality at $3
                [-]
            ]

            Move to and decrement digit counter at $4
            > -
        ] <<<<

        Store digit counter in $4
        >>>> +++++++++++

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
				Increment ident counter at $7
				>>>> + <<<<

                Reset equal at $3
                [-]

                Reset continue flag at $0
                <<< [-] >>>

                Store digit counter in $4
                > [-] +++++++++++ <

                Log out input byte and reload next byte in $1
                << ., >>
            ]

            Move to and decrement digit counter at $4
            > -
        ]

	Move back to ident continue flag at $7 and decrement it
	>>> -
	
	Rerun ident loop and shift back to continue flag after
	] <<<<<<<

    If continue flag still set emit illegal token [
        Emit illegal token using continue flag as scratch
        [-] +++++++++++. [-]

        Emit illegal input byte in $1
        > . <
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
        Reset eating flag at $6
        [-]

        Set else at $5 to false
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
