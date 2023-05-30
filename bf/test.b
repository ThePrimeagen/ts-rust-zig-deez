
    Check if ident [
        This uses the same algorithm for numbers but more advanced to support
		lower and upper letters along with numbers

        Store digit counter in $4
        >> +++++++++++++++++++++++++

        Loop through lower letters using counter and equality [
			Clone the input byte at $1 to $2 and advance to $3
            <<< [>+>+<<-]>>[<<+>>-]

            Subtract the 'a' character code from $2
            < ------------------------------------------------------------------------------------------------

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

				Reset letter counter
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

            Move to and decrement digit counter at $5
            >> -
        ] <<<<
		
