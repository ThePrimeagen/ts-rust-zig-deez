#!/usr/bin/env bats
# vim: ft=bash sw=4 ts=4 sts=4

function setup {
    export SRC="${BATS_TEST_DIRNAME}/../src"
}


@test "source" {
    source "${SRC}/lexer.sh"
}


@test "empty input" {
    source "${SRC}/lexer.sh"
    lexer_scan ''
}


@test "identify valid symbols" {
    source "${SRC}/lexer.sh"
    lexer_scan '
        = + , ; : ( ) [ ] { }
    '

    local expected=(
        'EQUAL'
        'PLUS'
        'COMMA'
        'SEMI'
        'COLON'
        'L_PAREN'
        'R_PAREN'
        'L_BRACKET'
        'R_BRACKET'
        'L_BRACE'
        'R_BRACE'
    )

    for idx in "${!expected[@]}" ; do
        local exp="${expected[idx]}"
        local -n token_r="${TOKENS[idx]}" 
        [[ ${token_r[type]} == "$exp" ]]
    done
}


@test "identify illegal symbols" {
    source "${SRC}/lexer.sh"
    lexer_scan '
        . * &
    '

    local expected=(
        'ILLEGAL'
        'ILLEGAL'
        'ILLEGAL'
        'EOF'
    )

    for idx in "${TOKENS[@]}" ; do
        local exp="${expected[idx]}"
        local token="${TOKENS[idx]}"
        local -n token_r="$token"
        [[ ${token_r[type]} == "$exp" ]]
    done
}


@test "identifiers vs. keywords" {
    source "${SRC}/lexer.sh"
    lexer_scan '
        foo bar fn let
    '

    local -A EXP_0=( [type]='IDENTIFIER'   [value]='foo' )
    local -A EXP_1=( [type]='IDENTIFIER'   [value]='bar' )
    local -A EXP_2=( [type]='FUNCTION'     [value]='fn'  )
    local -A EXP_3=( [type]='LET'          [value]='let' )
    local -A EXP_4=( [type]='EOF'          [value]=''    )

    for idx in "${!TOKENS[@]}" ; do
        local token="${TOKENS[idx]}"
        local -n token_r="$token"
        local -n exp_r="EXP_${idx}"

        [[ ${token_r[type]}  == "${exp_r[type]}"  ]]
        [[ ${token_r[value]} == "${exp_r[value]}" ]]
    done
}


# Single-width, multi-width, and multi-line tokens.
@test "location information" {
    source "${SRC}/lexer.sh"

    lexer_scan '0

    this

    "a
    multiline
    string"
    '

    local -n token0_r="${TOKENS[0]}"
    local -n loc_r="${token0_r[location]}"
    (( loc_r[start_ln]   == 1 &&
       loc_r[start_col]  == 1 &&
       loc_r[end_ln]     == 1 &&
       loc_r[end_col]    == 1
    ))

    local -n token1_r="${TOKENS[1]}"
    local -n loc_r="${token1_r[location]}"
    (( loc_r[start_ln]   == 3 &&
       loc_r[start_col]  == 5 &&
       loc_r[end_ln]     == 3 &&
       loc_r[end_col]    == 8
    ))

    local -n token2_r="${TOKENS[2]}"
    local -n loc_r="${token2_r[location]}"
    (( loc_r[start_ln]   == 5 &&
       loc_r[start_col]  == 5 &&
       loc_r[end_ln]     == 7 &&
       loc_r[end_col]    == 11
    ))
}


@test "unterminated string throws error" {
    source "${SRC}/lexer.sh"
    run lexer_scan '"unterminated string'

    [[ $output =~ 'Unterminated string.' ]]
    [[ $status == 1 ]]
}


@test "lexes ints & floats" {
    source "${SRC}/lexer.sh"
    run lexer_scan '1 10 10.0 10.00'
    
    local -A EXP_0=( [type]='NUMBER'   [value]='1'     )
    local -A EXP_1=( [type]='NUMBER'   [value]='10'    )
    local -A EXP_2=( [type]='NUMBER'   [value]='10.0'  )
    local -A EXP_3=( [type]='NUMBER'   [value]='10.00' )
    local -A EXP_4=( [type]='NUMBER'   [value]=''      )

    for idx in "${!TOKENS[@]}" ; do
        local token="${TOKENS[idx]}"
        local -n token_r="$token"
        local -n exp_r="EXP_${idx}"

        [[ ${token_r[type]}  == "${exp_r[type]}"  ]]
        [[ ${token_r[value]} == "${exp_r[value]}" ]]
    done
}


@test "fixed dumb issue with floats" {
    skip "Don't yet know if this is an error."
    source "${SRC}/lexer.sh"

    # Don't know if this should be a syntax error here, or throw error during
    # semantic analysis. Latter would allow for methods on ints if applicable?
    # Iunno.
    run lexer_scan '
        10.0
        10. 0
        10. foo
    '
}
