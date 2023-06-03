#!/usr/bin/env bash
# vim: ft=bash sw=4 ts=4 sts=4
#-------------------------------------------------------------------------------
set -e

declare -gA KEYWORD=(
   ['fn']='FUNCTION'
   ['let']='LET'
)

declare -g   CHAR=''
declare -g   PEEK_CHAR=''
declare -g   LOCATION=

declare -ga  CHARRAY=()
declare -ga  TOKENS=()

declare -gA  FREEZE=()
declare -gA  CURSOR=(
   [index]=-1
   [lineno]=1
   [colno]=0
)

#                             constructors & utils
#-------------------------------------------------------------------------------
function location_new {
   # Using "object" here to hold location information so it can be later
   # transferred to the AST node. E.g., the binary expression `1 + 2` would
   # contain a 'start location' of the 1's .location, and an 'end location' of
   # the 2's .location. Allows for significantly better error reporting with
   # insignificant overhead in the lexer.

   local loc="LOC_$(( ++_LOC_NUM ))"
   declare -gA "$loc"
   declare -g  LOCATION="$loc"

   local -n loc_r="$loc"
   loc_r['start_ln']=
   loc_r['start_col']=
   loc_r['end_ln']=
   loc_r['end_col']=
}


function token_new {
   local type=$1  value=$2

   local token="TOKEN_$(( ++_TOKEN_NUM ))"
   declare -gA "$token"
   TOKENS+=( "$token" )

   declare -n t_r="$token"
   t_r['type']="$type"
   t_r['value']="$value"

   location_new
   t_r['location']="$LOCATION"

   # shellcheck disable=SC2178
   local -n loc_r="$LOCATION"
   loc_r['start_ln']="${FREEZE[lineno]}"
   loc_r['start_col']="${FREEZE[colno]}"
   loc_r['end_ln']="${CURSOR[lineno]}"
   loc_r['end_col']="${CURSOR[colno]}"
}


function lexer_advance {
   # As the index is initially primed to `-1' prior to the lexer starting, the
   # first increment sets it to `0'. In an arithmetic context, that evaluates
   # to a non-0 exit code. This is a silly way to ensure the increment always
   # returns a successful status.
   (( ++CURSOR['index'] , 1 ))
   (( ++CURSOR['colno']     ))
   local -i idx="${CURSOR[index]}"

   declare -g CHAR="${CHARRAY[$idx]}"
   declare -g PEEK_CHAR="${CHARRAY[$idx + 1]}"

   if [[ "$CHAR" == $'\n' ]] ; then
      (( ++CURSOR['lineno'] ))
      CURSOR['colno']=0
   fi
}

#                                 the scanner
#-------------------------------------------------------------------------------
function lexer_scan {
   local input="$1"

   # Easier lookahead, make array of characters. Surprisingly the `while read`
   # seems to be faster than looping & indexing into the string with parameter
   # substitution:  ${input:i:1}
   local char
   while read -rN1 char ; do
      CHARRAY+=( "$char" )
   done <<< "$input"

   while (( "${CURSOR[index]}" < ${#CHARRAY[@]} )) ; do
      lexer_advance ; [[ ! "$CHAR" ]] && break

      # Start of token cursor information.
      FREEZE['index']=${CURSOR['index']}
      FREEZE['lineno']=${CURSOR['lineno']}
      FREEZE['colno']=${CURSOR['colno']}

      if [[ $CHAR =~ [[:space:]] ]] ; then
         continue
      fi

      case $CHAR in
         '=')  token_new  'EQUAL'     "$CHAR"  ; continue ;;
         '+')  token_new  'PLUS'      "$CHAR"  ; continue ;;
         ',')  token_new  'COMMA'     "$CHAR"  ; continue ;;
         ';')  token_new  'SEMI'      "$CHAR"  ; continue ;;
         ':')  token_new  'COLON'     "$CHAR"  ; continue ;;
                                      
         '(')  token_new  'L_PAREN'   "$CHAR"  ; continue ;;
         ')')  token_new  'R_PAREN'   "$CHAR"  ; continue ;;
                     
         '[')  token_new  'L_BRACKET' "$CHAR"  ; continue ;;
         ']')  token_new  'R_BRACKET' "$CHAR"  ; continue ;;
                     
         '{')  token_new  'L_BRACE'   "$CHAR"  ; continue ;;
         '}')  token_new  'R_BRACE'   "$CHAR"  ; continue ;;
      esac

      # Strings. Surrounded by `"` only.
      if [[ $CHAR == '"' ]] ; then
         lexer_string ; continue
      fi

      # Identifiers.
      if [[ $CHAR =~ [[:alpha:]_] ]] ; then
         lexer_identifier ; continue
      fi

      # Numbers.
      if [[ $CHAR =~ [[:digit:]] ]] ; then
         lexer_number ; continue
      fi

      token_new 'ILLEGAL'  "$CHAR" ; continue
   done

   FREEZE[lineno]="${CURSOR[lineno]}"
   FREEZE[colno]="${CURSOR[colno]}"
   token_new 'EOF'
}


function lexer_identifier {
   local buffer="$CHAR"

   while [[ -n $CHAR ]] ; do
      [[ $PEEK_CHAR =~ [^[:alnum:]_] ]] && break
      lexer_advance ; buffer+="$CHAR"
   done

   local keyword=${KEYWORD[$buffer]}
   if [[ $keyword ]] ; then
      token_new "$keyword"  "$buffer"
   else
      token_new 'IDENTIFIER'  "$buffer"
   fi
}


function lexer_number {
   local number="${CHAR}"
   while [[ $PEEK_CHAR =~ [[:digit:]] ]] ; do
      lexer_advance ; number+="$CHAR"
   done

   # TODO: may want to throw a syntax error if there's a period *not*
   # immediately followed by a number.
   if [[ $PEEK_CHAR == '.' ]] ; then
      lexer_advance ; number+='.'
      while [[ $PEEK_CHAR =~ [[:digit:]] ]] ; do
         lexer_advance ; number+="$CHAR"
      done
   fi

   token_new 'NUMBER' "$number"
}


function lexer_string {
   local -a buffer=()

   while [[ $PEEK_CHAR ]] ; do
      lexer_advance

      if [[ $CHAR == '"' ]] ; then
         # shellcheck disable=SC1003,2128
         if [[ $buffer && ${buffer[-1]} == '\' ]] ; then
            unset 'buffer[-1]'
         else
            break
         fi
      fi

      buffer+=( "$CHAR" )
   done

   local join=''
   for c in "${buffer[@]}" ; do
      join+="$c"
   done

   if [[ ! $PEEK_CHAR ]] ; then
      printf 'Unterminated string.\n' >&2
      exit 1
   fi

   token_new 'STRING' "$join"
}
