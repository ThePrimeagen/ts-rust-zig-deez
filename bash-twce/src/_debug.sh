#!/usr/bin/env bash
# vim: ft=bash sw=4 ts=4 sts=4 tw=80
#
# Debugging, pretty-printing, etc.
#-------------------------------------------------------------------------------

# If sourcing this file, probably doing some debugging.
trap '(( $? )) && traceback 2' EXIT

function traceback {
   local -i depth="$1"
   local -i len=${#FUNCNAME[@]}-1

   (( depth = (depth < 1) ? 1 : depth ))

   printf 'Traceback:\n'
   for (( i=len; i>=depth; --i )) ; do
      printf '%5sln.%4d in %-28s%s\n'  \
         ''                            \
         "${BASH_LINENO[$i-1]}"        \
         "${FUNCNAME[$i]}"             \
         "${BASH_SOURCE[$i]##*/}"
   done
}


function pp_token {
   local t="$1"
   local -n t_r="$t"
   local -n l_r="${t_r[location]}"

   local params=(
      "${t_r['type']}"
      "${l_r['start_ln']}" "${t_r['start_ln']}"
      "${l_r['end_ln']}"   "${t_r['end_col']}"
      "${t_r['value']}"
   )

   printf '%-11s [%4d:%-3d -> %4d:%-3d]  %s\n'  "${params[@]}" >&2
}
