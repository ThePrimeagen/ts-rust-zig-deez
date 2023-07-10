import Lake
open Lake DSL

package «lean» {
  -- add package configuration options here
}

lean_lib «Deez» {
  -- add library configuration options here
}

@[default_target]
lean_exe «lean» {
  root := `Main
}
