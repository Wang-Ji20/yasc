fun lookupInEntry(name: string, entry: string list list) =
  case entry of
    [] => raise Fail "lookupInEntry"
   | (name'::args)::entry' =>
      if name = name' then entry-f args
      else lookupInEntry name entry' entry-f

