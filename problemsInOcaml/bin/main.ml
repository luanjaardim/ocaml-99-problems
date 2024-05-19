let main () =
  (* Problem 1 *)
  assert (Funcs.last [ "a"; "b"; "c"; "d" ] = Some "d");
  assert (Funcs.last [] = None);
  (* Problem 2 *)
  assert (Funcs.last_n_penultimate [ "a"; "b"; "c"; "d" ] = Some ("c", "d"));
  assert (Funcs.last_n_penultimate [ "a" ] = None);
  (* Problem 3 *)
  assert (Funcs.at 3 [ "a"; "b"; "c"; "d"; "e" ] = Some "c");
  assert (Funcs.at 3 [ "a" ] = None);
  (* Problem 4 *)
  assert (Funcs.length [ "a"; "b"; "c" ] = 3);
  assert (Funcs.length [] = 0);
  (* Problem 5 *)
  assert (Funcs.rev [ "a"; "b"; "c" ] = [ "c"; "b"; "a" ]);
  (* Problem 6 *)
  assert (Funcs.is_palin [ "x"; "a"; "m"; "a"; "x" ] = true)
in
main ()
