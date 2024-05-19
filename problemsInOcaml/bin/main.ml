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
  assert (Funcs.is_palin [ "x"; "a"; "m"; "a"; "x" ] = true);
  (* Problem 7 *)
  assert (
    Funcs.flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
    = [ "a"; "b"; "c"; "d"; "e" ]);
  (* Problem 8 *)
  let longList = [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ] in
  assert (Funcs.compress longList = [ "a"; "b"; "c"; "a"; "d"; "e" ]);
  (* Problem 9 *)
  assert (
    Funcs.pack longList
    = [ [ "a"; "a"; "a"; "a" ]
      ; [ "b" ]
      ; [ "c"; "c" ]
      ; [ "a"; "a" ]
      ; [ "d" ]
      ; [ "e"; "e"; "e"; "e" ]
      ]);
  (* Problem 10 *)
  assert (Funcs.encode longList = [ 4, "a"; 1, "b"; 2, "c"; 2, "a"; 1, "d"; 4, "e" ]);
  (* Problem 11 *)
  assert (
    Funcs.rle_encode longList
    = [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]);
  (* Problem 12 *)
  assert (
    Funcs.rle_decode
      [ Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e") ]
    = longList);
  (* Problem 13, same as 11 *)
  (* Problem 14 *)
  assert (
    Funcs.dup_elems [ "a"; "b"; "c"; "c"; "d" ]
    = [ "a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d" ]);
  (* Problem 15 *)
  assert (Funcs.replicate [ "a"; "b"; "c" ] 3 = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]);
  (* Problem 16 *)
  assert (
    Funcs.drop_nth [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
    = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]);
  (* Problem 17 *)
  assert (
    Funcs.split_at_pos [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
    = ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ]));
  assert (Funcs.split_at_pos [ "a"; "b"; "c"; "d" ] 5 = ([ "a"; "b"; "c"; "d" ], []))
in
main ()
