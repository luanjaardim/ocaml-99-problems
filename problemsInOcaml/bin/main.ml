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
  let longList =
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  in
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
  assert (
    Funcs.replicate [ "a"; "b"; "c" ] 3 = [ "a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c" ]);
  (* Problem 16 *)
  assert (
    Funcs.drop_nth [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
    = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]);
  (* Problem 17 *)
  assert (
    Funcs.split_at_pos [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
    = ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ]));
  assert (Funcs.split_at_pos [ "a"; "b"; "c"; "d" ] 5 = ([ "a"; "b"; "c"; "d" ], []));
  (* Problem 18 *)
  assert (
    Funcs.slice_from_list [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6
    = [ "c"; "d"; "e"; "f"; "g" ]);
  (* Problem 19 *)
  assert (
    Funcs.rotate_left [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
    = [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]);
  assert (
    Funcs.rotate_left [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 11
    = [ "d"; "e"; "f"; "g"; "h"; "a"; "b"; "c" ]);
  assert (
    Funcs.rotate_left [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] (-2)
    = [ "g"; "h"; "a"; "b"; "c"; "d"; "e"; "f" ]);
  (* Problem 20 *)
  assert (Funcs.remove_at 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "c"; "d" ]);
  (* Problem 21 *)
  assert (Funcs.insert_at "alfa" 1 [ "a"; "b"; "c"; "d" ] = [ "a"; "alfa"; "b"; "c"; "d" ]);
  assert (Funcs.insert_at "alfa" 3 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "c"; "alfa"; "d" ]);
  assert (Funcs.insert_at "alfa" 4 [ "a"; "b"; "c"; "d" ] = [ "a"; "b"; "c"; "d"; "alfa" ]);
  (* Problem 22 *)
  assert (Funcs.range 4 9 = [ 4; 5; 6; 7; 8; 9 ]);
  assert (Funcs.range 9 4 = [ 9; 8; 7; 6; 5; 4 ]);
  (* Problem 23 *)
  (* WARNING: This assert can fail if the pseudorandom values change *)
  assert (
    Funcs.extract_randomly [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
    = [ "c"; "a"; "b" ]);
  (* Problem 24 *)
  (* WARNING: This assert can fail if the pseudorandom values change *)
  assert (Funcs.lotto_select ~qtd:5 ~end_bound:10 = [ 6; 4; 8; 1; 10 ]);
  (* Problem 25 *)
  assert (
    Funcs.permutation [ "a"; "b"; "c"; "d"; "e"; "f" ] = [ "f"; "e"; "c"; "a"; "b"; "d" ]);
  (* Problem 26 *)
  assert (
    Funcs.combination 2 [ "a"; "b"; "c"; "d" ]
    = [ [ "a"; "b" ]
      ; [ "a"; "c" ]
      ; [ "a"; "d" ]
      ; [ "b"; "c" ]
      ; [ "b"; "d" ]
      ; [ "c"; "d" ]
      ]);
  (* Problem 27 *)
  assert (
    Funcs.group [ "a"; "b"; "c"; "d" ] [ 2; 1 ]
    = [ [ [ "c"; "d" ]; [ "a" ] ]
      ; [ [ "c"; "d" ]; [ "b" ] ]
      ; [ [ "b"; "d" ]; [ "a" ] ]
      ; [ [ "b"; "d" ]; [ "c" ] ]
      ; [ [ "b"; "c" ]; [ "a" ] ]
      ; [ [ "b"; "c" ]; [ "d" ] ]
      ; [ [ "a"; "d" ]; [ "b" ] ]
      ; [ [ "a"; "d" ]; [ "c" ] ]
      ; [ [ "a"; "c" ]; [ "b" ] ]
      ; [ [ "a"; "c" ]; [ "d" ] ]
      ; [ [ "a"; "b" ]; [ "c" ] ]
      ; [ [ "a"; "b" ]; [ "d" ] ]
      ]);
  (* Problem 28 *)
  assert (
    Funcs.sort_list_length
      [ [ "a"; "b"; "c" ]
      ; [ "d"; "e" ]
      ; [ "f"; "g"; "h" ]
      ; [ "d"; "e" ]
      ; [ "i"; "j"; "k"; "l" ]
      ; [ "m"; "n" ]
      ; [ "o" ]
      ]
    = [ [ "o" ]
      ; [ "m"; "n" ]
      ; [ "d"; "e" ]
      ; [ "d"; "e" ]
      ; [ "f"; "g"; "h" ]
      ; [ "a"; "b"; "c" ]
      ; [ "i"; "j"; "k"; "l" ]
      ]);
  assert (
    Funcs.sort_list_frequency
      [ [ "a"; "b"; "c" ]
      ; [ "d"; "e" ]
      ; [ "f"; "g"; "h" ]
      ; [ "d"; "e" ]
      ; [ "i"; "j"; "k"; "l" ]
      ; [ "m"; "n" ]
      ; [ "o" ]
      ]
    = [ [ "i"; "j"; "k"; "l" ]
      ; [ "o" ]
      ; [ "f"; "g"; "h" ]
      ; [ "a"; "b"; "c" ]
      ; [ "m"; "n" ]
      ; [ "d"; "e" ]
      ; [ "d"; "e" ]
      ]);
  (* Problem 31 *)
  assert (not (Funcs.is_prime 1));
  assert (Funcs.is_prime 7);
  assert (not (Funcs.is_prime 12));
  (* Problem 32 *)
  assert (Funcs.gcd 13 27 = 1);
  assert (Funcs.gcd 20536 7826 = 2);
  (* Problem 33 *)
  assert (Funcs.coprime 13 27);
  assert (not (Funcs.coprime 20536 7826));
  (* Problem 34 *)
  assert (Funcs.phi 10 = 4);
  assert (Funcs.phi 13 = 12);
  (* Problem 35 *)
  assert (Funcs.factors 315 = [ 3; 3; 5; 7 ]);
  (* Problem 36 *)
  assert (Funcs.factors_and_qtd 315 = [ 3, 2; 5, 1; 7, 1 ]);
  (* Problem 37 *)
  assert (Funcs.phi_improved 10 = 4);
  assert (Funcs.phi_improved 13 = 12);
  (* Problem 38 *)
  (*Just measuring the time between 2 algorithms*)
  (* Problem 39 *)
  assert (List.length @@ Funcs.all_primes 2 7920 = 1000);
  (* Problem 40 *)
  assert (Funcs.goldbach 28 = (5, 23));
  (* Problem 41 *)
  assert (
    Funcs.goldbach_list 9 20
    = [ 10, (3, 7); 12, (5, 7); 14, (3, 11); 16, (3, 13); 18, (5, 13); 20, (3, 17) ]);
  assert (
    Funcs.goldbach_limit 1 2000 50
    = [ 992, (73, 919); 1382, (61, 1321); 1856, (67, 1789); 1928, (61, 1867) ]);
  (* Problem 46 *)
  assert (
    Funcs.table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")))
    = [ true, true, true; true, false, true; false, true, false; false, false, false ]);
  (* Problem 48 *)
  assert (
    Funcs.table [ "a"; "b" ] (And (Var "a", Or (Var "a", Var "b")))
    = [ [ "a", true; "b", true ], true
      ; [ "a", true; "b", false ], true
      ; [ "a", false; "b", true ], false
      ; [ "a", false; "b", false ], false
      ]);
  (* Problem 49 *)
  assert (Funcs.gray 1 = [ "0"; "1" ]);
  assert (Funcs.gray 2 = [ "00"; "01"; "11"; "10" ]);
  assert (Funcs.gray 3 = [ "000"; "001"; "011"; "010"; "110"; "111"; "101"; "100" ]);
  (* Problem 50 *)
  assert (
    Funcs.huffman [ "a", 45; "b", 13; "c", 12; "d", 16; "e", 9; "f", 5 ]
    = [ "a", "0"; "b", "101"; "c", "100"; "d", "111"; "e", "1101"; "f", "1100" ])
in
main ()
