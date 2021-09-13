(* Exercise: pokefun [★★★] *)
(* Write a function max_hp : pokemon list -> pokemon option that, given a list of pokemon, finds the Pokémon with the highest HP. *)

type pokemon = Ex16_pokerecord.pokemon

let max_hp (pokemones : pokemon list) : pokemon option =
  let rec find_max_hp (max_hp_poke : pokemon) (rest : pokemon list) =
    match rest with
    | [] -> Some max_hp_poke
    | poke :: rest ->
        if poke.hp > max_hp_poke.hp then find_max_hp poke rest
        else find_max_hp max_hp_poke rest
  in
  match pokemones with
  | [] -> None
  | poke :: tail -> find_max_hp poke tail

module Test = struct
  open OUnit2

  let p1 : pokemon = { name = "Lala"; hp = 1; ptype = Fire }

  let p2 : pokemon = { p1 with hp = 2 }

  let p3 : pokemon = { p1 with hp = 3 }

  let make_test test_name expected_output input =
    test_name >:: fun _ -> assert_equal expected_output (max_hp input)

  let tests_get_fifth_element =
    "tests for max_hp function"
    >::: [
           make_test "empty list" None [];
           make_test "one pokemon" (Some p1) [ p1 ];
           make_test "two pokemons" (Some p2) [ p2; p1 ];
           make_test "three pokemons" (Some p3) [ p2; p1; p3 ];
         ]

  let test () = run_test_tt_main tests_get_fifth_element
end
