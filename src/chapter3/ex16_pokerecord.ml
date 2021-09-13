(* Exercise: pokerecord [★★]

   Here is a variant that represents a few Pokémon types:

   type poketype = Normal | Fire | Water

   Define the type pokemon to be a record with fields name (a string), hp (an integer), and ptype (a poketype).
   Create a record named charizard of type pokemon that represents a Pokémon with 78 HP and Fire type.
   Create a record named squirtle of type pokemon that represents a Pokémon with 44 HP and Water type. *)

type poketype = Normal | Fire | Water

type pokemon = { name : string; hp : int; ptype : poketype }

let charizard = { name = "charizard"; hp = 78; ptype = Fire }

let squirtle = { name = "squirtle"; hp = 44; ptype = Water }
