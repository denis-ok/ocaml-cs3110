(* Exercise: cards [★★]

   Define a variant type suit that represents the four suits, ♣ ♦ ♥ ♠, in a standard 52-card deck.
   All the constructors of your type should be constant.

   Define a type rank that represents the possible ranks of a card: 2, 3, …, 10, Jack, Queen, King, or Ace.
   There are many possible solutions; you are free to choose whatever works for you.
   One is to make rank be a synonym of int, and to assume that Jack=11, Queen=12, King=13, and Ace=1 or 14.
   Another is to use variants.

   Define a type card that represents the suit and rank of a single card. Make it a record with two fields.

   Define a few values of type card:
   the Ace of Clubs, the Queen of Hearts, the Two of Diamonds, the Seven of Spades. *)

type suit = Spade | Heart | Diamond | Club

type rank =
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace

type card = { rank : rank; suit : suit }

let aceOfClubs = { rank = Ace; suit = Club }

let queenOfHearts = { rank = Queen; suit = Heart }

let twoOfDiamonds = { rank = Two; suit = Diamond }

let sevenOfSpades = { rank = Seven; suit = Spade }
