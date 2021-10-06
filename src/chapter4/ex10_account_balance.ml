(* Exercise: account balance [★★★]

   Write a function which, given a list of numbers representing debits, deducts them from an account balance,
   and finally returns the remaining amount in the balance.
   Write three versions: fold_left, fold_right, and a direct recursive implementation. *)

let account_balance (current_balance : int) (debits : int list) =
  List.fold_left (fun acc debit -> acc - debit) current_balance debits

let account_balance' (current_balance : int) (debits : int list) =
  List.fold_right (fun debit acc -> acc - debit) debits current_balance

let rec account_balance'' (current_balance : int) (debits : int list) =
  match debits with
  | [] -> current_balance
  | head :: tail -> account_balance'' (current_balance - head) tail

let test () =
  let current_balance = 0 in
  let debits = [ 1; 2; 3 ] in
  assert (account_balance current_balance debits = -6);
  assert (account_balance' current_balance debits = -6);
  assert (account_balance'' current_balance debits = -6)
