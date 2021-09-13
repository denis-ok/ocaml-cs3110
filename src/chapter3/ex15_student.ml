(* Exercise: student [★★]
   Assume the following type definition:
   type student = {first_name : string; last_name : string; gpa : float}
   Give OCaml expressions that have the following types:

   student
   student -> string * string (a function that extracts the student’s name)
   string -> string -> float -> student (a function that creates a student record) *)

type student = { first_name : string; last_name : string; gpa : float }

let student = { first_name = "John"; last_name = "Brown"; gpa = 3.5 }

let get_name (student : student) = (student.first_name, student.last_name)

let make_student first_name last_name gpa = { first_name; last_name; gpa }
