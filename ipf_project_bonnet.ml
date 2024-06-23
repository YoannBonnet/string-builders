(* Programmation fonctionnelle - Projet 2022 : String builders*)

(* Question 1 *)
type string_builder =
  | Empty
  | Leaf of int * string
  | Node of int * string_builder * string_builder;;

let essai = Node(5,
                  Leaf(1, "H"),
                  Node(4,
                        Leaf(1, "E"),
                        Leaf(3, "LLO")
                      )
                );;

(* This function creates a string builder from a string passed in argument *)
let word x = Leaf (String.length x, x);;

(* This function returns the length of a string builder *)
let string_builder_length = function
  | Empty -> 0;
  | Leaf(n, x) -> n;
  | Node(n, x1, x2) -> n;;

(* This function creates a string builder resulting from the concatenation of two string builders passed in argument *)
let concat sb1 sb 2 = match (sb1, sb2) with
  | (Empty, sb2) -> sb2;
  | (sb1, Empty) -> sb1;
  | (sb1, sb2) -> Node(string_builder_length sb1 + string_builder_length sb2, sb1, sb2);;

(* Question 2 *)
(* This function returns the nth caracter of a string builder passed in argument *)
(* If we have an empty string builder, the function will return an error *)
(* If we only have a leaf, we'll search the nth caracter of the string in the leaf *)
(* Then, if the number n passed in argument is lower than the left string builder, we search trough the left sb, 
else we search through the right string builder at the right position, that is to say (n-length of the left sb) *)

let rec char_at i sb = match sb with
  | Empty -> failwith "String builder vide";
  | Leaf(_,x) -> String.get x i;
  | Node(_,sb1,sb2) -> let n1 = string_builder_length sb1 in
      if i < n1 then char_at i sb1
      else char_at (i-n1) sb2;;

(* Question 3 *)
(* In the case of a leaf, it's quite simple : we simply use the String.sub function and we return the corresponding leaf *)
(* In the case of a node, there are three cases: either the factor sought is a factor of the left part sb1, or of the right part sb2,
or it is the result of the concatenation of a suffix of sb1 and a prefix of sb2 *)
let rec sub_string i m sb = match sb with
  | Empty -> failwith "String builder vide";
  | Leaf(n, x) -> Leaf(m, String.sub x i m);
  | Node(n, sb1, sb2) when i+m <= string_builder_length sb1 -> sub_string i m sb1; (* if the length of the sought substring is lower than the lefth sb's length, we search though this one*)
  | Node(n, sb1, sb2) when i >= string_builder_length sb1 -> sub_string (i-string_builder_length sb1) m sb2; (* If the index of the first caracter is greater than the length of the left sb's length, we search the sub string through the right string builder *)
  | Node(n, sb1, sb2) -> let n1 = string_builder_length sb1 in concat (sub_string i (n1-i) sb1) (sub_string 0 (m-n1+i) sb2) (* in the most general case, we search the begenning of the substring we want through the end of the left sb and the beginning of the right sb *);;

(* Question 4 *)
(* This function returns the cost of a string builder according to the definition given in the statement *)
(* To implement the function, we use an auxiliary function which take as arguments an accumulator and the depth of the tree*)
(* It will return the accumulator *)
(* In the most simple case, that is to say a leaf, we call the function by increasing the depth by 1 and increasing the accumulator by the length of the leaf multiplied by its depth, according to the statement *)
(* In the most general case, we apply the auxiliary function to the left and right string builder of the node by increasing the depth by 1 *)
let cost sb = 
  let rec aux acc prof s = match s with
    | Empty -> acc;
    | Leaf(n, x) -> aux (acc + n*prof) (prof + 1) Empty;
    | Node(_, s1, s2) -> aux acc (prof+1) s1 + aux acc (prof +1) s2;
  in aux 0 0 sb;;

(* Question 5 *)
(*Generate a random character between 'a' and 'z' by converting the ASCII code into the corresponding caracter *)
(* In order to have a lowercase caracter between 'a' and 'z', the ASCII code should be between 97 and 123 *)
let rand_chr () = (Char.chr (97 + (Random.int 26)));; 

(*Generate a random vowel *)
let rec rand_voy () = let chr = (rand_chr ()) in match chr with
  | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' ->  chr;
  | _ -> rand_voy ();;

(*Generate a random consonant *)
let rec rand_con () = let chr = (rand_chr ()) in match chr with
  | 'a' | 'e' | 'i' | 'o' | 'u' | 'y' ->  rand_con ();
  | _ -> chr;;

(* Generate a random syllable, which is a consonant followed by a vowel *)
(* We use 'Char.escaped' function in order to convert char into string *)
let rec rand_convoy acc syll_number () = match syll_number with
  | 0 -> acc;
  | _ -> rand_convoy (acc ^ (Char.escaped (rand_con ())) ^ (Char.escaped(rand_voy()))) (syll_number - 1) ();;        

(* Generate a random short word between 1 and 3 syllables*)
let rand_word () = (rand_convoy "" (1 + (Random.int 2)) ());;

(* We finally generate a random tree using the random generating word function *)
(* A tree of a depth of 0 must be an empty tree *)
(* If the precised length is 1, we generate a random string and we return the corresponding leaf *)
(* If the precised length is i, concatenate a leaf with a random string of size i-1 *)
let rec random_string i = match i with
  | 0 -> Empty;
  | 1 -> let str = rand_word () in Leaf(String.length(str), str);
  | _ -> let str = random_string (i-1) in Node(i, word (rand_word()), str);;

(* Question 6 *)
(* This function returns the list of strings in the same order as the tree *)
(* If we have an empty tree, we simply return an empty list. *)
(* If we just have one leaf, we return the list with only the string contained in the leaf *)
(* In the most general case, we just apply the function recursively first in the left string builder, and then in the right one *)
let rec list_of_string sb = match sb with
  | Empty -> [];
  | Leaf(n,x) -> [x];
  | Node(n,sb1,sb2) -> list_of_string(sb1)@list_of_string(sb2);;

(* Question 7 *)
(* First step : we transform the tree into a list of its leaves *)
(* We use the previous function to obtain the list of all strings in the right order *)
(* Given this list, we could apply a map to this list in order to convert all the strings into leaves *)
let list_of_leaves sb = let list = list_of_string sb in 
  let f x = Leaf(String.length(x), x) in List.map f list;;

(* Second step : we look for the minimum concatenation cost through the list *)
(* We work with references because the values will change as we go along the tree *)
let min_cost_concat leaves_list =
  let min_index = ref 0 in (* At the beginning, the searched index is equal to zero *)
  let first_concat = concat (List.nth leaves_list 0) (List.nth leaves_list 1) in (* we concatenate the first two elements of the list *)
  let min_cost = ref (cost first_concat) in (* we use another reference to stock the value of the first cost concatenation *)

  (* Then, in order to find the minimum througn the whole list, we use an auxiliary function *)
  (* Argument 'i' gives us information about our position in the list *)
  let rec aux leaves_list i = match leaves_list with 
    | [] -> failwith "Empty list";
    | [sb] -> cost sb;
    | sb1::sb2::q -> let cost_concat = cost (concat sb1 sb2) in (* stock the value of the concatenation of the element pointed by the cursor and its successor in a variable *)
        if cost_concat < !min_cost then (min_cost := cost_concat; (* if the concatenation we just done costed less than the current minimum, we replace the minimum cost by this minimum *)
                                              min_index := i; (* we must also replace the index of the minimum *)
                                              aux (sb2::q) (i+1); (* we continue searching through the rest of the list *)
                                            )
        else aux (sb2::q) (i+1); (* if the concatenation we just done doesn't cost less, we continue searching through the rest of the list *)
  
  (* We apply this auxiliary function in our list of leaves by passing 0 as argument in order to visit all the list *)
  in ignore (aux leaves_list 0); (* we use the function ignore to not return the result of the auxiliary function because we only care about the minimum index and not the cost of the concatenation *)

  !min_index;; (* we return the minimum index found by the auxiliary function *)

(* Third step : knowing an index 'i', this function replace two successives elements by their concatenation *)
let rec replace_concat list i = match list with
  | [] -> failwith "Empty list";
  | [sb] -> failwith "Impossible concatenation because the list contains only one element";
  | sb1::sb2::q -> if i == 0 then let c = concat sb1 sb2 in c::q else let t = replace_concat (sb2::q) (i-1) in sb1::t;;

(* Fourth step : balance the string builder by applying the algorithm *)
let balance sb =
  let leaves_list = list_of_leaves sb in (* conversion of the string builder into a list of its leaves *)
  let rec aux leaves_list = match leaves_list with (* we apply recursively the algorithm while we have two elements in the leaves list *)
    | [] -> failwith "The string builder list is empty."
    | [sb] -> sb (* if we only have a leaf, the tree is already balanced and so we could return it *)
    | leaves_list -> let index_min = min_cost_concat leaves_list in (* we search the two successive elements whose concatenation has the lowest cost *)
        let concat_list = replace_concat leaves_list index_min in (* we replace the two elements previously found *)
        aux concat_list (* we repeat the algorithm recursively until we found a base case *)
  in aux leaves_list;; (* the algorithm is applied to the list of leaves we created at the beginning of the function *)

(* Question 8 *)
(* First, 'random_list_tree' creates a function that return a list of 'i' randomly generated trees *)
let rec random_list_tree i = match i with
  | 0 -> failwith "Please enter a nonzero value";
  | 1 -> [random_string (Random.int 10)];
  | i -> random_string (Random.int 10)::(random_list_tree (i-1));;

(* Then, 'balanced_random' creates the balanced trees from the list of initial trees *)
let balanced_random list = List.map balance list;;

(* Then, we implement different functions that return the min, the max, the mean and the median *)
let rec max_list l = match l with
  | [] -> failwith "Empty list";
  | [x] -> x;
  | t::q -> max t (max_list q);;

let rec min_list l = match l with
  | [] -> failwith "Empty list";
  | [x] -> x;
  | t::q -> min t (min_list q);;

let rec sum list = match list with
  | [] -> 0
  | h::t -> h + (sum t);;

let mean_list l = let s = sum l in s/(List.length l);;

(* Finally, we could implement the function that will compare the costs *)
let gain i = let random_list = random_list_tree i in 
      let random_list_balanced = balanced_random (random_list) in
      let cost_list = List.map cost random_list in
      let cost_list_balanced = List.map cost random_list_balanced in
      let min_cost = min_list cost_list in
      let min_cost_bal = min_list cost_list_balanced in
      let max_cost = max_list cost_list in
      let max_cost_bal = max_list cost_list_balanced in
      let mean_cost = mean_list cost_list in
      let mean_cost_bal = mean_list cost_list_balanced in 
      Printf.printf "Min, max and mean cost for the random trees : %d, %d, %d\n" min_cost max_cost mean_cost ;
      Printf.printf "Min, max and mean cost for the balanced random trees : %d, %d, %d\n" min_cost_bal max_cost_bal mean_cost_bal;;