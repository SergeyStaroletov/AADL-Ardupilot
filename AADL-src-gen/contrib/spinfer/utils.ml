module type UnorderedType = sig
    type t
    val equal: t -> t -> bool
end

module type UnorderedSet = sig
    type elt
    type t
    val empty: t
    val is_empty: t -> bool
    val mem: elt -> t -> bool
    val add: elt -> t -> t
    val singleton: elt -> t
    val remove: elt -> t -> t
    val union: t -> t -> t
    val inter: t -> t -> t
    val diff: t -> t -> t
    val equal: t -> t -> bool
    val subset: t -> t -> bool
    val cardinal: t -> int
    val find: elt -> t -> elt
    val exists: (elt -> bool) -> t -> bool
    val filter: (elt -> bool) -> t -> t
    val of_list: elt list -> t
    val elements: t -> elt list
end

module UnorderedSetMake(Unord: UnorderedType)
    : (UnorderedSet with type elt = Unord.t) =
struct
    type elt = Unord.t
    type t = elt list

    let mem (elt: elt) (set: t) : bool =
        try
            ignore (List.find (Unord.equal elt) set);
            true
        with Not_found -> false

    let empty = []

    let is_empty set = if set = [] then true else false

    let cardinal = List.length

    let add elt set = if mem elt set then set else elt::set

    let singleton elt = [elt]

    let remove elt set =
        let new_set = List.fold_left (fun accu test_elt ->
            if Unord.equal test_elt elt then accu else test_elt::accu
        ) empty set
        in
        (* Ensure equality if no change *)
        if (cardinal set) = (cardinal new_set) then set else new_set

    let union set1 set2 =
        List.fold_left (fun accu elt ->
            if mem elt set1
            then accu
            else elt::accu
        ) set1 set2

    let inter set1 set2 =
        List.fold_left (fun accu elt ->
            if mem elt set1
            then elt::accu
            else accu
        ) empty set2

    let diff set1 set2 =
        List.fold_left (fun accu elt ->
            if mem elt set2
            then accu
            else elt::accu
        ) empty set1

    let equal set1 set2 =
        ((cardinal set1) = (cardinal set2)) &&
        (List.for_all (fun elt -> mem elt set2) set1)

    let subset set1 set2 = List.for_all (fun elt -> mem elt set2) set1

    let find elt = List.find (fun test_elt -> Unord.equal elt test_elt)

    let exists = List.exists
    let filter = List.filter

    let of_list l =
        List.fold_left (fun accu elt ->
            if mem elt accu
            then accu
            else elt::accu
        ) [] l

    let elements set = set
end

let assoc_left (l: ('a * 'b) list) : 'a list = fst (List.split l)
let assoc_right (l: ('a * 'b) list) : 'b list = snd (List.split l)

let findi_opt (f: int -> 'a -> bool) (l: 'a list) : (int * 'a) option =
    let found_elt, _ = List.fold_left (fun (found_elt, index) elt ->
        match found_elt with
        | Some(_) -> (found_elt, index + 1)
        | None ->
            if f index elt
            then (Some((index, elt)), index + 1)
            else (None, index + 1)
    ) (None, 0) l
    in
    found_elt

let findalli (f: int -> 'a -> bool) (l: 'a list) : (int * 'a) list =
    let found_elts, _ = List.fold_left (fun (found_elts, index) elt ->
        if f index elt
        then ((index, elt)::found_elts, index + 1)
        else (found_elts, index + 1)
    ) ([], 0) l
    in
    List.rev found_elts


let filteri (f: int -> 'a -> bool) (l: 'a list) : 'a list =
    let li = List.mapi (fun i elt -> (i, elt)) l in
    let filtered_li = List.filter (fun (i, elt) -> f i elt) li in
    assoc_right filtered_li


module IntSet = Set.Make(struct
    type t = int
    let compare = compare
end)

module StringSet = Set.Make(String)

let memoizer htbl key f =
    if Hashtbl.mem htbl key
    then
        Hashtbl.find htbl key
    else begin
        let res = f () in
        Hashtbl.replace htbl key res;
        res
    end

let stack_to_list stack =
    let res = ref [] in
    Stack.iter (fun elt -> res := elt::!res) stack;
    List.rev !res


let htbl_exists (f: 'a -> 'b -> bool) (htbl: ('a, 'b) Hashtbl.t) : bool =
    let exists = ref false in
    begin try
        Hashtbl.iter (fun key value ->
            if f key value then raise Exit
        ) htbl
    with Exit -> exists := true
    end;
    !exists


let htbl_to_list (htbl: ('a, 'b) Hashtbl.t) : ('a * 'b) list =
    Hashtbl.fold (fun key value l ->
        (key, value)::l
    ) htbl []


(* FIXME: To be removed in 4.06 *)
let list_init (index: int) (f: int -> 'a) : ('a list) =
    let rec iter_index i accu =
        if i = index
        then
            List.rev accu
        else
            iter_index (i+1) ((f i)::accu)
    in
    iter_index 0 []


(* FIXME: To be removed in 4.08 *)
let option_get (value: 'a option) : 'a =
    match value with
    | Some(v) -> v
    | None -> raise (Invalid_argument("Option is None"))


let list_is_subset_of (small_l: 'a list) (big_l: 'a list) : bool =
    if List.length small_l > List.length big_l
    then false
    else
        let rec iter_on small big =
            match small, big with
            | [], _ -> true
            | _, [] -> false
            | head_s::tail_s, head_b::tail_b ->
                if head_s = head_b
                then iter_on tail_s tail_b
                else iter_on small tail_b
        in
        iter_on (List.sort compare small_l) (List.sort compare big_l)


let print_progress total index =
    let index = ((index+1)*50)/total in
    let get_char = function
    | i when (i = index) -> '>'
    | i when (i < index) -> '='
    | _ -> ' '
    in

    let to_print = "[" ^ (String.init 50 get_char) ^ "]" in

    Printf.eprintf "\r%s%!" to_print


(** Pair elements between two list. Returns the list of pairs represented
by their indexes in the original lists *)
let bilist_pairs (f: int * 'a -> int * 'b -> bool)
                 (comp: int * 'b -> int * 'b -> int)
                 (l1: 'a list) (l2: 'b list)
                 : (int * int) list =
    let rec iter_on_list (i: int) (l: 'a list) (pairs: (int * int) list)
                         : (int * int) list =
        match l with
        | [] -> List.rev pairs
        | head::tail ->
            let new_f j b : bool =
                (* elt from l2 is not already picked *)
                not (List.mem j (assoc_right pairs))
                &&
                (* elts validate predicate f *)
                f (i, head) (j, b)
            in
            match findalli new_f l2 with
            | [] ->
                iter_on_list (i+1) tail pairs
            | all_elts ->
                let index_l2, _ = List.hd (List.sort comp all_elts) in
                iter_on_list (i+1) tail ((i, index_l2)::pairs)
    in
    iter_on_list 0 l1 []


let list_index (l: 'a list) : (int * 'a) list =
    List.mapi (fun i elt -> (i, elt)) l

let list_unindex (l: (int * 'a) list) : 'a list =
    assoc_right l


let rec cart_prod (l1: 'a list) (l2: 'b list) (f: 'a -> 'b -> 'c) : 'c list =
    List.flatten (
        List.map (fun e1 ->
            List.map (f e1) l2
        ) l1
    )

let common_prefix (s1: string) (s2: string) =
    let max_len = min (String.length s1) (String.length s2) in
    let rec comp (i: int) =
        if (i == max_len || s1.[i] <> s2.[i])
        then String.sub s1 0 i
        else comp (i+1)
    in
    comp 0


let partition_list (l: 'a list) (equal: 'a -> 'a -> bool) : 'a list list =
    let rec find_corresponding accu_group l =
        match l with
        | [] -> accu_group
        | re::tail ->
            let found = ref false in
            let accu_group = List.fold_left (fun accu_group group ->
                match group with
                | (head_member)::_ when equal re head_member ->
                   found := true;
                   (re::group)::accu_group
                | _ -> group::accu_group
            ) [] accu_group
            in
            if !found
            then find_corresponding accu_group tail
            else find_corresponding ([re]::accu_group) tail
    in
    find_corresponding [] l


let add_opt_to_list (opt: 'a option) (l: 'a list) : 'a list =
    match opt with
    | Some(a) -> a::l
    | None -> l
