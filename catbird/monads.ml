(* ERROR AND STATE MONAD *)
(* CORE HAS SOME BUILT IN MONAD FUNCTOR OR SOMETHING BUT I CAN'T BE BOTHERED
 * TO FIGURE THAT OUT THANK YOU. *)
(******************************************************************************)

(* ERROR MONAD. THIS ONE IS SIMPLE. *)
module Or_failure = struct

  type ('a, 'e) t =
    | Success of 'a
    | Failure of 'e

  (* MONAD IS A SUCCESS, BUT DON'T LET THAT GET TO ITS HEAD *)
  let success (a : 'a) : ('a, 'e) t = Success a

  (* MONAD IS A FAILURE, BUT DON'T BREAK IT'S HEART *)
  let failure (e : 'e) : ('a, 'e) t = Failure e

  (* ERROR MONAD BIND. *)
  let (>>) (t : ('a, 'e) t) (f : ('a -> ('b, 'e) t)) : ('b, 'e) t =
    match t with
    | Success a -> f a
    | Failure e -> failure e

end

module State = struct

  type ('a, 's) t = 's -> ('a * 's)

  let state (a : 'a) : ('a, 's) t = fun s -> (a, s)

  let run_state (t : ('a, 's) t) (s : 's) : ('a * 's) = t s 

  (* STATE MONAD BIND. *)
  let (>>) (t : ('a, 's) t) (f : ('a -> ('b, 's) t)) : ('b, 's) t =
    fun s -> let (a, s') = run_state t s in 
             run_state (f a) s'

end
