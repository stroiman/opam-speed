open Speed
open Speed.Dsl
open! Utils;;

Dsl.register [ test "One failing test should result in fail count of 1" (fun _ -> ()) ]
