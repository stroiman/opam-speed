open Speed
open Assertions
open Dsl.Effect.Simple
open Utils
open Null_formatter

let pending _ = ()
let lwt_pass () = Lwt.return ();;

run_root (fun _ ->
  context "*** Lwt runner" (fun _ ->
    test "It runs all tests" (fun _ ->
      let test1_executed = ref false in
      let test2_executed = ref false in
      let test =
        Domain.LwtDomain.(
          make_suite ()
          |> add_example "1" (fun _ ->
            test1_executed := true;
            Lwt.return ()
          )
          |> add_example "2" (fun _ ->
            test2_executed := true;
            Lwt.return ()
          )
        )
      in
      Runner.LwtRunner.run_suite_wait ~fmt test |> ignore;
      !test1_executed |> should be_true;
      !test2_executed |> should be_true
    );

    test "It outputs in the right sequence - even if completed out of order"
      (fun _ ->
         let s = ref "" in
         let fmt = make_ref_string_printer s in
         let p1, r1 = Lwt.wait () in
         let p2, r2 = Lwt.wait () in
         let p3, r3 = Lwt.wait () in
         let p4, r4 = Lwt.wait () in
         let p5, r5 = Lwt.wait () in
         let suite =
           Domain.LwtDomain.(
             make "Root"
             |> add_context "Child1" (fun ctx ->
               ctx
               |> add_example "Ex 1" (fun _ -> p1)
               |> add_example "Ex 2" (fun _ -> p2)
             )
             |> add_context "Child2" (fun ctx ->
               ctx
               |> add_example "Ex 3" (fun _ -> p3)
               |> add_example "Ex 4" (fun _ -> p4)
               |> add_example "Ex 5" (fun _ -> p5)
             )
           )
         in

         let suite_p = Runner.LwtRunner.run_suite_return ~fmt suite in

         Lwt.wakeup r5 ();
         Lwt.wakeup r4 ();
         Lwt.wakeup r3 ();
         Lwt.wakeup r2 ();
         Lwt.wakeup r1 ();
         Runner.LwtRunner.wait suite_p |> ignore;
         let expected =
           "• Root\n\
           \  • Child1\n\
           \    ✔ Ex 1\n\
           \    ✔ Ex 2\n\
           \  • Child2\n\
           \    ✔ Ex 3\n\
           \    ✔ Ex 4\n\
           \    ✔ Ex 5"
         in
         !s |> should (equal_string expected)
    );

    it "Should write the output as tests complete, but not too early" (fun _ ->
      let s = ref "" in
      let fmt = make_string_ref_formatter s in
      let p1, r1 = Lwt.wait () in
      let p2, r2 = Lwt.wait () in
      let p3, r3 = Lwt.wait () in
      let e1_started = ref false in
      let e2_started = ref false in
      let e3_started = ref false in
      let suite =
        Domain.LwtDomain.(
          make_suite ()
          |> add_context "ctx" (fun ctx ->
            ctx
            |> add_example "1" (fun _ ->
              e1_started := true;
              p1
            )
            |> add_example "2" (fun _ ->
              e2_started := true;
              p2
            )
            |> add_example "3" (fun _ ->
              e3_started := true;
              p3
            )
          )
        )
      in
      let suite_p = Runner.LwtRunner.run_suite_return ~fmt suite in
      !e1_started |> should ~name:"E1 started" be_true;
      !e2_started |> should ~name:"E2 started" be_true;
      !e3_started |> should ~name:"E3 started" be_true;
      Lwt.wakeup r3 ();
      Format.fprintf fmt "@<10000>%s" "";
      !s |> should (equal_string "• ctx");
      Lwt.wakeup r1 ();
      Format.fprintf fmt "@<10000>%s" "";
      !s |> should (equal_string "• ctx\n  ✔ 1");
      Lwt.wakeup r2 ();
      !s |> should (equal_string "• ctx\n  ✔ 1\n  ✔ 2\n  ✔ 3");
      Runner.LwtRunner.wait suite_p |> ignore
    )
  )
)
