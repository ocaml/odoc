(** Main module of this test. See [run.t] *)

module X = Test_x
module X_out = Test_x.Out
module X_in = Test_x.In
module Y = Test_y
module Y_out = Test_y.Out
module Y_in = Test_y.In

type t = Test_x.t (* Through hidden path *)

type u = Y.t (* Through canonical path *)
