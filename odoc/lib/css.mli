(** Copies odoc's default CSS to a specified location. *)

(* TODO: make the first parameter a [Fs.Directory.t] *)
val copy_default_css : etc_dir:string -> output_dir:Fs.Directory.t -> unit
