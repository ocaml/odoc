(** Input for ocaml.org, which builds documentation one package per job.

    Each job runs the voodoo driver on a package prepared by [voodoo-prep]: a
    directory [prep/universes/<universe>/<pkg>/<version>] holding the files the
    package installed. Dependencies are not in [prep]; they are installed in the
    job's switch, and their [.odoc] files are those earlier jobs wrote into the
    odoc directory. *)

type pkg
(** A prepared package. *)

val find_pkg : string -> blessed:bool -> pkg option
(** Find the prepared package of the given name. [blessed] marks the version
    ocaml.org shows by default; it decides the identifiers, see {!of_voodoo}. *)

val of_voodoo : pkg -> Packages.t * Installed.t
(** Read the package's [META] files, objects and documentation from the prep
    directory. Identifiers start with [p/<pkg>/<version>] for a blessed package
    and [u/<universe>/<pkg>/<version>] otherwise. *)

val occurrence_file_of_pkg : pkg -> Fpath.t
(** Where the package's occurrence counts go, relative to the odocl directory.
*)
