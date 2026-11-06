(** What the driver builds: units, grouped into libraries and packages.

    A {e unit} is one artifact odoc compiles: a module interface, a module
    implementation, a page or an asset. A {e library} groups the units of its
    modules. A {e package} groups its libraries and its pages, and is the thing
    the driver builds in one go. See {!Odoc_units_of} for how these are made
    from what is installed, and {!Compile} and {!Build} for what is done with
    them. *)

(** {1 Units} *)

type +'a t = {
  parent_id : Odoc.Id.t;
      (** Decides the unit's identifier, and so how references to it resolve and
          where its HTML goes. *)
  input_file : Fpath.t;
      (** The [.cmti], [.cmt], [.mld], [.md] or asset file. *)
  input_copy : Fpath.t option;
      (** For the interface of a virtual library: a place to copy the [.cmti]
          to, next to the [.odoc] file. A later run documenting an
          implementation can then find it. *)
  odoc_file : Fpath.t;  (** Where [odoc compile] writes. *)
  odocl_file : Fpath.t;  (** Where [odoc link] writes. *)
  enable_warnings : bool;  (** Report odoc's warnings for this unit. *)
  to_output : bool;  (** Link and render this unit. *)
  kind : 'a;
}
(** One unit. What all the units of a library or of a package share lives in
    {!lib} and {!pkg} instead. *)

type intf_extra = {
  hidden : bool;  (** Not shown to readers; compiled but not linked. *)
  hash : string;  (** The digest of the interface. *)
  deps : (string * Digest.t) list;
      (** The modules it depends on, which must be compiled first. *)
}

and intf = [ `Intf of intf_extra ]

val hash : intf t -> string
(** The digest of an interface. *)

val deps : intf t -> Digest.t list
(** The digests of the interfaces an interface depends on. *)

type impl_extra = {
  src_id : Odoc.Id.t;  (** The identifier of the rendered source page. *)
  src_path : Fpath.t;  (** The source file. *)
}

type impl = [ `Impl of impl_extra ]
type mld = [ `Mld ]
type md = [ `Md ]
type asset = [ `Asset ]
type any = [ impl | intf | mld | asset | md ] t

type module_unit = [ intf | impl ] t
(** An interface or an implementation: the units of a library. *)

type page = [ mld | md | asset ] t
(** A page or an asset: the units of a package's documentation. *)

(** {1 Libraries and packages} *)

type scope = {
  page_roots : (string * Fpath.t) list;
      (** The packages whose pages may be referred to, with the directory of
          their [.odoc] files: the [-P] arguments. *)
  lib_roots : (string * Fpath.t) list;
      (** The libraries whose modules may be referred to, likewise: the [-L]
          arguments. *)
}
(** What a unit may refer to at link time. Every unit of a package, the pages
    the driver writes included, is linked with the package's scope and nothing
    else. *)

type lib = {
  lib_name : string;
  pkgname : string option;
      (** The package that provides it. It is the tag the library's warnings
          carry, so that [odoc link] can report them for the packages the run
          was asked to document and for no others. *)
  lib_includes : (string * Fpath.t) list;
      (** The library and its transitive dependencies: the libraries it requires
          and, in turn, what those require. Each is a library name and the
          directory of its [.odoc] files, passed to [odoc] as [-L name:dir]. It
          is what the compiler could see when it built the library.

          The library's units are compiled ([odoc compile], [odoc compile-impl])
          with it. [odoc] finds the [.odoc] files of their imports in it, and
          records its names in each unit. They are not linked with it but with
          the package's {!type-scope}, which holds every library of this list
          that has been documented, since it holds each of the package's
          libraries with its transitive dependencies. [odoc link] finds there
          the libraries each unit recorded.

          The driver also orders a package's libraries by it, and finds a
          package's {!pkg.deps} from it. *)
  units : module_unit list;
  page : mld t option;
      (** The page the driver writes to list the library's modules. [None] when
          the driver writes no pages for this package. It names each module with
          its library, [{!modules: /lib/M}], so it shows this library's modules
          and not a same-named module of another library. *)
}
(** A library and its modules. *)

type index = {
  index_file : Fpath.t;  (** The [.odoc-index] file. *)
  sidebar_file : Fpath.t;  (** The [.odoc-sidebar] file derived from it. *)
  html_dir : Fpath.t;
      (** The package's directory in the HTML output, where the search database
          and the JSON sidebar go. *)
}
(** A package's index, built from the [.odocl] files of its units. *)

type pkg = {
  pkgname : string option;
      (** [None] for the top-level index, which belongs to no package. *)
  scope : scope;
  index : index option;  (** [None] for the top-level index. *)
  libs : lib list;
  pages : page list;  (** Including the landing pages the driver writes. *)
  deps : string list;
      (** The packages it depends on: those that provide a library in the
          {!lib.lib_includes} of one of its libraries, other than itself. *)
  doc_deps : string list;
      (** The packages its documentation needs beyond its dependencies
          ({!Packages.t.doc_deps}). *)
}
(** A package, the thing the driver builds in one go. *)

val is_output : any -> bool
(** Linked and rendered: {!t.to_output}, and not a hidden interface. A hidden
    interface is compiled, but neither linked nor shown to readers. *)

val all_units : pkg -> any list
(** The package's pages, and the units of its libraries. *)

val index_inputs : pkg -> Fpath.t list
(** The [.odocl] files a package's index is built from: those of its units that
    are output. *)

(** {1 Where things go}

    Identifiers follow one layout and files on disk another.

    {e Identifiers} put a package's pages under the package's name, and a
    library's modules under [<pkg>/<lib>]. They decide the URLs.

    {e Files} mirror the switch. A library's [.odoc] files are written at the
    path of its object directory, [lib/<findlib dir>]. Libraries that share a
    directory in the switch therefore share one here, just as one [-I] reached
    them all for the compiler. A package's pages are written below [doc/<pkg>].
    Every location is a function of a name, so a run finds what earlier runs
    built with no record of them. The same paths hold below the odocl directory.
*)

val pkg_dir : Packages.t -> Fpath.t
(** The identifier of the package's top-level page, and the root of its HTML. *)

val doc_dir : Packages.t -> Fpath.t
(** The identifier below which the package's pages live. *)

val lib_dir : Packages.t -> Packages.libty -> Fpath.t
(** The identifier of a library's modules. *)

val src_dir : Packages.t -> Fpath.t
(** The identifier below which rendered sources live. *)

val src_lib_dir : Packages.t -> Packages.libty -> Fpath.t
(** The identifier below which a library's rendered sources live. *)

val lib_obj_dir : Packages.libty -> Fpath.t
(** Where a library's [.odoc] files go, relative to the odoc directory. *)

val pages_dir : Packages.t -> Fpath.t
(** Where a package's pages go, relative to the odoc directory. *)

val page_obj_dir : Packages.t -> Fpath.t -> Fpath.t
(** [page_obj_dir pkg id] is where a page with identifier [id] goes: the
    position of [id] below {!doc_dir}, reproduced below {!pages_dir}. *)

(** {1 Directories} *)

type dirs = {
  odoc_dir : Fpath.t;  (** [.odoc] files. *)
  odocl_dir : Fpath.t;  (** [.odocl] files. *)
  index_dir : Fpath.t;  (** Index and sidebar files. *)
  mld_dir : Fpath.t;  (** The pages the driver writes itself. *)
}
(** Where intermediate files go. *)
