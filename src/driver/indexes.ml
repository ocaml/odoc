let package fmt (pkg : Packages.t) =
  Format.fprintf fmt "{0 Package %s}\n" pkg.pkgname.p_name;
  Format.fprintf fmt "{1 Libraries}\n";
  List.iter
    (fun (lib : Packages.libty) ->
      Format.fprintf fmt "{2 %s}\n" lib.archive_name)
    pkg.libraries
