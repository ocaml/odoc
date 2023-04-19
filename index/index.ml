let () =
  let odoc_directory = Sys.argv.(1) and db_filename = Sys.argv.(2) in
  Index_lib.main ~odoc_directory ~db_filename (module Storage_marshal)
