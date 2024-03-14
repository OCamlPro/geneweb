let prefix =
  try Sys.getenv "GWPREFIX"
  with Not_found -> "/usr"
