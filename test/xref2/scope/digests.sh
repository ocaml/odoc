# Helpers that read the import table of a compiled file with ocamlobjinfo.
# For a .cmt or .cmti they read the "Cmt interfaces imported" table, which
# is the one odoc reads (cmt_imports); for other files, "Interfaces
# imported". Each row of a table is a digest, a tab and a name. A digest of
# dashes means no digest. The helpers never print a digest, as digests
# change between compiler versions.

table () {
  case "$1" in
    *.cmt|*.cmti) h="Cmt interfaces imported:" ;;
    *) h="Interfaces imported:" ;;
  esac
  ocamlobjinfo "$1" | awk -v h="$h" '$0 == h { t = 1; next } t && !/^\t/ { t = 0 } t'
}

# [entry FILE NAME]: "digest", "no digest" or "no entry" for NAME in FILE.
entry () {
  table "$1" | awk -F'\t' -v n="$2" '$3 == n { f = 1; print ($2 ~ /^-+$/ ? "no digest" : "digest") } END { if (!f) print "no entry" }'
}

# [digest FILE NAME]: the digest FILE records for NAME, if any.
digest () {
  table "$1" | awk -F'\t' -v n="$2" '$3 == n && $2 !~ /^-+$/ { print $2 }'
}

# [iface FILE]: the interface digest a .cmt records for its own unit.
iface () {
  ocamlobjinfo "$1" | sed -n 's/^cmt interface digest: *//p'
}

# [same A B]: "same" or "different"; "missing" if either is empty.
same () {
  if [ -z "$1" ] || [ -z "$2" ]; then echo missing
  elif [ "$1" = "$2" ]; then echo same
  else echo different; fi
}

# [from FILE NAME CMI...]: each CMI whose own digest FILE records for NAME.
from () {
  f=$1; n=$2; shift 2; d=$(digest "$f" "$n"); r=none
  for c in "$@"; do
    if [ -n "$d" ] && [ "$d" = "$(digest "$c" "$n")" ]; then echo "$c"; r=; fi
  done
  if [ -n "$r" ]; then echo "$r"; fi
}
