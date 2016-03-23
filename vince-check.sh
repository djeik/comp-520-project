#!/bin/bash

# Checks programs against golitec

ssh mimi.cs.mcgill.ca 'mkdir -p goto-testing'

rsync -r programs mimi.cs.mcgill.ca:goto-testing/

ssh mimi.cs.mcgill.ca "bash" <<'EOF' | cut -c14-
#!/bin/bash

set -e

GOLITEC="/home/course/cs520/golitec"
OUT="goto-testing/out"

find goto-testing/programs/valid -type f |
while read line ; do
    "$GOLITEC" parse "$line" > "$OUT" 2>&1 || cat "$OUT"
done

find goto-testing/programs/invalid -type f |
while read line ; do
    "$GOLITEC" parse "$line" > "$OUT" 2>&1 && (echo -n "$line " ; cat "$OUT")
done

find goto-testing/programs/valid-type -type f |
while read line ; do
    "$GOLITEC" typecheck "$line" > "$OUT" 2>&1 || cat "$OUT"
done

find goto-testing/programs/invalid-type -type f |
while read line ; do
    "$GOLITEC" typecheck "$line" > "$OUT" 2>&1 && (echo -n "$line " ; cat "$OUT")
done
EOF

