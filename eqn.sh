OLDIFS="$IFS";IFS=$'\n';while read -r;do curl -H 'Content-Type: application/json'  -X POST localhost:5000/reduced_elementary_axes -d "$REPLY";echo;done <eqn
