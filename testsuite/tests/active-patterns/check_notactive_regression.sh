NEW_OCAML=../../..
OLD_OCAML=../../../../../old_ocaml # change this to path to stable ocaml 4.12

# other files in basic/patmatch_*.ml are already covered by [%%expect ...] annotations
# make one DIR=tests/basic -- to test these other files
FILES="../basic/patmatch.ml
../basic/stringmatch.ml
../basic/tuple_match.ml
../basic-more/morematch.ml
../basic-more/pr10294.ml
../basic-more/robustmatch.ml
../match-exception/*.ml
../match-exception-warnings/*.ml
../misc/sorts.ml
../parsetree/source.ml
"

mkdir res
for filename in $FILES
do
    $NEW_OCAML/boot/ocamlrun $NEW_OCAML/ocamlc  -nostdlib -I $NEW_OCAML/stdlib -drawlambda $filename 2> res/$(basename $filename).new
    $OLD_OCAML/boot/ocamlrun $OLD_OCAML/ocamlc  -nostdlib -I $OLD_OCAML/stdlib -drawlambda $filename 2> res/$(basename $filename).old
    
    # because of changed predefined definitions (addition of Choice_n) numbers of other predefined structures changed
    # so we just delete lines containing these false-positive diffs
    grep -E -v "Assert_failure|Match_failure" res/$(basename $filename).old > "tmp"
    mv "tmp" res/$(basename $filename).old 
    grep -E -v "Assert_failure|Match_failure" res/$(basename $filename).new > "tmp"
    mv "tmp" res/$(basename $filename).new 

    if cmp -s res/$(basename $filename).old res/$(basename $filename).new; then
        echo "OK: $filename"
    else
        echo "FAIL: Pattern-matching lambda regression in $filename"
    fi
done
rm -rf res a.out