#!/bin/sh
breakchars="(){}[],^%$#@\"\";:''|\\"
CLOJURE_DIR=/Users/josh/projects/earthmine/research/josh/clojure/clojure-1.2.0
CLOJURE_JAR="$CLOJURE_DIR"/clojure.jar
CLOJURE_CONTRIB=/Users/josh/projects/earthmine/research/josh/clojure/clojure-contrib-1.2.0
CLOJURE_CONTRIB_JAR="$CLOJURE_CONTRIB"/clojure-contrib.jar

OTHER_LIBS=/Users/josh/projects/earthmine/research/josh/clojure/proj/clj-web-crawler

if [ $# -eq 0 ]; then 
     exec rlwrap --remember -c -b "$breakchars" \
        -f "$HOME"/.clj_completions \
         java -cp "$CLOJURE_JAR":"$CLOJURE_CONTRIB_JAR":"$OTHER_LIBS" clojure.main
else
     exec java -cp "$CLOJURE_JAR":"$CLOJURE_CONTRIB_JAR":"$OTHER_LIBS" clojure.main $1 -- "$@"
fi

