#/bin/bash

for i in $(ls -1 graphs/*.dot); do dot2tex $i --figonly --autosize -ftikz > $(echo $i |sed 's/dot/tex/g'); done
lualatex pres.tex
