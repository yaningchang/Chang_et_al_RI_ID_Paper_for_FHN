#!/bin/bash
# Oral language training: training the model for three levels of oral proficiency (i.e., 2000000 epochs)

for k in 2000000
do
	for i in 27
	do
     
     echo "version=${i}, iteration=${k}"

     ./bin/eng_oral_blis -seed ${i} -epsilon 0.05 -iteration ${k}
     
	done
done
