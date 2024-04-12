#!/bin/bash

#export MIKENET_DIR="/work/yaningchang/mikenet"
#echo $MIKENET_DIR

#Reading training: Training the OP and OS focused training models with different levels of oral language proficiency 
#(LP:low proficiency, MP:Moderate proficiency, HP:High proficiency) 

for k in 'HP'
do
	for i in 21 22 23 24 25
	do
     
     #echo "OP focused training: version=${i}, proficiency level=${k}"
     ./bin/eng_OP_focused_reading_blis -seed ${i} -trained_weights ../TrainedWeights/Oral/${k}/Oral_Weight_${k}_v${i} -epsilon 0.05 -iteration 1000000
     
     #echo "OS focused training: version=${i}, proficiency level=${k}"
     ./bin/eng_OS_focused_reading_blis -seed ${i} -trained_weights ../TrainedWeights/Oral/${k}/Oral_Weight_${k}_v${i} -epsilon 0.05 -iteration 1000000
     
     #echo "balanced training: version=${i}, proficiency level=${k}"
     ./bin/eng_balanced_reading_blis -seed ${i} -trained_weights ../TrainedWeights/Oral/${k}/Oral_Weight_${k}_v${i} -epsilon 0.05 -iteration 1000000
	done
done
