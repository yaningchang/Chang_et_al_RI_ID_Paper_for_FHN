%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Please refer to Chang et al. (2024) paper for the model architecture and training/testing procedures. 
The simulations are run using the Mikenet simulator: https://github.com/gtojty/OtoP/blob/master/Mikenet/mikenet_v8.tar.gz


Created by Ya-Ning Chang, 12 Apr 2024
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#######################################################################################################
# Training the English Reading model - oral language training and Reading training
# Executable files : eng_oral and eng_reading
# Parameters: 
# -seed : random seed for initial weights (integer)
# -iteration: training times
# -trained_weight: load the trained weight to the model
#######################################################################################################
To train the model, enter the following command in the unix-like terminal with Mikenet installed:

## oral language training 
sh eng_oral.sh

## reading training: training the OP, OS and OP-OS focused training models 

sh eng_reading.sh

#######################################################################################################
# Testing the model's phonological performance and semantic performance
# Executable file : eng_evaluator
# Parameters:
# -key: the information about the training set
# -patterns: testing set (e.g., ps_randcon.txt) 
# -semantic: test the model's semantic performance (without this index, the default is to test the model's phonological performance) 
# -weights: trained weight
# >:  output file 
#######################################################################################################
To test the model, enter the following command in the unix-like terminal with Mikenet installed:

e.g. for listening comprehension (PS)
./eng_evaluator -key 6kdict.txt -patterns ps_randcon.pat -semantic -weights ../TrainedWeights/Oral/PS_Weight_v1 > PS_v1.txt

e.g. for speaking (SP)
./eng_evaluator -key 6kdict.txt -patterns sp.pat -weights ../TrainedWeights/Oral/SP_Weight_v1 > SP_v1.txt

e.g. for word comprehension (OS)
./eng_evaluator -key 6kdict.txt -patterns englishdict_randcon.pat -semantic -weights ../TrainedWeights/Reading/OP/OP_Reading_Weight_v1_t1000000 > OP_focused_OS_v1.txt

e.g. for word naming (OP)
./eng_evaluator -key 6kdict.txt -patterns englishdict_randcon.pat -weights ../TrainedWeights/Reading/OP/OP_Reading_Weight_v1_t1000000 > OP_focused_OP_v1.txt


The evaluator output format:
For phonology, column names are 'test_name', 'word',  'correctness', 'error score'
For semantics, column names are 'test_name', 'word', 'unit activation....'

To further compute the accuracy and error scores (euclidean distance) for semantics, a Matlab function is provided    
e.g. In the Matlab terminal, enter:
>> examine_semantic_output('PS_v1.txt')
and this will generate an output file 'nearest_PS_v1.txt'

################################################################################################################################
#
# Division of Labour: Compute unique contributions to semantics and phonology from different pathways
# in the OP focused and OS focused models using the lesioning technique
#
################################################################################################################################
Enter the following command in the unix-like terminal with Mikenet installed:

sh eng_dol.sh


