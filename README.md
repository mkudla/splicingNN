# convolutional neural network prediction of splicing factor binding

![Screenshot](https://github.com/mkudla/splicingNN/blob/master/roc.png)
![Screenshot](https://github.com/mkudla/splicingNN/blob/master/prec.png)

## notes

This is project applies convolutional neural networks to prediction of splicing factor binding. Main thesis of the experiment to be tested is that supplying the information about the structure should improve the accuracy of prediction. This appears to be true (see light green curve versus black). The rationale behind this is that algorithm for optimal structure predicion is not trivial and although one can argue that big and deep enough network could learn rules for folding and the facto read the structure from the sequence, it does seem that at least for simple network with limited amount of training data, it is better to supply the structure information for the RNA.

This is pretty much just like preparing an additional analysis for a human operator that has to make a decision. This additional analysis is preprocessing of the data, but it is based on the set of rules, that were derived using either additional knowledge or extra data. For example for the RNA structure prediction it were physical principles and experiments.


## requirements

requires keras and Vienna RNA package installed for RNA structure prediction.

## use

Run splicingNN.R from within rstudio
