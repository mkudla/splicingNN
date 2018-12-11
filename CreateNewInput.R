
#sd<-read.table("splicefactor.data.set")
#"EFTUD2"
#"TBRG4"

library(keras)
#label<-"EFTUD2"

# Prepare the table for given splice factor:
t<-sd[sd[,2]==label,] # get splice factor relevant sequences
tneg<-sd[sd[,2]!=label,] # get negative sequences, which are unrelated sequences
tneg<-tneg[sample(nrow(tneg), dim(t)[1]), ] # make table same in size (we have too much negative)
t<-cbind(t,"positives") # add label to positives that they are positive
tneg<-cbind(tneg,"negatives") # add label to negatives that they are negative
colnames(t)<-"" # reset colnames
colnames(tneg)<-""
t<-rbind(t,tneg) # bind together t and tneg tables, now they have labels that distinguish them
t <- t[sample(nrow(t)),] # shuffle the table row-wise
t<-cbind(t, t[,4]=="positives")    # get vector of T/F

# the mother table for splicing factor is now ready.

t<-t[nchar(as.character(t[,3])) >= MINSIZE, ] # filter the sequences by length
t<-t[nchar(as.character(t[,3])) <= MAXSIZE, ]

t.test.x<-t[t[,1]=="test",3] # take only sequences to test / input
t.test.y<-t[t[,1]=="test",5] # take only values to test / output

t.train.x<-t[t[,1]=="train",3] # take only sequences to train / input
t.train.y<-t[t[,1]=="train",5] # take only sequences to train / output

t.test.x<-as.character(t.test.x) # we only need it as a list of character sequences
t.train.x<-as.character(t.train.x)

t.train.x.OH<-ConvertStringArray2List(t.train.x) # let's convert it to one-hot encoding
t.test.x.OH<-ConvertStringArray2List(t.test.x)

t.train.x.OH<-PadSequenceList(t.train.x.OH, MAXSIZE) # pad sequences, so they are same size (MAXSIZE)
t.test.x.OH<-PadSequenceList(t.test.x.OH, MAXSIZE)

t.train.y<-as.numeric(t.train.y) # we need our output as a list of numbers
t.test.y<-as.numeric(t.test.y)

#
t.test.x.OH<-ConvertListToArray(t.test.x.OH) # we need to convert our list to array
t.train.x.OH<-ConvertListToArray(t.train.x.OH)

t.test.x.OH<-array_reshape(t.test.x.OH,c(nrow(t.test.x.OH),dim(t.test.x.OH[1,,])[1],dim(t.test.x.OH[1,,])[2],1)) # and reshape the array so it is compatible with the first input layer
t.train.x.OH<-array_reshape(t.train.x.OH,c(nrow(t.train.x.OH),dim(t.train.x.OH[1,,])[1],dim(t.train.x.OH[1,,])[2],1))

assign(paste0(label,".test.x.OH"), t.test.x.OH) # let's finally name our objects with their label name
assign(paste0(label,".train.x.OH"), t.train.x.OH)

assign(paste0(label,".test.y"), t.test.y)
assign(paste0(label,".train.y"), t.train.y)

# structure is FROM HERE
# create structure string on input
tt<-t.test.x
i<-1
y.struct<-list()
while(i<=length(tt)[1]){
  y.struct[i]<-returnStructureString(as.character(tt[i]))
  i<-i+1
  if(!(i%%100))
    print(i)
}
# return one hot string on input
i<-1
y.struct.OH<-list()
while(i<=length(y.struct)){
  y.struct.OH[[i]]<-StructStringToOneHotAlt(as.character(y.struct[[i]]))
  i<-i+1
  if(!(i%%100))
    print(i)
}
y.struct.OH<-PadSequenceList(y.struct.OH, MAXSIZE) # pad sequences
#y.struct.OH<-ConvertListToNNinput(y.struct.OH,MAXSIZE) # faulty
y.struct.OH<-ConvertListToArray(y.struct.OH) # we need to convert our list to array
y.struct.OH<-array_reshape(y.struct.OH,c(nrow(y.struct.OH),dim(y.struct.OH[1,,])[1],dim(y.struct.OH[1,,])[2],1)) # and reshape the array so it is compatible with the first input layer
assign(paste0(label,".test.x.OH.struct"), y.struct.OH) 

# number 2

tt<-t.train.x
i<-1
y.struct<-list()
while(i<=length(tt)[1]){
  y.struct[i]<-returnStructureString(as.character(tt[i]))
  i<-i+1
  if(!(i%%100))
    print(i)
}
# return one hot string on input
i<-1
y.struct.OH<-list()
while(i<=length(y.struct)){
  y.struct.OH[[i]]<-StructStringToOneHotAlt(as.character(y.struct[[i]]))
  i<-i+1
  if(!(i%%100))
    print(i)
}
y.struct.OH<-PadSequenceList(y.struct.OH, MAXSIZE) # pad sequences
#y.struct.OH<-ConvertListToNNinput(y.struct.OH,MAXSIZE) # faulty!
y.struct.OH<-ConvertListToArray(y.struct.OH) # we need to convert our list to array
y.struct.OH<-array_reshape(y.struct.OH,c(nrow(y.struct.OH),dim(y.struct.OH[1,,])[1],dim(y.struct.OH[1,,])[2],1)) # and reshape the array so it is compatible with the first input layer

assign(paste0(label,".train.x.OH.struct"), y.struct.OH) 

# TO HERE

rm(t, tneg, t.test.x, t.train.x, t.test.y, t.train.y,  t.test.x.OH, t.train.x.OH, y.struct.OH, y.struct, tt)

