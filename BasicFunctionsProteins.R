

##
# This function works as expected
# It converts String to one hot encoded matrix
DNAstringToOneHot<-function(x) {
  z<-strsplit(x,"")
  z<-z[[1]]
  i<-1
  a<-c(0,0,0,0)
  while(i<length(z)+1) {
    #print(z[i]) #debug
    #case switch
    conv = switch(z[i],
                  "A" = c(1,0,0,0),
                  "C" = c(0,1,0,0),
                  "G" = c(0,0,1,0),
                  "T" = c(0,0,0,1),
                  "U" = c(0,0,0,1)) # there was no U here. Ahahahahahaahahahahaha!
    #print(conv) #debug
    a<-cbind(a,conv)
    i=i+1
  }
  a<-a[,-1]
  return(as.matrix(t(a)))
}
##
# function ends

#
# Works as supposed, USE THIS ONE!!
# This function converts input to 3D array of One Hot encoded DNA
ConvertStringArray2<-function(x) {
  len<-length(x)
  strdim<-dim(DNAstringToOneHot(x[1]))
  xlen<-strdim[1]
  ylen<-strdim[2]
  arr<-array(seq(0,0,length.out=len*xlen*ylen), c(len,xlen,ylen))
  ii<-1
  while(ii<len+1) {
    k<-DNAstringToOneHot(x[ii])
    #print(k)
    arr[ii,,]<-(k)
    ii=ii+1
  }
  return(arr)
}
##
# function ends

ConvertStringArray2List<-function(x) {
  len<-length(x)
  listseq<-list()
  ii<-1
  while(ii<len+1) {
    k<-DNAstringToOneHot(x[ii])
    #print(k)
    listseq[[ii]]<-k
    #listseq<-append(listseq, k)
    ii=ii+1
  }
  return(listseq)
}

PadSequenceList<-function(x,maxlength) {
  i<-1
  len<-length(x)
  while(i<len+1) {
    lenx<-dim(x[[i]])[1]
    zeromat<-matrix(0,ncol = 4, nrow = maxlength-lenx)
    x[[i]]<-rbind(x[[i]],zeromat)
    i<-i+1
  }
  return(x)
}

ConvertListToArray<-function(x) {
  len<-length(x)
  xlen<-dim(x[[1]])[1]
  ylen<-dim(x[[1]])[2]
  
  #arr<-array()
  arr<-array(0, c(len,xlen,ylen))
  
  i<-1
  while(i<len+1) {
    arr[i,,]<-x[[i]]
    i<-i+1
  }
  return(arr)
}

ConvertListToNNinput<-function(x,maxsize) {
  x<-as.character(x)
  x<-ConvertStringArray2List(x)
  x<-PadSequenceList(x,maxsize)
  x<-ConvertListToArray(x)
  x<-array_reshape(x,c(nrow(x),dim(x[1,,])[1],dim(x[1,,])[2],1))
  return(x)
}


ReturnTPRFPRArray<-function(x,truth){
  arr<-array()
  for(i in 1:1000){
    cutoff<- 0.001 * i
    prediction<-as.numeric(x>cutoff)
    FP<-xor(prediction,truth)&prediction
    FN<-xor(prediction,truth)&truth
    TN<-!xor(prediction,truth)&!truth
    TP<-!xor(prediction,truth)&truth
    nTP<-sum(TP)
    nTN<-sum(TN)
    nFP<-sum(FP)
    nFN<-sum(FN)
    FPR = nFP/(nFP+nTN)
    TPR = nTP/(nTP+nFN)
    PREC = nTP/(nTP+nFP)
    arr<-rbind(arr,c(cutoff,FPR,TPR, PREC))
  }
  colnames(arr)<-c("cutoff","FPR","TPR","PREC")
  arr<-arr[-1,]
  return(arr)
}

ReturnTPRFPRArrayBounded<-function(x,truth, minx,maxx,steps = 1000){
  arr<-array()
  for(i in 1:steps){
    cutoff<- ((maxx-minx)/steps * i)+minx
    prediction<-as.numeric(x>cutoff)
    FP<-xor(prediction,truth)&prediction
    FN<-xor(prediction,truth)&truth
    TN<-!xor(prediction,truth)&!truth
    TP<-!xor(prediction,truth)&truth
    nTP<-sum(TP)
    nTN<-sum(TN)
    nFP<-sum(FP)
    nFN<-sum(FN)
    FPR = nFP/(nFP+nTN)
    TPR = nTP/(nTP+nFN)
    PREC = nTP/(nTP+nFP)
    arr<-rbind(arr,c(cutoff,FPR,TPR, PREC))
  }
  colnames(arr)<-c("cutoff","FPR","TPR","PREC")
  arr<-arr[-1,]
  return(arr)
}

returnStructureString<-function(substring2){
  cmd<-paste0("echo \"",substring2,"\" | RNAfold --noPS | sed \'s/ /\\n/\'")
  capturedoutput<-system(cmd,intern = TRUE)[2]
  return(capturedoutput)
}

StructStringToOneHotAlt<-function(x) {
  z<-strsplit(x,"")
  z<-z[[1]]
  i<-1
  a<-c(0,0,0,0)
  while(i<length(z)+1) {
    #print(z[i]) #debug
    #case switch
    conv = switch(z[i],
                  "(" = c(1,0,0,0),
                  ")" = c(0,1,0,0),
                  "{" = c(0,0,1,0),
                  "}" = c(0,0,1,0),
                  "|" = c(0,0,1,0),
                  "," = c(0,0,1,0), # this probably needs separate class
                  "." = c(0,0,0,1))
    #print(conv) #debug
    a<-cbind(a,conv)
    i=i+1
  }
  a<-a[,-1]
  return(as.matrix(t(a)))
}
