#Copyright (c) 2018 Marek Kudla.
#MIT License

source("BasicFunctionsProteins.R")

library(keras)


sd<-read.table("splicefactor.data.set")
MINSIZE<-15
MAXSIZE<-250

# labels to try out: AKAP8L AARS AGGF1
label<-"AKAP8L"
source("CreateNewInput.R")

# conventional NN stack

tnn9 <- keras_model_sequential() 
tnn9 %>% 
  layer_conv_2d(filters = 64, kernel_size = c(8,4), activation = 'relu', input_shape = c(MAXSIZE,4,1)) %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dropout(rate = 0.1) %>% 
  layer_flatten() %>%
  layer_dense(units = 1, activation='linear')

tnn9 %>% compile(
  loss = 'mean_squared_error',
  optimizer = 'adam',
  metrics = c('accuracy')
)

htnn9<-tnn9 %>% fit(
  get(paste0(label,".train.x.OH")), get(paste0(label,".train.y")), 
  epochs = 15, batch_size = 128, 
  validation_split = 0.1
)

tnet_test9<-tnn9 %>% predict(get(paste0(label,".test.x.OH")))

#
# tnn14bi - bifurcated inputs sequence / structure
#

main_input <- layer_input(shape = c(MAXSIZE,4,1), name = 'main_input')

sequence_subnet <- main_input %>%
  layer_conv_2d(filters = 64, kernel_size = c(8,4), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(4,1)) %>%
  layer_dropout(rate=0.1)

auxiliary_input <- layer_input(shape = c(MAXSIZE,4,1), name = 'aux_input')

structure_subnet <- auxiliary_input %>%
  layer_conv_2d(filters = 64, kernel_size = c(8,4), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(4,1)) %>%
  layer_dropout(rate=0.1)

main_output <- layer_concatenate(c(sequence_subnet, structure_subnet),axis=2) %>%
  layer_conv_2d(filters = 64, kernel_size = c(6,2), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(4,1)) %>%
  layer_dropout(rate=0.1) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units = 32, activation = 'relu') %>%
  layer_dropout(rate=0.1) %>%
  layer_dense(units = 1, activation = 'sigmoid', name = 'main_output')

tnn14bi <- keras_model(
  inputs = c(main_input, auxiliary_input),
  outputs = c(main_output)
)

tnn14bi %>% compile(
  loss = 'mean_squared_error',
  optimizer = "adam",
  metrics = c('accuracy')
)
htnn14bi<-tnn14bi %>% fit(
  x = list(get(paste0(label,".train.x.OH")), get(paste0(label,".train.x.OH.struct"))),
  y = get(paste0(label,".train.y")),
  epochs = 10, batch_size = 128,
  validation_split = 0.1
)

tnet_test14bi<-tnn14bi %>% predict(list(get(paste0(label,".test.x.OH")), get(paste0(label,".test.x.OH.struct"))))

plot(ReturnTPRFPRArrayBounded(tnet_test9, get(paste0(label,".test.y")), minx=-0.5,maxx=1.5)[,2:3],xlim=c(0,1),ylim=c(0,1),type='l',xlab="FPR",ylab="TPR",col="black", main= "ROC")
lines(ReturnTPRFPRArrayBounded(tnet_test14bi, get(paste0(label,".test.y")), minx=-0.5,maxx=1.5)[,2:3],xlim=c(0,1),ylim=c(0,1),type='l',xlab="FPR",ylab="TPR",col="lightgreen", main = "ROC")
abline(b=1,a=0)

plot(ReturnTPRFPRArrayBounded(tnet_test9, get(paste0(label,".test.y")), minx=-0.5,maxx=1.5)[,3:4],xlim=c(0,1),ylim=c(0,1),type='l',xlab="FPR",ylab="TPR",col="black", main = "precision recall curve")
lines(ReturnTPRFPRArrayBounded(tnet_test14bi, get(paste0(label,".test.y")), minx=-0.5,maxx=1.5)[,3:4],xlim=c(0,1),ylim=c(0,1),type='l',xlab="FPR",ylab="TPR",col="lightgreen", main = "precision recall curve")
abline(b=1,a=0)

library(pROC)
auc(get(paste0(label,".test.y")),tnet_test9)
auc(get(paste0(label,".test.y")),tnet_test14bi)
