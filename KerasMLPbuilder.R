###########
# Author: Loic Verlingue
# build and run a MLP in a single function
# optionally it can be used to search for hyperparameters 

###########
# first define your defaults hyperparameters
Default<-list(Layers=3, Regul=c(2,3), Type="l2", lambda=c(0.1668956, 0.1668956), 
              Dropout=0, DropoutVal=0, SkipConnection=F,
              beta_1=0.9, beta_2=0.999, Function="relu", FinalFunction="softmax",
              BigL1drop=T, lr = 0.005, Decay=0.001, epochs=60)

###########
# function to build and run a MLP from defined hyperparmaters

#### generic MLP builder function
MLPmodel<-function(UNITS, Default, Regul){ 
  
  # UNITS is a vector of the number of units you want for each layer - defined after
  # Default : your hyperparameters
  # Regul : type of Regulation L1 or L2 in names, and related layer in value
    
  input_tensor <- layer_input(shape = c(UNITS[1])) 
    
  listLayers<-list(input_tensor)
  
  #L=3
  for(L in seq(Default$Layers)){ #[-c(Layers)]
    
    Reg<-paste("regularizer_",names(Regul)[Regul%in%L],"(",Default$lambda[Regul%in%L],")",sep="")
    
    if(L!=Default$Layers){ 
      
        listLayers[[length(listLayers)+1]]<-layer_dense( object = listLayers[[length(listLayers)]] ,
                                                         units = UNITS[L+1], activation = Default$Function, 
                                                         name = paste("dens",L,sep = ""), 
                                                         kernel_regularizer = if(L%in%Regul){eval(parse(text=Reg))}else{NULL}) #names(Regul)[Regul%in%L]
        if(L%in%Default$Dropout){
          listLayers[[length(listLayers)+1]]<-layer_dropout(object = listLayers[[length(listLayers)]], 
                                                            rate = Default$DropoutVal[Default$Dropout%in%L],name = paste("Dropout",L,sep = "") ) 
        }
      
      
     if(Default$SkipConnection&L%%2==0){ # only odd numbers
        
       if(dim(listLayers[[length(listLayers)-1]])[[2]]==dim(listLayers[[length(listLayers)]])[[2]]){
          listLayers[[length(listLayers)+1]] <- layer_add(c(listLayers[[length(listLayers)-1]], listLayers[[length(listLayers)]]),
                                                          name = paste("Residual",L,sep = ""))
          #} else {
          #  print("No skip connections because different dimensions")
        }
      }
      
      # add a batch regularization layer for hidden layers
      #L+1!=Layers
      
      listLayers[[length(listLayers)+1]]<-layer_batch_normalization(object = listLayers[[length(listLayers)]], name = paste("batch",L,sep = "")) 
    
      } else { # add a final output layer

          listLayers[[length(listLayers)+1]]<-layer_dense( object = listLayers[[length(listLayers)]] ,
                                                           units = UNITS[L+1], activation = Default$FinalFunction, 
                                                           name = paste("dens",L,sep = ""), 
                                                           kernel_regularizer = if(L%in%Regul){eval(parse(text=Reg))}else{NULL}) 
          if(L%in%Default$Dropout){
            listLayers[[length(listLayers)+1]]<-layer_dropout(object = listLayers[[length(listLayers)]], 
                                                              rate = Default$DropoutVal[Default$Dropout%in%L],name = paste("Dropout",L,sep = "") ) 
          }     
      }
  }
  
   # names(listLayers)<-paste("dens",seq(length(listLayers)),sep = "" )
  #  list2env(listLayers,envir = .GlobalEnv)
  
  model <- keras_model(inputs = input_tensor , outputs = listLayers[[length(listLayers)]])
  
  return(model)
}

BuildRunMLP<-function(XtrainN=X, Target=Y, 
                      validation_split=TrainSplit, Default=Default, Model=NULL){ 
  
  # XtrainN is your matrix of inputs
  # Target is your feature vector or matrix
  # validation_split is your internal validation during training
  # Default is you list of hyperparameters, again
  # Model : you can also just run a given model without the buiding step
   
   ##### MLP in & out
  input_tensor <- as.array(as.matrix(XtrainN)) 
  #dim(input_tensor);dim(Target)
  
    Nvar<-ncol(input_tensor)
    Nex<-nrow(input_tensor)
    Nclass<-ncol(Target)
    
    ##### MLP structure
    # units and layers
    if(is.null(Model)){
      if(Default$BigL1drop){
        #UNITS<-round(exp(seq(log(Nvar), log(Nclass),length.out = Default$Layers+1)))
        UNITS<-round(c(Nvar,exp(seq(log(Nvar/Default$Layers), log(Nclass),length.out = Default$Layers+1))[-1]))
      } else {
        UNITS<-round(seq(Nvar, Nclass,length.out = Default$Layers+1))
      }
      
      # skip connections
      if(Default$SkipConnection&Default$Layers>4){
        SELECT<-floor(Default$Layers/2):(floor(Default$Layers/2)+floor(Default$Layers/3))
        UNITS[SELECT]<-round(mean(UNITS[SELECT]))
      } else {
        Default$SkipConnection<-FALSE
      }  
      
      # layers for regularization
      Regul<- Default$Regul 
      names(Regul)<-Default$Type
      
      if(length( Default$lambda)!=length(Default$Regul) ){
        Default$lambda<-rep( Default$lambda, length(Default$Regul) )
      }
      if(length( Default$Type)!=length(Default$Regul) ){
        Default$lambda<-rep( Default$Type[1], length(Default$Regul) )
      }

      # build the MLP model
      model = MLPmodel(UNITS = UNITS, Default=Default, Regul = Regul)
    } else {
      model<-Model
    }
  
    ## run the optimization
    print(model)
  
    opt = optimizer_adam(lr = Default$lr, beta_1 = Default$beta_1, beta_2 = Default$beta_2, 
                         epsilon = 10e-8, decay=Default$Decay)
    #opt = optimizer_rmsprop(lr = Default$lr)
    
    model %>% compile(
      optimizer = opt,
      loss = ifelse("softmax"%in%Default$FinalFunction,"categorical_crossentropy","mse") ,
      metrics = c("accuracy")
    )
    
    history <- model %>% fit(
      x = input_tensor, 
      y = Target,
      view_metrics = F, batch_size = 2^4,
      epochs = Default$epochs, 
      validation_split = validation_split, shuffle = F)
  
  return(list(model=model,history=history))
}
