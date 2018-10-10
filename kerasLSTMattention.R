#######
# example of a LSTM with attention mechanism, in R Keras.
# exemple data from imdb, see desciption in keras book from Chollet et al
# structure of the net influenced by Andrew Ng's tutorial from Bahdanau et al 2014 and Xu et al 2015 papers
# Author: Loic Verlingue, published the september 26th 2018

library(keras)

# load data
# here reduce a lots de data to have a fast runin

mVoc = 100
imdb <- dataset_imdb(num_words = mVoc) #10000
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% imdb

# descriptions
length(train_data)
length(test_data)
length(train_data[[1]])
train_data[[1]]
train_labels[[1]]
max(sapply(train_data, max))

# further reduce dataset

Red_x_train<-lapply(train_data[1:200], function(Red){
  Red[1:50]
})
Red_y_train<-train_labels[1:200]

########
# padding

max(sapply(Red_x_train, length))
maxlen=max(sapply(Red_x_train, length))

PADval<-sample(setdiff(1:10, unique(unlist(Red_x_train)) ),1)
PADval

PAD_x_train <- lapply(Red_x_train, function(PAD){
  c(PAD, rep(PADval, maxlen-length(PAD)) )
})

#function for 1-hot-encoding the integer sequence into a binary matrix 
vectorize_sequences <- function(sequences, dimension = mVoc) { #10000
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  for (i in 1:length(sequences))
    results[i, sequences[[i]]] <- 1
  results
}

# vectorize each examples into a list of 1-hot-encoded matrices
# Xoh.shape = (m, Tx, len(human_vocab))
Xoh <- lapply(PAD_x_train,vectorize_sequences) # x_train
dim(Xoh[[1]])

#  x_test <- lapply(test_data,vectorize_sequences)  # x_test

# preprocess

# The pre-attention Bi-LSTM goes through  Tx  time steps; 
# the post-attention LSTM goes through  Ty  time steps

Tx=maxlen
Ty = 10


# n_a -- hidden state size of the Bi-LSTM #
# n_s -- hidden state size of the post-attention LSTM #
n_a = 32
n_s = 64

s_prev <- layer_input(shape = c(n_s))
a<-layer_input(shape = c(Tx,2*n_a))

### compute attention step
one_step_attention<-function(a, s_prev, Tx){
  #Arguments:
  #  a -- hidden state output of the Bi-LSTM, numpy-array of shape (m, Tx, 2*n_a)
  #  s_prev -- previous hidden state of the (post-attention) LSTM, numpy-array of shape (m, n_s)
  
  ## repeat s_prev to be of shape (m, Tx, n_s) so that you can concatenate it with all hidden states "a"
  s_prev <- layer_repeat_vector(s_prev, Tx)
  
  # concatenate a and s_prev on the last axis
  concat = layer_concatenate(list(a, s_prev), axis = -1)

  # propagate concat through a small fully-connected neural network to compute the "intermediate energies" variable e. 
  e = layer_dense(concat, units = 10, activation = "tanh")
 
  # propagate e through a small fully-connected neural network to compute the "energies" variable energies.
  energies = layer_dense(e, units = 1, activation = "relu")
 
  # compute the attention weights "alphas" 
  alphas = layer_activation_softmax(energies, axis = 1, name='attention_weights') #
  
  # "alphas" and "a" to compute the context vector to be given to the next (post-attention) LSTM-cell
  context = layer_dot(list(alphas,a), axes = 1) 
  
  return(context)
}

###########
### model
# of type: "many to one" i.e sequencial inputs, a single output
X_size=mVoc
Y_size=1 #length(Red_y_train) #only 1 output, Ty is therefore removed here 

model<-function(Tx, n_a, n_s, X_size, Y_size){ 
  
  # inputs of the model
  X <- layer_input(shape = c(Tx, X_size)) #(None, 30, 37) 
  s0 <- layer_input(shape = c(n_s), name='s0')  # (None, 64)  
  c0 <- layer_input(shape = c(n_s), name='c0') # (None, 64)  
  
  s = s0
  c = c0
  # Initialize empty list of outputs
  
  # Define your pre-attention Bi-LSTM (return_sequences=True)
  a = bidirectional(object = X, layer = layer_lstm(units =  n_a, return_sequences=TRUE))
  
  # Step 2: either iterate for Ty steps, or do the following when only 1 output
  # one step of the attention mechanism to get back the context vector
  context = one_step_attention(a, s, Tx)
  
  # Apply the post-attention LSTM cell to the "context" vector
  conc_plus<-layer_concatenate(list(context ,layer_reshape(s, c(1,n_s))))
  
  c(s,Z, c) %<-% layer_lstm(object = conc_plus, units = n_s, return_state = TRUE) 
   
  # The value of initial_state should be a tensor or list of tensors representing the initial state of the RNN layer
  
  # Apply Dense layer to the hidden state output of the post-attention LSTM
  out <-  layer_dense(s, 1, activation="sigmoid")
  
  # finally, create model instance taking three inputs and returning the outputs.
  
  model <- keras_model(inputs = c(X,s0,c0), outputs = out)
  
  return(model)
}

# build and compile your model
model = model(Tx, n_a, n_s, X_size, Y_size)
model

opt = optimizer_adam(lr = 0.005, beta_1 = 0.9, beta_2 = 0.999, epsilon = 10e-8, decay=0.01)
model %>% compile(
  optimizer = opt,
  loss = "binary_crossentropy",
  metrics = c("acc")
)


# initial values
s0 = matrix(0,nrow = length(Xoh), ncol = n_s)
c0 = matrix(0,nrow = length(Xoh), ncol = n_s)

# run the learning phase
# epochs=1 for convenience, add more iteration for real learning, but can be long

history <- model %>% fit(x = list(Xoh, s0, c0),y = Red_y_train, epochs=1, batch_size=10)

#visualization

plot(history)
get_weights(model)
names(model)
sapply(names(model),function(x){grep("attention", model$x)})
model

# last, vizualize attention mechanisms as a heatmap #todo

