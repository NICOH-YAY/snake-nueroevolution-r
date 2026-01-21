# src/neural_network.R - FIXED VERSION
library(R6)

NeuralNetwork <- R6Class("NeuralNetwork",
  public = list(
    input_size = 12,    # FIXED: 12, not 24
    hidden_size = 8,
    output_size = 4,
    w1 = NULL, b1 = NULL, w2 = NULL, b2 = NULL,
    
    initialize = function(input_size = 12, hidden_size = 8, output_size = 4,
                          activation = "tanh") {
      self$input_size <- input_size
      self$hidden_size <- hidden_size
      self$output_size <- output_size
      
      # Initialize weights
      self$w1 <- matrix(rnorm(input_size * hidden_size, 0, 0.5), 
                        nrow = input_size, ncol = hidden_size)
      self$b1 <- rnorm(hidden_size, 0, 0.1)
      self$w2 <- matrix(rnorm(hidden_size * output_size, 0, 0.5),
                        nrow = hidden_size, ncol = output_size)
      self$b2 <- rnorm(output_size, 0, 0.1)
    },
    
    forward = function(x) {
      # Ensure x is a matrix with correct dimensions
      if(!is.matrix(x)) {
        x <- matrix(x, nrow = 1)
      }
      
      # Check dimensions
      if(ncol(x) != self$input_size) {
        stop(sprintf("Input has %d features, but NN expects %d", 
                     ncol(x), self$input_size))
      }
      
      # Forward pass
      hidden <- tanh(x %*% self$w1 + self$b1)
      output <- hidden %*% self$w2 + self$b2
      
      # Softmax
      exp_output <- exp(output - max(output))
      as.numeric(exp_output / sum(exp_output))
    },
    
    get_weights = function() {
      c(as.vector(self$w1), as.vector(self$b1),
        as.vector(self$w2), as.vector(self$b2))
    },
    
    set_weights = function(w) {
      n_w1 <- length(self$w1)
      n_b1 <- length(self$b1)
      n_w2 <- length(self$w2)
      
      self$w1 <- matrix(w[1:n_w1], nrow(self$w1), ncol(self$w1))
      self$b1 <- w[(n_w1+1):(n_w1+n_b1)]
      self$w2 <- matrix(w[(n_w1+n_b1+1):(n_w1+n_b1+n_w2)], 
                        nrow(self$w2), ncol(self$w2))
      self$b2 <- w[(n_w1+n_b1+n_w2+1):length(w)]
    },
    
    clone_network = function() {
      nn <- NeuralNetwork$new(self$input_size, self$hidden_size, self$output_size)
      nn$set_weights(self$get_weights())
      return(nn)
    }
  )
)
