# src/genetic_algorithm.R - UPDATED TO MATCH WORKING VERSION
library(R6)

GeneticAlgorithm <- R6Class("GeneticAlgorithm",
                            public = list(
                              pop_size = 10,
                              population = NULL,
                              fitness = NULL,
                              best_individual = NULL,
                              best_fitness = 0,
                              
                              # Constructor - uses pop_size NOT population_size
                              initialize = function(pop_size = 10) {
                                self$pop_size <- pop_size
                                self$population <- list()
                                for(i in 1:pop_size) {
                                  self$population[[i]] <- NeuralNetwork$new()
                                }
                                self$fitness <- numeric(pop_size)
                              },
                              
                              # Alias for compatibility
                              initialize_population = function() {
                                cat("Population of", self$pop_size, "neural networks ready\n")
                              },
                              
                              evaluate = function(steps = 20) {
                                cat("Evaluating... ")
                                for(i in 1:self$pop_size) {
                                  game <- SnakeGame$new(grid_size = 10)
                                  nn <- self$population[[i]]
                                  dirs <- c("up", "right", "down", "left")
                                  
                                  for(s in 1:steps) {
                                    state <- game$get_state()
                                    probs <- nn$forward(state)
                                    move <- dirs[which.max(probs)]
                                    if(!game$move(move)) break
                                  }
                                  
                                  self$fitness[i] <- game$calculate_fitness()
                                }
                                cat("Best:", round(max(self$fitness), 1), "\n")
                              },
                              
                              evolve = function() {
                                # Keep best 2
                                best_idx <- order(self$fitness, decreasing = TRUE)[1:2]
                                new_pop <- list(
                                  self$population[[best_idx[1]]]$copy(),
                                  self$population[[best_idx[2]]]$copy()
                                )
                                
                                # Fill rest with mutations
                                for(i in 3:self$pop_size) {
                                  parent <- self$population[[sample(best_idx, 1)]]$copy()
                                  w <- parent$get_weights()
                                  w <- w + rnorm(length(w), 0, 0.1)
                                  parent$set_weights(w)
                                  new_pop[[i]] <- parent
                                }
                                
                                self$population <- new_pop
                              },
                              
                              run_evolution = function(generations = 10, steps = 50) {
                                cat(sprintf("Running %d generations (population: %d)...\n", generations, self$pop_size))
                                for(g in 1:generations) {
                                  cat(sprintf("Gen %2d: ", g))
                                  self$evaluate(steps = steps)
                                  
                                  if(g < generations) {
                                    self$evolve()
                                  }
                                }
                                
                                # Save best
                                best_idx <- which.max(self$fitness)
                                self$best_fitness <- self$fitness[best_idx]
                                self$best_individual <- self$population[[best_idx]]$copy()
                                
                                cat("\nEvolution complete!\n")
                                cat("Best fitness:", round(self$best_fitness, 2), "\n")
                                
                                return(list(
                                  best_fitness = self$best_fitness,
                                  best_individual = self$best_individual
                                ))
                              },
                              
                              # Alias for run_evolution without parameters
                              run = function() {
                                return(self$run_evolution())
                              }
                            )
)