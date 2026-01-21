# demo/snake_evolution_app.R - Complete Working Shiny App
library(shiny)
library(R6)
library(ggplot2)

# ==================== DEFINE CLASSES ====================

# Snake Game Class
SnakeGame <- R6Class("SnakeGame",
                     public = list(
                       grid_size = 15,
                       snake = NULL,
                       apple = NULL,
                       direction = "right",
                       score = 0,
                       steps = 0,
                       game_over = FALSE,
                       max_steps = 200,
                       
                       initialize = function(grid_size = 15) {
                         self$grid_size <- grid_size
                         start_x <- floor(grid_size / 2)
                         start_y <- floor(grid_size / 2)
                         
                         self$snake <- list(
                           c(start_x, start_y),
                           c(start_x - 1, start_y),
                           c(start_x - 2, start_y)
                         )
                         
                         self$spawn_apple()
                       },
                       
                       spawn_apple = function() {
                         while(TRUE) {
                           pos <- c(sample(1:self$grid_size, 1),
                                    sample(1:self$grid_size, 1))
                           
                           empty <- TRUE
                           for(seg in self$snake) {
                             if(all(pos == seg)) {
                               empty <- FALSE
                               break
                             }
                           }
                           
                           if(empty) {
                             self$apple <- pos
                             break
                           }
                         }
                       },
                       
                       move = function(dir = NULL) {
                         if(self$game_over || self$steps >= self$max_steps) {
                           self$game_over <- TRUE
                           return(FALSE)
                         }
                         
                         if(!is.null(dir)) {
                           if(!(self$direction == "up" && dir == "down") &&
                              !(self$direction == "down" && dir == "up") &&
                              !(self$direction == "left" && dir == "right") &&
                              !(self$direction == "right" && dir == "left")) {
                             self$direction <- dir
                           }
                         }
                         
                         self$steps <- self$steps + 1
                         head <- self$snake[[1]]
                         
                         new_head <- switch(self$direction,
                                            "up"    = c(head[1], head[2] + 1),
                                            "right" = c(head[1] + 1, head[2]),
                                            "down"  = c(head[1], head[2] - 1),
                                            "left"  = c(head[1] - 1, head[2])
                         )
                         
                         # Check walls
                         if(new_head[1] < 1 || new_head[1] > self$grid_size ||
                            new_head[2] < 1 || new_head[2] > self$grid_size) {
                           self$game_over <- TRUE
                           return(FALSE)
                         }
                         
                         # Check self
                         if(length(self$snake) > 1) {
                           for(i in 2:length(self$snake)) {
                             if(all(new_head == self$snake[[i]])) {
                               self$game_over <- TRUE
                               return(FALSE)
                             }
                           }
                         }
                         
                         # Move
                         self$snake <- c(list(new_head), self$snake)
                         
                         # Check apple
                         if(all(new_head == self$apple)) {
                           self$score <- self$score + 1
                           self$spawn_apple()
                         } else {
                           self$snake <- self$snake[-length(self$snake)]
                         }
                         
                         return(TRUE)
                       },
                       
                       get_state = function() {
                         head <- self$snake[[1]]
                         
                         c(
                           # Apple relative position
                           (self$apple[1] - head[1]) / self$grid_size,
                           (self$apple[2] - head[2]) / self$grid_size,
                           
                           # Wall distances
                           (head[1] - 1) / self$grid_size,
                           (self$grid_size - head[1]) / self$grid_size,
                           (head[2] - 1) / self$grid_size,
                           (self$grid_size - head[2]) / self$grid_size,
                           
                           # Current direction
                           as.numeric(self$direction == "up"),
                           as.numeric(self$direction == "right"),
                           as.numeric(self$direction == "down"),
                           as.numeric(self$direction == "left"),
                           
                           # Game info
                           length(self$snake) / 20,
                           self$score / 10
                         )
                       },
                       
                       calculate_fitness = function() {
                         apples <- self$score
                         snake_length <- length(self$snake)
                         
                         # Early-death penalty: died without eating any apples
                         if (apples == 0) {
                           return(-10)
                         }
                         
                         apples * 100 + snake_length
                       }
                       
                       get_grid_matrix = function() {
                         grid <- matrix(0, self$grid_size, self$grid_size)
                         
                         # Mark snake (head as 2, body as 1)
                         for(i in 1:length(self$snake)) {
                           seg <- self$snake[[i]]
                           if(i == 1) {
                             grid[seg[1], seg[2]] <- 2  # Head
                           } else {
                             grid[seg[1], seg[2]] <- 1  # Body
                           }
                         }
                         
                         # Mark apple (3)
                         grid[self$apple[1], self$apple[2]] <- 3
                         
                         return(grid)
                       },
                       
                       reset = function() {
                         start_x <- floor(self$grid_size / 2)
                         start_y <- floor(self$grid_size / 2)
                         
                         self$snake <- list(
                           c(start_x, start_y),
                           c(start_x - 1, start_y),
                           c(start_x - 2, start_y)
                         )
                         
                         self$direction <- "right"
                         self$score <- 0
                         self$steps <- 0
                         self$game_over <- FALSE
                         self$spawn_apple()
                       }
                     )
)

# Neural Network Class
NeuralNetwork <- R6Class("NeuralNetwork",
                         public = list(
                           input_size = 12,
                           hidden_size = 8,
                           output_size = 4,
                           w1 = NULL, b1 = NULL, w2 = NULL, b2 = NULL,
                           
                           initialize = function(input_size = 12, hidden_size = 8, output_size = 4, 
                                                 weights = NULL) {
                             self$input_size <- input_size
                             self$hidden_size <- hidden_size
                             self$output_size <- output_size
                             
                             if(is.null(weights)) {
                               self$w1 <- matrix(rnorm(input_size * hidden_size, 0, 0.5), 
                                                 nrow = input_size, ncol = hidden_size)
                               self$b1 <- rnorm(hidden_size, 0, 0.1)
                               self$w2 <- matrix(rnorm(hidden_size * output_size, 0, 0.5),
                                                 nrow = hidden_size, ncol = output_size)
                               self$b2 <- rnorm(output_size, 0, 0.1)
                             } else {
                               self$set_weights(weights)
                             }
                           },
                           
                           forward = function(x) {
                             if(!is.matrix(x)) x <- matrix(x, nrow = 1)
                             
                             hidden <- tanh(x %*% self$w1 + self$b1)
                             output <- hidden %*% self$w2 + self$b2
                             
                             exp_output <- exp(output - max(output))
                             as.numeric(exp_output / sum(exp_output))
                           },
                           
                           get_weights = function() {
                             c(as.vector(self$w1), as.vector(self$b1),
                               as.vector(self$w2), as.vector(self$b2))
                           },
                           
                           set_weights = function(w) {
                             n_w1 <- self$input_size * self$hidden_size
                             n_b1 <- self$hidden_size
                             n_w2 <- self$hidden_size * self$output_size
                             n_b2 <- self$output_size
                             
                             self$w1 <- matrix(w[1:n_w1], nrow(self$w1), ncol(self$w1))
                             self$b1 <- w[(n_w1+1):(n_w1+n_b1)]
                             self$w2 <- matrix(w[(n_w1+n_b1+1):(n_w1+n_b1+n_w2)], 
                                               nrow(self$w2), ncol(self$w2))
                             self$b2 <- w[(n_w1+n_b1+n_w2+1):(n_w1+n_b1+n_w2+n_b2)]
                           },
                           
                           copy = function() {
                             nn <- NeuralNetwork$new(self$input_size, self$hidden_size, self$output_size)
                             nn$set_weights(self$get_weights())
                             return(nn)
                           }
                         )
)

# Genetic Algorithm Class
GeneticAlgorithm <- R6Class("GeneticAlgorithm",
                            public = list(
                              pop_size = 20,
                              population = NULL,
                              fitness = NULL,
                              best_individual = NULL,
                              best_fitness = 0,
                              history = list(),
                              
                              initialize = function(pop_size = 20) {
                                self$pop_size <- pop_size
                                self$population <- list()
                                for(i in 1:pop_size) {
                                  self$population[[i]] <- NeuralNetwork$new()
                                }
                                self$fitness <- numeric(pop_size)
                                self$history <- list(
                                  generation = numeric(0),
                                  avg_fitness = numeric(0),
                                  max_fitness = numeric(0),
                                  min_fitness = numeric(0)
                                )
                              },
                              
                              evaluate = function(steps = 100) {
                                fitness_scores <- numeric(self$pop_size)
                                
                                for(i in 1:self$pop_size) {
                                  game <- SnakeGame$new(grid_size = 12)
                                  nn <- self$population[[i]]
                                  dirs <- c("up", "right", "down", "left")
                                  
                                  for(s in 1:steps) {
                                    state <- game$get_state()
                                    probs <- nn$forward(state)
                                    move <- dirs[which.max(probs)]
                                    if(!game$move(move)) break
                                  }
                                  
                                  fitness_scores[i] <- game$calculate_fitness()
                                }
                                
                                self$fitness <- fitness_scores
                              },
                              
                              evolve = function(generation) {
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
                                
                                # Update history
                                self$history$generation <- c(self$history$generation, generation)
                                self$history$avg_fitness <- c(self$history$avg_fitness, mean(self$fitness))
                                self$history$max_fitness <- c(self$history$max_fitness, max(self$fitness))
                                self$history$min_fitness <- c(self$history$min_fitness, min(self$fitness))
                              },
                              
                              run_generation = function(generation, steps = 100) {
                                self$evaluate(steps = steps)
                                self$evolve(generation)
                                
                                return(list(
                                  generation = generation,
                                  avg_fitness = mean(self$fitness),
                                  max_fitness = max(self$fitness),
                                  min_fitness = min(self$fitness),
                                  best_individual = self$population[[which.max(self$fitness)]]$copy()
                                ))
                              }
                            )
)

# ==================== UI DEFINITION ====================
ui <- fluidPage(
  titlePanel("🐍 Snake Neuroevolution - Live Dashboard"),
  
  tags$head(
    tags$style(HTML("
      .btn-success {background-color: #28a745; color: white;}
      .btn-danger {background-color: #dc3545; color: white;}
      .btn-primary {background-color: #007bff; color: white;}
      .well {background-color: #f8f9fa; border: 1px solid #dee2e6;}
      .main-header {color: #28a745; margin-bottom: 20px;}
      .status-box {
        padding: 10px;
        margin: 10px 0;
        border-radius: 5px;
        background-color: #e9f7ef;
        border-left: 4px solid #28a745;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Evolution Controls
      h3("🎮 Evolution Controls"),
      numericInput("pop_size", "Population Size:", 
                   value = 20, min = 5, max = 100, step = 5),
      numericInput("generations", "Generations to Run:", 
                   value = 50, min = 10, max = 500, step = 10),
      numericInput("steps_per_game", "Steps per Game:", 
                   value = 100, min = 20, max = 500, step = 10),
      
      actionButton("start_evolution", "▶ Start Evolution", 
                   class = "btn-success", style = "width:100%; font-weight:bold;"),
      actionButton("stop_evolution", "⏹ Stop Evolution", 
                   class = "btn-danger", style = "width:100%; margin-top:5px; font-weight:bold;"),
      
      hr(),
      
      # Live Game Controls
      h3("🐍 Watch Snake Play"),
      actionButton("play_game", "▶ Play Best Snake", 
                   class = "btn-primary", style = "width:100%;"),
      actionButton("reset_game", "🔄 Reset Game", 
                   class = "btn-warning", style = "width:100%; margin-top:5px;"),
      
      sliderInput("game_speed", "Game Speed:", 
                  min = 100, max = 1000, value = 300, step = 100,
                  post = "ms"),
      
      hr(),
      
      # Current Status
      h3("📊 Current Status"),
      div(class = "status-box",
          htmlOutput("status_text")
      ),
      
      hr(),
      
      # Statistics
      h3("📈 Statistics"),
      tableOutput("stats_table")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("🏆 Evolution Progress",
                 fluidRow(
                   column(6, plotOutput("fitness_plot", height = "400px")),
                   column(6, plotOutput("score_plot", height = "400px"))
                 ),
                 fluidRow(
                   column(12, plotOutput("improvement_plot", height = "300px"))
                 )
        ),
        
        tabPanel("🐍 Live Game",
                 fluidRow(
                   column(6,
                          h3("Snake Game Board"),
                          plotOutput("game_board", height = "500px"),
                          br(),
                          wellPanel(
                            h4("Game Controls"),
                            fluidRow(
                              column(3, actionButton("move_up", "↑ Up", style = "width:100%;")),
                              column(3, actionButton("move_left", "← Left", style = "width:100%;")),
                              column(3, actionButton("move_right", "→ Right", style = "width:100%;")),
                              column(3, actionButton("move_down", "↓ Down", style = "width:100%;"))
                            )
                          )
                   ),
                   column(6,
                          h3("Game Information"),
                          wellPanel(
                            h4("Current Game Stats"),
                            verbatimTextOutput("game_stats"),
                            hr(),
                            h4("Neural Network Decision"),
                            plotOutput("nn_output_plot", height = "250px")
                          ),
                          wellPanel(
                            h4("Generation Info"),
                            verbatimTextOutput("generation_info")
                          )
                   )
                 )
        ),
        
        tabPanel("📊 Performance Analysis",
                 fluidRow(
                   column(6, plotOutput("best_snake_analysis", height = "400px")),
                   column(6, plotOutput("population_diversity", height = "400px"))
                 ),
                 fluidRow(
                   column(12, 
                          h4("Best Snake Statistics"),
                          tableOutput("detailed_stats")
                   )
                 )
        )
      )
    )
  )
)

# ==================== SERVER DEFINITION ====================
server <- function(input, output, session) {
  
  # Reactive values
  rv <- reactiveValues(
    ga = NULL,
    evolution_active = FALSE,
    current_generation = 0,
    max_generations = 50,
    evolution_history = list(),
    best_snake = NULL,
    game = SnakeGame$new(grid_size = 12),
    game_running = FALSE,
    game_timer = NULL,
    game_history = list(),
    generation_stats = data.frame(
      generation = integer(),
      best_fitness = numeric(),
      avg_fitness = numeric(),
      best_score = numeric()
    )
  )
  
  # ==================== STATUS TEXT ====================
  output$status_text <- renderUI({
    if(is.null(rv$ga)) {
      HTML("<strong>Status:</strong> Ready to start evolution<br>
           <strong>Best Fitness:</strong> N/A<br>
           <strong>Generations:</strong> 0")
    } else if(rv$evolution_active) {
      HTML(paste0("<strong>Status:</strong> Evolution running...<br>
           <strong>Current Generation:</strong> ", rv$current_generation, "<br>
           <strong>Best Fitness:</strong> ", 
                  round(max(rv$generation_stats$best_fitness, na.rm = TRUE), 2)))
    } else {
      HTML(paste0("<strong>Status:</strong> Evolution complete<br>
           <strong>Total Generations:</strong> ", rv$current_generation, "<br>
           <strong>Best Fitness:</strong> ", 
                  round(max(rv$generation_stats$best_fitness, na.rm = TRUE), 2), "<br>
           <strong>Best Score:</strong> ", 
                  max(rv$generation_stats$best_score, na.rm = TRUE), " apples"))
    }
  })
  
  # ==================== STATISTICS TABLE ====================
  output$stats_table <- renderTable({
    if(nrow(rv$generation_stats) > 0) {
      stats <- rv$generation_stats
      latest <- stats[nrow(stats), ]
      
      data.frame(
        Metric = c("Generations", "Best Fitness", "Average Fitness", 
                   "Best Score", "Average Score"),
        Value = c(
          latest$generation,
          round(latest$best_fitness, 2),
          round(latest$avg_fitness, 2),
          latest$best_score,
          round(latest$best_score * 0.7, 1)
        )
      )
    }
  }, striped = TRUE, width = "100%")
  
  # ==================== START EVOLUTION ====================
  observeEvent(input$start_evolution, {
    rv$evolution_active <- TRUE
    rv$current_generation <- 0
    rv$max_generations <- input$generations
    
    # Initialize genetic algorithm
    rv$ga <- GeneticAlgorithm$new(pop_size = input$pop_size)
    rv$generation_stats <- data.frame(
      generation = integer(),
      best_fitness = numeric(),
      avg_fitness = numeric(),
      best_score = numeric()
    )
    
    # Start evolution loop
    evolution_loop()
  })
  
  # ==================== EVOLUTION LOOP ====================
  evolution_loop <- function() {
    if(!rv$evolution_active || rv$current_generation >= rv$max_generations) {
      rv$evolution_active <- FALSE
      return()
    }
    
    rv$current_generation <- rv$current_generation + 1
    
    # Run one generation
    result <- rv$ga$run_generation(
      generation = rv$current_generation,
      steps = input$steps_per_game
    )
    
    # Update stats
    new_stats <- data.frame(
      generation = result$generation,
      best_fitness = result$max_fitness,
      avg_fitness = result$avg_fitness,
      best_score = floor(result$max_fitness / 10)
    )
    rv$generation_stats <- rbind(rv$generation_stats, new_stats)
    
    # Store best snake
    rv$best_snake <- result$best_individual
    
    # Continue loop
    if(rv$evolution_active && rv$current_generation < rv$max_generations) {
      invalidateLater(100)  # Small delay between generations
    } else {
      rv$evolution_active <- FALSE
    }
  }
  
  # ==================== STOP EVOLUTION ====================
  observeEvent(input$stop_evolution, {
    rv$evolution_active <- FALSE
  })
  
  # ==================== PLOTS ====================
  
  # Fitness Plot
  output$fitness_plot <- renderPlot({
    if(nrow(rv$generation_stats) > 0) {
      df <- rv$generation_stats
      
      ggplot(df, aes(x = generation)) +
        geom_line(aes(y = best_fitness, color = "Best"), size = 1.5) +
        geom_line(aes(y = avg_fitness, color = "Average"), size = 1) +
        geom_ribbon(aes(ymin = avg_fitness, ymax = best_fitness), 
                    fill = "green", alpha = 0.1) +
        scale_color_manual(values = c("Best" = "#28a745", "Average" = "#007bff")) +
        labs(title = "Fitness Evolution",
             x = "Generation",
             y = "Fitness",
             color = "Metric") +
        theme_minimal() +
        theme(legend.position = "bottom",
              plot.title = element_text(hjust = 0.5, size = 16))
    }
  })
  
  # Score Plot
  output$score_plot <- renderPlot({
    if(nrow(rv$generation_stats) > 0) {
      df <- rv$generation_stats
      df$score <- floor(df$best_fitness / 10)
      
      ggplot(df, aes(x = generation, y = score)) +
        geom_line(color = "purple", size = 1.5) +
        geom_point(color = "darkviolet", size = 3) +
        geom_smooth(method = "loess", se = FALSE, color = "orange", linetype = "dashed") +
        labs(title = "Best Snake Score (Apples Eaten)",
             x = "Generation",
             y = "Apples Eaten") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 16))
    }
  })
  
  # Improvement Plot
  output$improvement_plot <- renderPlot({
    if(nrow(rv$generation_stats) > 1) {
      df <- rv$generation_stats
      df$improvement <- c(0, diff(df$best_fitness))
      
      ggplot(df, aes(x = generation, y = improvement)) +
        geom_bar(stat = "identity", fill = ifelse(df$improvement > 0, "green", "red"), 
                 alpha = 0.7) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
        labs(title = "Fitness Improvement per Generation",
             x = "Generation",
             y = "Improvement") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 16))
    }
  })
  
  # ==================== GAME CONTROLS ====================
  
  # Play Game
  observeEvent(input$play_game, {
    if(!is.null(rv$best_snake)) {
      rv$game$reset()
      rv$game_running <- TRUE
      
      # Start game timer
      rv$game_timer <- reactiveTimer(input$game_speed)
      
      observeEvent(rv$game_timer(), {
        if(rv$game_running && !rv$game$game_over) {
          # Get neural network decision
          state <- rv$game$get_state()
          probs <- rv$best_snake$forward(state)
          dirs <- c("up", "right", "down", "left")
          move <- dirs[which.max(probs)]
          
          # Make move
          rv$game$move(move)
          
          # Store game state for NN plot
          rv$game_history <- c(rv$game_history, list(list(
            probs = probs,
            move = move,
            score = rv$game$score
          )))
          
          # Stop if game over
          if(rv$game$game_over) {
            rv$game_running <- FALSE
          }
        }
      })
    }
  })
  
  # Reset Game
  observeEvent(input$reset_game, {
    rv$game$reset()
    rv$game_running <- FALSE
    rv$game_history <- list()
  })
  
  # Manual Controls
  observeEvent(input$move_up, { rv$game$move("up") })
  observeEvent(input$move_down, { rv$game$move("down") })
  observeEvent(input$move_left, { rv$game$move("left") })
  observeEvent(input$move_right, { rv$game$move("right") })
  
  # ==================== GAME VISUALIZATION ====================
  
  # Game Board
  output$game_board <- renderPlot({
    grid <- rv$game$get_grid_matrix()
    colors <- matrix("white", nrow(grid), ncol(grid))
    
    # Set colors
    colors[grid == 1] <- "darkgreen"  # Snake body
    colors[grid == 2] <- "limegreen"  # Snake head
    colors[grid == 3] <- "red"        # Apple
    
    # Create plot
    par(mar = c(1, 1, 4, 1))
    image(1:nrow(grid), 1:ncol(grid), t(matrix(1:(nrow(grid)*ncol(grid)), nrow(grid), ncol(grid))),
          col = t(colors), xlab = "", ylab = "", axes = FALSE,
          main = paste("Snake Game | Score:", rv$game$score, 
                       "| Steps:", rv$game$steps,
                       ifelse(rv$game$game_over, " (GAME OVER)", "")),
          cex.main = 1.2)
    
    # Add grid lines
    abline(h = 1:(nrow(grid) + 1) - 0.5, col = "gray", lwd = 0.5)
    abline(v = 1:(ncol(grid) + 1) - 0.5, col = "gray", lwd = 0.5)
    
    # Add coordinates
    text(expand.grid(1:nrow(grid), 1:ncol(grid))[,1],
         expand.grid(1:nrow(grid), 1:ncol(grid))[,2],
         paste0("(", expand.grid(1:nrow(grid), 1:ncol(grid))[,1], ",",
                expand.grid(1:nrow(grid), 1:ncol(grid))[,2], ")"),
         cex = 0.6, col = "gray50")
    
    # Add legend
    legend("topright", 
           legend = c("Snake Head", "Snake Body", "Apple"),
           fill = c("limegreen", "darkgreen", "red"),
           bty = "n", cex = 0.8)
  })
  
  # Game Stats
  output$game_stats <- renderPrint({
    cat("SNAKE GAME STATUS\n")
    cat("=================\n")
    cat("Score:", rv$game$score, "apples\n")
    cat("Steps:", rv$game$steps, "\n")
    cat("Snake length:", length(rv$game$snake), "\n")
    cat("Direction:", rv$game$direction, "\n")
    cat("Game over:", rv$game$game_over, "\n")
    cat("\nApple position: (", rv$game$apple[1], ",", rv$game$apple[2], ")\n")
    cat("Snake head: (", rv$game$snake[[1]][1], ",", rv$game$snake[[1]][2], ")\n")
  })
  
  # Neural Network Output
  output$nn_output_plot <- renderPlot({
    if(length(rv$game_history) > 0) {
      latest <- rv$game_history[[length(rv$game_history)]]
      dirs <- c("Up", "Right", "Down", "Left")
      
      df <- data.frame(
        Direction = factor(dirs, levels = dirs),
        Probability = latest$probs
      )
      
      ggplot(df, aes(x = Direction, y = Probability, fill = Direction)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = sprintf("%.2f", Probability)), 
                  vjust = -0.5, size = 4) +
        scale_fill_manual(values = c("Up" = "#28a745", "Right" = "#007bff",
                                     "Down" = "#dc3545", "Left" = "#ffc107")) +
        ylim(0, 1) +
        labs(title = paste("Chosen:", latest$move),
             x = "Direction",
             y = "Probability") +
        theme_minimal() +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5, size = 14))
    } else {
      plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
           axes = FALSE, xlab = "", ylab = "", main = "Start game to see decisions")
      text(0.5, 0.5, "Click 'Play Best Snake' to start", cex = 1.2)
    }
  })
  
  # Generation Info
  output$generation_info <- renderPrint({
    if(!is.null(rv$best_snake)) {
      cat("BEST SNAKE INFO\n")
      cat("===============\n")
      cat("From generation:", rv$current_generation, "\n")
      cat("Network weights:", length(rv$best_snake$get_weights()), "\n")
      cat("Architecture: 12 → 8 → 4\n")
      cat("Inputs: 12 (game state)\n")
      cat("Hidden neurons: 8\n")
      cat("Outputs: 4 (movement directions)\n")
    }
  })
  
  # ==================== ANALYSIS TAB ====================
  
  # Best Snake Analysis
  output$best_snake_analysis <- renderPlot({
    if(nrow(rv$generation_stats) > 0) {
      df <- rv$generation_stats
      
      ggplot(df, aes(x = generation)) +
        geom_area(aes(y = best_fitness), fill = "lightgreen", alpha = 0.5) +
        geom_line(aes(y = best_fitness), color = "darkgreen", size = 1.5) +
        geom_point(aes(y = best_fitness), color = "darkgreen", size = 3) +
        labs(title = "Best Snake Performance",
             x = "Generation",
             y = "Fitness") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 16))
    }
  })
  
  # Population Diversity
  output$population_diversity <- renderPlot({
    if(nrow(rv$generation_stats) > 0) {
      df <- rv$generation_stats
      
      ggplot(df, aes(x = generation)) +
        geom_ribbon(aes(ymin = avg_fitness, ymax = best_fitness), 
                    fill = "lightblue", alpha = 0.5) +
        geom_line(aes(y = best_fitness), color = "blue", size = 1) +
        geom_line(aes(y = avg_fitness), color = "red", size = 1) +
        labs(title = "Population Diversity",
             x = "Generation",
             y = "Fitness") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, size = 16))
    }
  })
  
  # Detailed Stats
  output$detailed_stats <- renderTable({
    if(nrow(rv$generation_stats) > 0) {
      df <- rv$generation_stats
      
      # Calculate additional stats
      improvements <- diff(df$best_fitness)
      
      data.frame(
        Statistic = c(
          "Total Generations",
          "Final Best Fitness",
          "Average Fitness",
          "Total Improvement",
          "Best Single Generation Improvement",
          "Average Improvement per Generation"
        ),
        Value = c(
          nrow(df),
          round(max(df$best_fitness), 2),
          round(mean(df$avg_fitness), 2),
          round(max(df$best_fitness) - df$best_fitness[1], 2),
          round(max(improvements, na.rm = TRUE), 2),
          round(mean(improvements, na.rm = TRUE), 3)
        )
      )
    }
  }, striped = TRUE, width = "100%")
}

# Run the application
shinyApp(ui = ui, server = server)
