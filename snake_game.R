# src/snake_game.R - FIXED VERSION
SnakeGame <- R6Class("SnakeGame",
                     public = list(
                       grid_size = 15,
                       snake = NULL,
                       apple = NULL,
                       direction = "right",
                       next_direction = "right",
                       score = 0,
                       steps = 0,
                       game_over = FALSE,
                       apples_eaten = 0,
                       max_length = 3,
                       moves_without_apple = 0,
                       efficiency_score = 0,
                       
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
                       
                       update_direction = function(new_dir) {
                         if((self$direction == "up" && new_dir == "down") ||
                            (self$direction == "down" && new_dir == "up") ||
                            (self$direction == "left" && new_dir == "right") ||
                            (self$direction == "right" && new_dir == "left")) {
                           return(FALSE)
                         }
                         self$next_direction <- new_dir
                         return(TRUE)
                       },
                       
                       move = function() {
                         if(self$game_over) return(FALSE)
                         
                         self$direction <- self$next_direction
                         self$steps <- self$steps + 1
                         self$moves_without_apple <- self$moves_without_apple + 1
                         
                         head <- self$snake[[1]]
                         
                         new_head <- switch(self$direction,
                                            "up"    = c(head[1], head[2] - 1),
                                            "right" = c(head[1] + 1, head[2]),
                                            "down"  = c(head[1], head[2] + 1),
                                            "left"  = c(head[1] - 1, head[2])
                         )
                         
                         # Check walls
                         if(new_head[1] < 1 || new_head[1] > self$grid_size ||
                            new_head[2] < 1 || new_head[2] > self$grid_size) {
                           self$game_over <- TRUE
                           return(FALSE)
                         }
                         
                         # Check self collision
                         for(i in 1:length(self$snake)) {
                           if(all(new_head == self$snake[[i]])) {
                             self$game_over <- TRUE
                             return(FALSE)
                           }
                         }
                         
                         # Add new head
                         self$snake <- c(list(new_head), self$snake)
                         
                         # Check apple
                         if(all(new_head == self$apple)) {
                           self$score <- self$score + 1
                           self$apples_eaten <- self$apples_eaten + 1
                           self$moves_without_apple <- 0
                           
                           if(self$steps > 0 && self$apples_eaten > 0) {
                             efficiency <- 100 / (self$steps / self$apples_eaten)
                             self$efficiency_score <- self$efficiency_score + efficiency
                           }
                           
                           if(length(self$snake) > self$max_length) {
                             self$max_length <- length(self$snake)
                           }
                           
                           self$spawn_apple()
                         } else {
                           self$snake <- self$snake[-length(self$snake)]
                         }
                         
                         if(self$moves_without_apple > self$grid_size * 2) {
                           self$game_over <- TRUE
                           return(FALSE)
                         }
                         
                         return(TRUE)
                       },
                       
                       get_state = function() {
                         if(self$game_over) return(rep(0, 12))
                         
                         head <- self$snake[[1]]
                         
                         c(
                           (self$apple[1] - head[1]) / self$grid_size,
                           (self$apple[2] - head[2]) / self$grid_size,
                           
                           self$check_danger(head[1], head[2] - 1),
                           self$check_danger(head[1] + 1, head[2]),
                           self$check_danger(head[1], head[2] + 1),
                           self$check_danger(head[1] - 1, head[2]),
                           
                           as.numeric(self$direction == "up"),
                           as.numeric(self$direction == "right"),
                           as.numeric(self$direction == "down"),
                           as.numeric(self$direction == "left"),
                           
                           length(self$snake) / 20,
                           self$score / 10
                         )
                       },
                       
                       check_danger = function(x, y) {
                         if(x < 1 || x > self$grid_size || y < 1 || y > self$grid_size) {
                           return(1)
                         }
                         
                         for(seg in self$snake) {
                           if(all(c(x, y) == seg)) {
                             return(1)
                           }
                         }
                         
                         return(0)
                       },
                       
                       # Simple working fitness function
                       calculate_fitness = function() {
                         if(self$game_over && self$steps < 10) {
                           return(1)
                         }
                         
                         fitness <- self$apples_eaten * 100 + self$steps * 0.1
                         
                         if(self$steps > 0) {
                           efficiency <- self$apples_eaten / self$steps
                           fitness <- fitness + (efficiency * 50)
                         }
                         
                         return(max(fitness, 1))
                       },
                       
                       # FIXED: Added = function() here
                       get_display_info = function() {
                         list(
                           score = self$score,
                           length = length(self$snake),
                           steps = self$steps,
                           apples_eaten = self$apples_eaten,
                           max_length = self$max_length,
                           game_over = self$game_over,
                           head_pos = self$snake[[1]],
                           apple_pos = self$apple,
                           direction = self$direction
                         )
                       },
                       
                       get_grid_matrix = function() {
                         grid <- matrix(0, self$grid_size, self$grid_size)
                         
                         for(i in 1:length(self$snake)) {
                           seg <- self$snake[[i]]
                           if(i == 1) {
                             grid[seg[1], seg[2]] <- 2
                           } else {
                             grid[seg[1], seg[2]] <- 1
                           }
                         }
                         
                         if(!is.null(self$apple)) {
                           grid[self$apple[1], self$apple[2]] <- 3
                         }
                         
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
                         self$next_direction <- "right"
                         self$score <- 0
                         self$steps <- 0
                         self$game_over <- FALSE
                         self$apples_eaten <- 0
                         self$max_length <- 3
                         self$moves_without_apple <- 0
                         self$efficiency_score <- 0
                         
                         self$spawn_apple()
                       }
                     )
)
