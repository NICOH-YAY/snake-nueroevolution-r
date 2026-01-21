Snake Neuroevolution in R 🐍🧬

A vibe-coded machine learning experiment built while learning Shiny, genetic algorithms, and agent-based modeling.

This project is a genetic algorithm that evolves a machine learning model to play the game Snake. Each model controls a snake, observes a simplified version of the game world, and decides which direction to move. Over many generations, better-performing snakes survive and reproduce, while weaker preforming snakes are discarded. 
Imprvoment happens through evolutionary pressure and random mutation consistent with darwinism. 

This model was my first exposure to using Shiny to host the interface on a server and other modeling techniques. Development was done in consultation with DeepSeek R1, which was primarily used to help debug R code and scaffold parts of the Shiny server logic.

What is  being evolved?:

Each snake is controlled by a neural network agent. In simple terms, this is a small mathematical model that takes inputs and produces decisions.

For every step of the game:

The agent observes a compact state representation of the environment
(for example: where the apple is relative to the snake, whether moving in certain directions is dangerous, and the snake’s current heading)
The neural network processes this information
The agent outputs a movement decision: up, right, down, or left

Why a genetic algorithm? 🌱:

I chose a genetic algorithm because I’ve always been fascinated by Darwinian evolution and the idea that complex, adaptive behavior can emerge from simple rules over time.

In biology, most organisms alive today are the result of:
random mutations
selection pressure
long time horizons

Small, often meaningless changes can accumulate into dramatic behavioral shifts.
Genetic algorithms mimic this process in code:
A population of agents is created with random “brains”
Each agent is tested in the environment
The best performers are selected
Their parameters are copied and slightly mutated
Over many generations, performance improves

I found this approach especially compelling because it does not require the model to “know” what it’s doing (nor me, so it's a great entry way! I thought of it as "brute-forcing") It simply survives if it works.

How the system works 
Environment

The Snake game is implemented from scratch in R using an object-oriented structure. The environment handles movement, collisions, apple spawning, and game termination.

Agent (Neural Network)
Each agent is a small feedforward neural network. This is a layered math function that transforms inputs into outputs. There is no training in the usual sense, only weight mutation.
Evolution loop
Create a population of random agents
Let each agent play Snake for a fixed number of steps
Assign a fitness score based mainly on apples eaten
Keep the best-performing agents
Mutate their weights to create the next generation

Repeat

Visualization (Shiny)
A Shiny dashboard is used to visualize:
fitness over generations
estimated apples eaten
the behavior of the best-performing snake
the structure of the neural network

What I learned 📚💡!!!!

This project taught me more than I expected, especially because things frequently broke.

Some key takeaways:
Reward design matters more than model complexity
Small changes to the fitness function completely altered agent behavior. Poor reward definitions encouraged survival without progress, while clearer objectives led to meaningful learning. 

Evolutionary methods behave very differently from gradient-based learning
Improvement is far from smooth, progress is noisy and sometimes regresses before improving again.

“Vibe coding” still requires theory!
Even when freely playing around, inconsistent assumptions (made from deepkseek!) (like how fitness maps to score) quickly became bugs I had to search for and correct for.  
For example, when I ran into a road-block, I asked Deepseek to help fix syntax error and it altered the fitness function to rewarding efficiency. WHich is bad because anyone who watched someone beat "Google Snake" knows you can't just go to the each apple in the least amount of steps. 

Limitations and what needs improvement 🔧
This is a rudimentary model... that’s intentional. There are several technical limitations worth noting:
Fitness design is still simplistic
While apples eaten is a clear objective, real Snake gameplay often rewards non-greedy behavior.
No memory or temporal awareness
The agent reacts only to the current state.
Shiny performance limitations
Evolution currently runs synchronously. Asynchronous execution or batch-based visualization would improve responsiveness.
<img width="637" height="656" alt="image" src="https://github.com/user-attachments/assets/a262f405-fd9a-4cee-98c7-c2c0fdbc2b30" /> <img width="866" height="923" alt="image" src="https://github.com/user-attachments/assets/e9592f5a-efcc-4898-9478-d2639c48002c" /> <img width="872" height="926" alt="image" src="https://github.com/user-attachments/assets/4b18a8bf-9595-4405-a766-3050e25d8d8d" />




