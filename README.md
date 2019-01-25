# Project-work
---
## Machine learning in Game theory(Tennis game)
---
Introduction(Overview):
---
Machine learning is the ability for a computer to learn and make a rational decisions according to previous experience and circumstances observed during the time of execution.Machine learning can be categorize into three groups namely; supervised, unsupervised and reinforcement learning. Machine learning has become a widely known discipline in the field of science and also has it application in divest areas such as image recognition, speech recognition, fraud detection in the financial sectors and finally in game theory. The fundamental theories of learning and intelligence is through interaction with the environment and feedbacks. The application of learning in problem with large search state spaces has become popular in machine learning especially reinforcement learning and game theory. Machine learning in game is a technique of creating a game playing program where the learn how to converge to the optimal strategy based on its past experience or feedbacks received from previous game play. 

Background of the game 
---
The project seek to address the tennis on paper game. a two-player game where both players have complete information about the game. Here is the rules to the game;

The field of the game is represented by seven fields and central line. The nature is shown in this form (-3,-2,-1,0,1,2,3) where negative fields belonging to player 1, positive fields belong to player 2. There is a centreline(0) indicating the initial position of the ball start of play by both players. The Figure 1.1 represents the layout of the game indicating the possible position to be occupy by the ball during the period of play. 
 In each play, both players simultaneously and independently make a bid from their given points (between 1 and the current points), the ball is move towards the player with a smaller bid and after that both points are deducted from their current point; when both players bid the same number then there is a tied so the ball remains at it position without be move. The main objective of the game is to move the ball to the third section of the opponent. The game ends if the ball gets to the third position of any player then that player has lost the game and also if both players have run out of points. The game is lost to a player when the ball is located in the area of the player after outcome of play.

 At the end of the game, the following are the fundamental rules of the game and how score are allocated at the end of play;


- The game comes to end if the ball moves beyond the field of any player or if both players run out of point meaning having no point to make draws from. 
- In both cases, the game is lost for the player whose side the ball is located at the end of play.
- A score of two points is allocated if the moves beyond the playing field, otherwise a score of one is allocated.

The link provide detail description and the mathematics behind the game [https://en.wikipedia.org/wiki/Tennis_(paper_game)](#Tennis game)