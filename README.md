
# The game

There are two players on a map. The symbol 'x' represents an obstacle,
all other squares on the map are free for passage.

A player can move on the map, each move costs a turn, and shoot, which
costs two turns. If during the second turn the line between the
shooting player and the other one does not cross an obstacle, the
latter is considered shot. One can also skip the turn.

# The training

A playing strategy is implemented as a three-layer artificial neural
network. The inputs are both players' positions, whether they have
started to shoot and number of shots left. The output is the turn that
the player makes.

A collection of random ANNs is produced, and then is put to play with
each other. They play until one of the players is dead or a certain
number of turns elapse. The networks are then arranged according to
their score and the top N networks are selected, who are randomly
modified and set to play with another collection of random networks. 

# Usage

All functions in Network.hs accept a parameter which tells whether the
initial position of players is random or fixed (the players are then
placed at the opposite corners of the map).

The function trainNetAndShowProgress returns a trained network that
can be saved (writeNetFromFile) and then loaded (readNetFromFile). 

The function drawBattle shows how the networks play against each
other. 


