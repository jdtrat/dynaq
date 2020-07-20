# Set up environments -----------------------------------------------------------

#reward probabilities used to determine the presence or absence of a reward whe nvisiting certain images.
rewardProb <- new.env(parent = emptyenv())

#used for transition information. Will include the transition function,
#transition table, and trackTrans (a more insightful version of the transition
#table which keeps track of the running proportion of states observed as a
#result of the first action/transition). Will also be used for the numSims
#counter that is used by updateTransFunction for tracking the number of
#simulations.
trans <- new.env(parent = emptyenv())


#Used for the Qtable and the QL/QR
Q <- new.env(parent = emptyenv())
