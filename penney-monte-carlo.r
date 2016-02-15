#A Monte-Carlo simulation to solve the Penney's game, as described here : http://blog.gtorangebuilder.com/2016/02/gto-brainteaser-10-flipping-coins.html

library(ggplot2)

#The game, it stops when the last three flips are "Head Head Tail" (0,0,1) or "Head Tail Tail" (0,1,1). It returns the last three flips.
game <- function(){
  #head = 0, tail = 1
  a <- sample(c(0,1),1)
  b <- sample(c(0,1),1)
  c <- sample(c(0,1),1)
  
  while(!all(c(a,b,c) == c(0,0,1)) & !all(c(a,b,c) == c(0,1,1)))
    {a <- b
    b <- c
    c <- sample(c(0,1),1)}
return(paste(a, b, c))}


#The simulation, it plays n games and return the profit
simulation <- function(n){
  simul <- replicate(n, game())
  simul <- simul == '0 1 1'
  simul[simul] <- 105
  simul[!simul] <- -100
  return(sum(simul))}

#The monte carlot simulation, it plays a number of simulations of n games and save the profits in a vector.
monte <- replicate(100000, simulation(100))

#The sample mean that estimate the Expected Value
mean(monte)/100

#The sample distribution, to get a sense of the variance
ggplot(data.frame(x = monte), aes(x=x)) + geom_histogram()
