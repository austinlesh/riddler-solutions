## The main part of this Riddler is a nice logic exercise.
## The extra credit (below) can be solved with a fun little Markov chain.

directions <- c("N", "S", "E", "W")
start <- c(1, 0, 0, 0)
names(start) <- directions

turn_prob <- matrix(rep(1/3, 16), nrow = 4, ncol = 4)
turn_prob[1,2] <- turn_prob[2,1] <- turn_prob[3,4] <- turn_prob[4,3] <- 0

rownames(turn_prob) <- colnames(turn_prob) <- directions

turns <- function(n){
    position <- start
    i = 0
    while(i < n){
        position <- position %*% turn_prob
        i = i + 1
    }
    return(position)
}

turns(10)

## The probability you're still driving north after 10 turns is a little
## over 1/4, and the probability for all other directions are equal and
## just a little bit under 1/4.

