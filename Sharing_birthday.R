# -------------------------------------------- #
# Expeced number of people sharing birthday    #
# Author: Guillem Perdigó                      #
# 10.03.2019 - Version 2                       #
# -------------------------------------------- #

library(ggplot2)
library(ggthemes)
library(scales)
library(ggpubr)

# Expected number of people to share birthday with someone else ####

# Source: https://math.stackexchange.com/questions/35791/birthday-problem-expected-number-of-collisions

n <- 1:450 # population size
exp_ppl_sharing <- n*(1-(1-(1/365.25))^(n-1)) # formula

bd_sharing <- data.frame(
  n,
  exp_ppl_sharing
)

# Expected number of people sharing birthday for Ubiqum's team (n = 35)
bd_sharing$exp_ppl_sharing[bd_sharing$n==35] #3.11507

tail(bd_sharing, 1)
# n           exp_ppl_sharing
# 100         23.78448
# 250         123.7413
# 365         230.5398
# 500         372.8193
# 1000        935.4776
# 2000        1991.697
# 4000        3999.931

# Line plot
ggplot(data = bd_sharing, aes(n, exp_ppl_sharing)) + 
  ggtitle("Expected number of people sharing birthday with someone else") +
  ylab("Exp. people sharing birthday") +
  xlab("Population size") +
  geom_line() + 
  geom_rangeframe() +
  theme_tufte() +
  grids(linetype = "dashed")


# Debate 1:
# Are birthdays uniformally distributed? Seems like they aren't: https://www.panix.com/~murphy/bday.html

# Debate 2: expected vs. most likely - how do they differ? We'll know by running some simulations...

# Simulations ####
DF_bd <- function(n, nsims=100000, feb29=TRUE) {
  # Using nsims simulations,for a population of n people
  
  bdays <- 1:366 
  # Feb 29 represented as day 366
  # We'll sample other days 4 times as often compared to day 366
  probs <- c(rep(4,365),1)
  
  if(!feb29) {
    # drop Feb 29 from bdays and probs
    bdays <- bdays[-366]
    probs <- probs[-366]
  }
  probs <- probs/sum(probs)
  mySims <<- as.data.frame(replicate(nsims, sample(bdays, n, prob=probs, replace=TRUE)))
}

# Run nsim simulations for n people
set.seed(888)
n = 35
nsims = 10000
DF_bd(n = n, nsims = nsims)

# Create a list with table of frequencies for every simulation
bb <- apply(mySims, 2, function(x)as.data.frame(table(x)))

# For every table of frequencies, we sum all the elements that are not 1
bbb <- c(0) #empty vector
for (i in 1:length(bb)) {
  bbb[i] <- sum(bb[[i]]$Freq[bb[[i]]$Freq!=1])
}

# Expected number of people sharing birthday
mean(bbb) #for n = 35, 3.1254
table(bbb)

# Bar plot
df_bbb <- as.data.frame(bbb) #df class needed for ggplot

ggplot(df_bbb, aes(bbb)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), fill = "#FF6666") +
  ggtitle("Expected number of people sharing birthday with someone else", 
          subtitle = paste0("Size of the population = ", n, "  |  Simulations = ", nsims)) +
  scale_x_discrete(name = "Number of people sharing birthday", limits = c(0:n)) + # limits fail to appear for large n
  scale_y_continuous(name = "Frequency (%)", labels = percent) +
  theme_tufte()


