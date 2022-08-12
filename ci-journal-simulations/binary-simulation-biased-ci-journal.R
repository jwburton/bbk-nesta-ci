#### script for simulations with binary prediction context ###

# clear environment ----
rm(list = ls())

# install + load packages  ----
pkgs <- c("plyr", "igraph", "tidyverse", 
          "reshape2", "plotrix", "ggh4x")

for (p in pkgs){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}
rm(p, pkgs)

# functions ----

# Integrating evidence from the world with prior when the truth is 1 (i.e., h is true)
learn_H <- function(prior, sensi, speci, e){
  
  # stop and print error if evidence is non-binary
  if(e != 0 & e != 1){
    stop("Evidence received is neither 0 nor 1. Only binary evidence is accepted.")
  }
  
  out <- ifelse(e == 1, 
                
                # if evidence is 1 (i.e., suggests h is true)...
                # P(h) * P(e|h) / [P(h) * P(e|h)] + [P(-h) * P(e|-h)]
                (prior*sensi) / ((prior*sensi) + ((1-prior)*(1-speci))), 
                
                # if evidence is 0 (i.e., suggests -h is true)...
                # P(h) * P(-e|h) / [P(-h) * P(-e|-h)] + [P(h) * P(-e|h)]
                (prior*(1-sensi)) / (((1-prior)*speci) + (prior*(1-sensi)))) 
  
  out
}

# Integrating evidence from the world with prior when the truth is 0 (i.e., -h is true)
learn_notH <- function(prior, sensi, speci, e){
  
  # stop and print error if evidence is non-binary
  if(e != 0 & e != 1){
    stop("Evidence received is neither 0 nor 1. Only binary evidence is accepted.")
  }
  
  out <- ifelse( e == 0, 
                 
                 # if evidence is 0, truth = -h           
                 # P(-h) * P(-e|-h) / P(-h) * P(-e|-h) + P(h) * P(-e|h)
                 ( (1-prior)*speci ) / ( ((1-prior)*speci) + (prior*(1-sensi))),
                 
                 # if evidence is 1, truth = -h
                 # P(-h) * P(e|-h) / [P(-h) * P(e|-h) + P(h) * P(e|h)]
                 ( (1-prior)*(1-speci) ) / ( ((1-prior)*(1-speci)) + (prior*sensi))) 
  
  1-out
  
}

# Feeds vectors of evidence through 'learn_H' function with starting prior = BR
feed_H <- function(vec){
  
  prior <- BR
  
  for (i in vec){
    res <- learn_H(prior, sensi, speci, i)
    prior <- res
  }
  return(prior)
}

# Feeds vectors of evidence through 'learn_notH' function with starting prior = BR
feed_notH <- function(vec){
  
  prior <- BR
  
  for (i in vec){
    res <- learn_notH(prior, sensi, speci, i)
    prior <- res
  }
  return(prior)
}

# Becker-DeGroot updating rule
becker_degroot <- function(prior,             # point belief estimate 
                           neighborsBeliefs,  # vector of neighbors' beliefs
                           truth){            # ground truth
  
  # if the agent has neighbors...
  if (!is.na(mean(neighborsBeliefs, na.rm = TRUE))){ 
    
    # calculate mean of neighbors' beliefs
    neighborsMean <- mean(neighborsBeliefs, na.rm = TRUE)
    
    # calculate absolute distance from the truth
    dist_truth <- (abs(prior-truth))
    
    # determine self-weight via regression from Becker et al., 2017, appendix, p. 33 + gaussian noise
    # with added gaussion noise, the correlation between dist_truth and self_weight is approx -0.23
    # i.e., more accurate = more resistant to social influence
    self_weight <- 0.74-(0.5*(dist_truth))+rnorm(1, m = 0, sd = 0.06)
    
    # apply standard degroot equation
    posterior <- (self_weight*prior) + ((1-self_weight)*neighborsMean) 
    
  } else {                                                           
    posterior <- prior # if agent has no neighbors, do not update
  }
  
  posterior
  
}

# Runs multiple specifications ala NetLogo -- 500 iterations each
runAll <- function(N_vec, n_rounds_vec, intervene_on_round_vec, BR_vec, n_sources_vec, sensi_vec, speci_vec){
  f <- function(N, n_rounds, intervene_on_round, BR, n_sources, sensi, speci){
    ldply(1:500, function(i) data.frame(iteration = i, binary_model(N, n_rounds, intervene_on_round, BR, n_sources, sensi, speci)))
  }
  vals <- expand.grid(N = N_vec, n_rounds = n_rounds_vec, intervene_on_round = intervene_on_round_vec, BR = BR_vec, n_sources = n_sources_vec,sensi = sensi_vec, speci = speci_vec)
  res <- Map(function(.N, .n_rounds, .intervene_on_round, .BR, .n_sources, .sensi, .speci){f(.N, .n_rounds, .intervene_on_round, .BR, .n_sources, .sensi, .speci)}, vals$N, vals$n_rounds, vals$intervene_on_round, vals$BR, vals$n_sources, vals$sensi, vals$speci)
  res <- do.call(rbind, res)
  row.names(res) <- NULL
  res
}

# Rewiring functions to manipulate adjacency matrices
## mean-extreme
me_rewire <- function(data, adjacency_matrix, current_beliefs_col_index){
  
  #order belief matrix so agent w/ lowest belief is on top
  data <- data[order(data[,current_beliefs_col_index]),] 
  
  #index extreme low and high agents
  extremeLo1 <- data[1,1] 
  extremeLo2 <- data[2,1] 
  extremeLo3 <- data[3,1] 
  
  extremeHi3 <- data[(nrow(data)-2),1] 
  extremeHi2 <- data[(nrow(data)-1),1] 
  extremeHi1 <- data[nrow(data),1] 
  
  #rewire
  if (mean(data[,current_beliefs_col_index]) > 0.5) {
    adjacency_matrix[extremeLo1, extremeHi1] <- 1
    adjacency_matrix[extremeLo2, extremeHi1] <- 1
    adjacency_matrix[extremeLo3, extremeHi1] <- 1
    
  } else {
    adjacency_matrix[extremeHi1, extremeLo1] <- 1
    adjacency_matrix[extremeHi2, extremeLo1] <- 1
    adjacency_matrix[extremeHi3, extremeLo1] <- 1
    
  }
  
  #reorder belief matrix to align with adjacency matrix
  data <- data[order(data[,1]),] 
  
  #output new adjacency matrix
  adjacency_matrix
}

## polarize
pol_rewire <- function(data, adjacency_matrix, current_beliefs_col_index){
  
  #order belief matrix so agent w/ lowest belief is on top
  data <- data[order(data[,current_beliefs_col_index]),] 
  
  #index extreme low and high agents
  extremeLo <- data[1,1] 
  extremeHi <- data[nrow(data),1] 
  
  #index core agents -- middle range (undecided) agents
  medAgent <- round(median(data$agent))
  core1 <- data[(medAgent-1),1] 
  core2 <- data[medAgent,1] 
  core3 <- data[(medAgent+1),1] 
  core4 <- data[(medAgent+2),1] 
  
  #cut all incoming communication to extreme agent to preserve belief
  adjacency_matrix[extremeLo, ] <- 0 
  adjacency_matrix[extremeHi, ] <- 0 
  
  #send extreme voices to core (i.e., undecided) agents
  adjacency_matrix[core1, extremeLo] <- 1 
  adjacency_matrix[core2, extremeLo] <- 1
  adjacency_matrix[core3, extremeHi] <- 1
  adjacency_matrix[core4, extremeHi] <- 1
  
  #return data to order by agent to re-align with adjacency matrix
  data <- data[order(data[,1]),] 
  
  #output new adjacency matrix
  adjacency_matrix
}

# preset edge lists for scheduled networks ----

## N = 16, D = 2, T = 4, cliques (N/D) = 8
dyad_t1 <- matrix(c(1, 2, 
                    3, 4,
                    5, 6,
                    7, 8,
                    9, 10,
                    11, 12,
                    13, 14,
                    15, 16),
                  nc = 2, byrow = TRUE)

dyad_t2 <- matrix(c(1, 3,
                    2, 4,
                    5, 7,
                    6, 8,
                    9, 11,
                    10, 12,
                    13, 15,
                    14, 16),
                  nc = 2, byrow = TRUE)

dyad_t3 <- matrix(c(1, 5,
                    2, 6,
                    7, 3,
                    8, 4,
                    9, 13,
                    10, 14,
                    11, 15,
                    12, 16),
                  nc = 2, byrow = TRUE)

dyad_t4 <- matrix(c(1, 9,
                    2, 10,
                    3, 11,
                    4, 12,
                    5, 13,
                    6, 14,
                    7, 15,
                    8, 16),
                  nc = 2, byrow = TRUE)

# turn edge lists into adjacency matrices
dyad_t1_adj <- dyad_t1 %>%
  graph_from_edgelist(directed = FALSE) %>%
  as_adjacency_matrix(names = TRUE, edges = FALSE) %>%
  as.matrix()

dyad_t2_adj <- dyad_t2 %>%
  graph_from_edgelist(directed = FALSE) %>%
  as_adjacency_matrix(names = TRUE, edges = FALSE) %>%
  as.matrix()

dyad_t3_adj <- dyad_t3 %>%
  graph_from_edgelist(directed = FALSE) %>%
  as_adjacency_matrix(names = TRUE, edges = FALSE) %>%
  as.matrix()

dyad_t4_adj <- dyad_t4 %>%
  graph_from_edgelist(directed = FALSE) %>%
  as_adjacency_matrix(names = TRUE, edges = FALSE) %>%
  as.matrix()

rm(dyad_t1, dyad_t2, dyad_t3, dyad_t4)

# model ----

binary_model <- function(N = 16,
                         n_rounds = 4,
                         intervene_on_round = 1,
                         BR,
                         n_sources = 16,
                         sensi,
                         speci,
                         updating_rule = becker_degroot,
                         ur_lab = "becker_degroot"){
  
  # set results df to grow 
  res <- NULL
  
  # put arguments/parameters in global environment for subsidiary functions
  assign("N", N, envir = .GlobalEnv)
  assign("n_rounds", n_rounds, envir = .GlobalEnv)
  assign("intervene_on_round", intervene_on_round, envir = .GlobalEnv)
  assign("BR", BR, envir = .GlobalEnv)
  assign("n_sources", n_sources, envir = .GlobalEnv)
  assign("sensi", sensi, envir = .GlobalEnv)
  assign("speci", speci, envir = .GlobalEnv)
  
  #### initialization ####
  
  # determine ground truth per BR
  truth <- 1 
  
  if (truth == 1){                  # if truth is 1...
    sources <- c(1:n_sources) 
    evidence <- vector("list", length(sources))       # create an empty vector for each source to be filled with evidence
    for (i in sources) {
      n <- sample(10, 1, replace = TRUE)              # randomly determine length of evidence (1-10) for each agent
      evidence[[i]] <- rbinom(n, 1, sensi)            # generate evidence per sensitivity
    }
    
  } else {                          # if truth is 0... 
    sources <- c(1:n_sources) 
    evidence <- vector("list", length(sources))       # create an empty vector for each source to be filled with evidence
    for (i in sources) {
      n <- sample(10, 1, replace = TRUE)              # randomly determine length of evidence (1-10) for each agent
      evidence[[i]] <- rbinom(n, 1, speci)            # generate evidence per specificity
      evidence[[i]] <- ifelse(evidence[[i]]==1, 0, 1) # flip evidence to suit truth of 0 (all 1s become 0s; all 0s become 1s)
    }
  }
  
  
  ## if there at fewer sources than agents, each agent samples from a 'bucket' of evidence until all evidence is taken, then all the evidence goes back into the 'bucket' for the remaining agents
  ## this means all of the sources are heard from by at least one agent
  if(N > n_sources){
    my_evidence <- c(sample(evidence, n_sources, replace = FALSE), sample(evidence, (N-n_sources), replace = FALSE))
  } 
  
  ## if there at least as many sources as agents, each agent samples from an independent source 
  ## (note that the evidence generated by each source is random, and could therefore still be identical when from independent sources)
  if (n_sources >= N) {
    my_evidence <- c(sample(evidence, N, replace = FALSE))
  }
  
  # feed vectors of evidence through learn function to calculate t0 beliefs
  if (truth == 1){
    t0 <- sapply(my_evidence, feed_H)
  } else {
    t0 <- sapply(my_evidence, feed_notH)
  }
  
  # save dataframe with settled t0 beliefs
  data <- data.frame("agent" = c(1:N), "t0" = t0)
  
  # generate small world network and corresponding adjacency matrix 
  rewireProb <- runif(1,0,1)
  netSim <- sample_smallworld(dim = 1, nei = 1, size = N, p = rewireProb) 
  adjMatrix <- as.matrix(as_adjacency_matrix(netSim, names = TRUE, edges = FALSE)) 
  
  #### static network ####
  
  # set up dataframe to work with
  controlData <- data
  
  # communicate + update
  for (i in 1:n_rounds){ # for each round...
    
    # overlay current beliefs with adjacency matrix; each row of 'mat' contains the beliefs visible to a given agent
    mat <- adjMatrix  * t(matrix(controlData[,(i+1)], nrow = N, ncol = N)) 
    mat[mat == 0] <- NA 
    
    # initialise null object to store all agents posteriors for a given round
    posts_all <- NULL
    
    for (j in 1:N){ # for each agent...
      
      # calculate agent j's posterior 
      post_j <- becker_degroot(prior = controlData[j, i+1],  neighborsBeliefs = mat[j,], truth = truth)
      
      # store agent j's posterior in 'post_all' object
      posts_all <- c(posts_all, post_j)
      
    } 
    
    # append updated beliefs to 'data' as new column 
    controlData <- cbind(controlData, posts_all)
    
    # name new columns appropriately
    names(controlData)[ncol(controlData)] <- paste0("t", i)
  }
  
  # calculate performance measures
  
  ## collective response; CR
  CR <- colMeans(controlData[2:6])
  
  ## collective error squared; CES
  CES <- (CR-truth)^2
  
  ## collective absolute error; CAE
  CAE <- abs(CR-truth)
  
  ## collective sqrt error; CSRE
  CSRE <- sqrt(abs(CR-truth))
  
  ## average individual error squared; AIES
  controlData2 <- controlData[2:6] 
  controlData2 <- (controlData2-truth)^2
  AIES <- colMeans(controlData2)
  rm(controlData2)
  
  ## average individual absolute error; AIAE
  controlData2 <- controlData[2:6] 
  controlData2 <- abs(controlData2-truth)
  AIAE <- colMeans(controlData2)
  rm(controlData2)
  
  ## average individual sqrt error; AISRE
  controlData2 <- controlData[2:6] 
  controlData2 <- sqrt(abs(controlData2-truth))
  AISRE <- colMeans(controlData2)
  rm(controlData2)
  
  ## variance 
  variance <- as.vector(sapply(controlData[2:6], function(i)
    var(i)))
  
  # bind together output measures
  controlOutput <- data.frame("condition" = "static"
                              , "BR" = BR
                              , "GT" = truth
                              , "N" = N
                              , "sensi" = sensi
                              , "speci" = speci
                              , "p" = rewireProb
                              , "updating_rule" = "becker_degroot"
                              , "time" = c("t0", "t1", "t2", "t3", "t4")
                              , "CR" = CR
                              , "CES" = CES
                              , "CAE" = CAE
                              , "CSRE" = CSRE
                              , "AIES" = AIES
                              , "AIAE" = AIAE
                              , "AISRE" = AISRE
                              , "VAR" = variance)
  
  # calculate non-communicating control measures by using t0 beliefs
  nonCommOutput <- data.frame("condition" = "non_comm"
                              , "BR" = BR
                              , "GT" = truth
                              , "N" = N
                              , "sensi" = sensi
                              , "speci" = speci
                              , "p" = rewireProb
                              , "updating_rule" = "becker_degroot"
                              , "time" = NA
                              , "CR" = (mean(controlData$t0))
                              , "CES" = (((mean(controlData$t0)) - truth )^2)
                              , "CAE" = abs( mean(controlData$t0) - truth )
                              , "CSRE" = sqrt(abs( mean(controlData$t0) - truth ))
                              , "AIES" = (mean(((controlData$t0) - truth)^2))
                              , "AIAE" = (mean(abs((controlData$t0) - truth)))
                              , "AISRE" = sqrt((mean(abs((controlData$t0) - truth))))
                              , "VAR" = (var(controlData$t0)))
  
  #### mean-extreme network ####
  
  # set up dataframe to work with
  expData <- data
  
  # communicate + update
  for (i in 1:n_rounds){
    
    if (i >= intervene_on_round) {
      adjMatrix2 <- me_rewire(data = expData, adjacency_matrix = adjMatrix, current_beliefs_col_index = ncol(expData))
    } else {
      adjMatrix2 <- adjMatrix
    }
    
    # overlay current beliefs with adjacency matrix; each row of 'mat' contains the beliefs visible to a given agent
    mat <- adjMatrix2  * t(matrix(expData[,(i+1)], nrow = N, ncol = N)) 
    mat[mat == 0] <- NA 
    
    # initialise null object to store all agents posteriors for a given round
    posts_all <- NULL
    
    for (j in 1:N){
      
      # calculate agent j's posterior 
      post_j <- becker_degroot(prior = expData[j, i+1],  neighborsBeliefs = mat[j,], truth = truth)
      
      # store agent j's posterior in 'post_all' object
      posts_all <- c(posts_all, post_j)
      
    } 
    
    # append updated beliefs to 'data' as new column 
    expData <- cbind(expData, posts_all)
    
    # name new column appropriately
    names(expData)[ncol(expData)] <- paste0("t", i)
  }
  
  #### calculate performance measures
  
  ## collective response; CR
  CR <- colMeans(expData[2:6])
  
  ## collective error squared; CES
  CES <- (CR-truth)^2
  
  ## collective absolute error; CAE
  CAE <- abs(CR-truth)
  
  ## collective sqrt error; CSRE
  CSRE <- sqrt(abs(CR-truth))
  
  ## average individual error squared; AIES
  expData2 <- expData[2:6] 
  expData2 <- (expData2-truth)^2
  AIES <- colMeans(expData2)
  rm(expData2)
  
  ## average individual absolute error; AIAE
  expData2 <- expData[2:6] 
  expData2 <- abs(expData2-truth)
  AIAE <- colMeans(expData2)
  rm(expData2)
  
  ## average individual sqrt error; AISRE
  expData2 <- expData[2:6] 
  expData2 <- sqrt(abs(expData2-truth))
  AISRE <- colMeans(expData2)
  rm(expData2)
  
  ## variance 
  variance <- as.vector(sapply(expData[2:6], function(i)
    var(i)))
  
  #bind together output measures
  expOutput <- data.frame("condition" = "mean_extreme"
                          , "BR" = BR
                          , "GT" = truth
                          , "N" = N
                          , "sensi" = sensi
                          , "speci" = speci
                          , "p" = rewireProb
                          , "updating_rule" = "becker_degroot"
                          , "time" = c("t0", "t1", "t2", "t3", "t4")
                          , "CR" = CR
                          , "CES" = CES
                          , "CAE" = CAE
                          , "CSRE" = CSRE
                          , "AIES" = AIES
                          , "AIAE" = AIAE
                          , "AISRE" = AISRE
                          , "VAR" = variance)
  
  #### polarize network ####
  
  # set up dataframe to work with
  polData <- data
  
  # communicate + update
  for (i in 1:n_rounds){
    
    if (i >= intervene_on_round) {
      adjMatrix2 <- pol_rewire(data = polData, adjacency_matrix = adjMatrix, current_beliefs_col_index = ncol(polData))
    } else {
      adjMatrix2 <- adjMatrix
    }
    
    # overlay current beliefs with adjacency matrix; each row of 'mat' contains the beliefs visible to a given agent
    mat <- adjMatrix2  * t(matrix(polData[,(i+1)], nrow = N, ncol = N)) 
    mat[mat == 0] <- NA 
    
    # initialise null object to store all agents posteriors for a given round
    posts_all <- NULL
    
    for (j in 1:N){
      
      # calculate agent j's posterior 
      post_j <- becker_degroot(prior = polData[j, i+1],  neighborsBeliefs = mat[j,], truth = truth)
      
      # store agent j's posterior in 'post_all' object
      posts_all <- c(posts_all, post_j)
      
    } 
    
    # append updated beliefs to 'data' as new column 
    polData <- cbind(polData, posts_all)
    
    # name new column appropriately
    names(polData)[ncol(polData)] <- paste0("t", i)
  }
  
  #### calculate performance measures
  
  ## collective response; CR
  CR <- colMeans(polData[2:6])
  
  ## collective error squared; CES
  CES <- (CR-truth)^2
  
  ## collective absolute error; CAE
  CAE <- abs(CR-truth)
  
  ## collective sqrt error; CSRE
  CSRE <- sqrt(abs(CR-truth))
  
  ## average individual error squared; AIES
  polData2 <- polData[2:6] 
  polData2 <- (polData2-truth)^2
  AIES <- colMeans(polData2)
  rm(polData2)
  
  ## average individual absolute error; AIAE
  polData2 <- polData[2:6] 
  polData2 <- abs(polData2-truth)
  AIAE <- colMeans(polData2)
  rm(polData2)
  
  ## average individual sqrt error; AISRE
  polData2 <- polData[2:6] 
  polData2 <- sqrt(abs(polData2-truth))
  AISRE <- colMeans(polData2)
  rm(polData2)
  
  ## variance 
  variance <- as.vector(sapply(polData[2:6], function(i)
    var(i)))
  
  #bind together output measures
  polOutput <- data.frame("condition" = "polarize"
                          , "BR" = BR
                          , "GT" = truth
                          , "N" = N
                          , "sensi" = sensi
                          , "speci" = speci
                          , "p" = rewireProb
                          , "updating_rule" = "becker_degroot"
                          , "time" = c("t0", "t1", "t2", "t3", "t4")
                          , "CR" = CR
                          , "CES" = CES
                          , "CAE" = CAE
                          , "CSRE" = CSRE
                          , "AIES" = AIES
                          , "AIAE" = AIAE
                          , "AISRE" = AISRE
                          , "VAR" = variance)
  
  
  #### scheduled network ####
  
  # set up dataframe to work with
  schData <- data
  
  # communicate + update
  
  ### round 1....
  
  # overlay current beliefs with adjacency matrix; each row of 'mat' contains the beliefs visible to a given agent
  mat <- dyad_t1_adj  * t(matrix(schData[,2], nrow = N, ncol = N)) 
  mat[mat == 0] <- NA 
  
  # initialise null object to store all agents posteriors 
  posts_all <- NULL
  
  for (j in 1:N){ # for each agent...
    
    # calculate agent j's posterior 
    post_j <- becker_degroot(prior = schData[j,2],  neighborsBeliefs = mat[j,], truth = truth)
    
    # store agent j's posterior in 'post_all' object
    posts_all <- c(posts_all, post_j)
    
  } 
  
  # rename vector appropriately
  t1 <- posts_all
  
  # append updated beliefs  as new column 
  schData <- cbind(schData, t1)
  
  ### round 2....
  
  # overlay current beliefs with adjacency matrix; each row of 'mat' contains the beliefs visible to a given agent
  mat <- dyad_t2_adj  * t(matrix(schData[,3], nrow = N, ncol = N)) 
  mat[mat == 0] <- NA 
  
  # initialise null object to store all agents posteriors 
  posts_all <- NULL
  
  for (j in 1:N){ # for each agent...
    
    # calculate agent j's posterior 
    post_j <- becker_degroot(prior = schData[j,3],  neighborsBeliefs = mat[j,], truth = truth)
    
    # store agent j's posterior in 'post_all' object
    posts_all <- c(posts_all, post_j)
    
  } 
  
  # rename vector appropriately
  t2 <- posts_all
  
  # append updated beliefs  as new column 
  schData <- cbind(schData, t2)
  
  ###### ROUND 3....
  
  # overlay current beliefs with adjacency matrix; each row of 'mat' contains the beliefs visible to a given agent
  mat <- dyad_t3_adj  * t(matrix(schData[,4], nrow = N, ncol = N)) 
  mat[mat == 0] <- NA 
  
  # initialise null object to store all agents posteriors 
  posts_all <- NULL
  
  for (j in 1:N){ # for each agent...
    
    # calculate agent j's posterior 
    post_j <- becker_degroot(prior = schData[j,4],  neighborsBeliefs = mat[j,], truth = truth)
    
    # store agent j's posterior in 'post_all' object
    posts_all <- c(posts_all, post_j)
    
  } 
  
  # rename vector appropriately
  t3 <- posts_all
  
  # append updated beliefs  as new column 
  schData <- cbind(schData, t3)
  
  ### round 4....
  
  # overlay current beliefs with adjacency matrix; each row of 'mat' contains the beliefs visible to a given agent
  mat <- dyad_t4_adj  * t(matrix(schData[,5], nrow = N, ncol = N)) 
  mat[mat == 0] <- NA 
  
  # initialise null object to store all agents posteriors 
  posts_all <- NULL
  
  for (j in 1:N){ # for each agent...
    
    # calculate agent j's posterior 
    post_j <- becker_degroot(prior = schData[j,5],  neighborsBeliefs = mat[j,], truth = truth)
    
    # store agent j's posterior in 'post_all' object
    posts_all <- c(posts_all, post_j)
    
  } 
  
  # rename vector appropriately
  t4 <- posts_all
  
  # append updated beliefs  as new column 
  schData <- cbind(schData, t4)
  
  
  # calculate performance measures
  
  ## collective response; CR
  CR <- colMeans(schData[2:6])
  
  ## collective error squared; CES
  CES <- (CR-truth)^2
  
  ## collective absolute error; CAE
  CAE <- abs(CR-truth)
  
  ## collective sqrt error; CSRE
  CSRE <- sqrt(abs(CR-truth))
  
  ## average individual error squared; AIES
  schData2 <- schData[2:6] 
  schData2 <- (schData2-truth)^2
  AIES <- colMeans(schData2)
  rm(schData2)
  
  ## average individual absolute error; AIAE
  schData2 <- schData[2:6] 
  schData2 <- abs(schData2-truth)
  AIAE <- colMeans(schData2)
  rm(schData2)
  
  ## average individual sqrt error; AISRE
  schData2 <- schData[2:6] 
  schData2 <- sqrt(abs(schData2-truth))
  AISRE <- colMeans(schData2)
  rm(schData2)
  
  ## variance 
  variance <- as.vector(sapply(schData[2:6], function(i)
    var(i)))
  
  # bind together output measures
  schOutput <- data.frame("condition" = "scheduled"
                          , "BR" = BR
                          , "GT" = truth
                          , "N" = N
                          , "sensi" = sensi
                          , "speci" = speci
                          , "p" = rewireProb
                          , "updating_rule" = "becker_degroot"
                          , "time" = c("t0", "t1", "t2", "t3", "t4")
                          , "CR" = CR
                          , "CES" = CES
                          , "CAE" = CAE
                          , "CSRE" = CSRE
                          , "AIES" = AIES
                          , "AIAE" = AIAE
                          , "AISRE" = AISRE
                          , "VAR" = variance)
  
  
  
  #### grow results ####
  
  res <- rbind(res, nonCommOutput, controlOutput, expOutput, polOutput, schOutput)
}
# parameter space ----
N_vec <- c(16)
n_rounds_vec <- c(4)
intervene_on_round_vec <- c(2)
BR_vec <- c(0.1) # BR = agents' prior. So, 0.1 means bias away from truth = 1
n_sources_vec <- c(16)
sensi_vec <- c(0.2, 0.4, 0.9)
speci_vec <- c(0.2, 0.4, 0.9)

# map parameter space --- 500 iterations of each specification will be run
paramspace <- expand.grid(N = N_vec, n_rounds = n_rounds_vec, intervene_on_round = intervene_on_round_vec, BR = BR_vec, n_sources = n_sources_vec, sensi = sensi_vec, speci = speci_vec)
paramspace

# simulation ----
res <- runAll(N_vec, n_rounds_vec, intervene_on_round_vec, BR_vec, n_sources_vec, sensi_vec, speci_vec)

# plots ----

### actual change in errror
static <- filter(res, time == "t4" & condition == "static")
noncomm <- filter(res, is.na(time) & condition == "non_comm")
meanex <- filter(res, time == "t4" & condition == "mean_extreme")
polarize <- filter(res, time == "t4" & condition == "polarize")
scheduled <- filter(res, time == "t4" & condition == "scheduled")

me_eval <- static[12:18] - meanex[12:18]
pol_eval <- static[12:18] - polarize[12:18]
sch_eval <- static[12:18] - scheduled[12:18]

me_eval <- cbind(meanex[2:9], me_eval)
pol_eval <- cbind(polarize[2:9], pol_eval)
sch_eval <- cbind(scheduled[2:9], sch_eval)

all_eval <- rbind(me_eval, pol_eval, sch_eval)

# change in error plot
png("binary_sim_biased.png", res = 500, width = 15, height = 11, units = "cm")
all_eval %>%
  select(condition, sensi, speci, BR, GT, CES, CAE, CSRE, AIES, AIAE, AISRE, VAR) %>%
  melt(id = c("condition", "sensi", "speci", "BR", "GT")) %>%
  group_by(variable, condition, sensi, speci) %>%
  summarise(M = mean(value), SD = sd(value), .groups = 'drop') %>%
  as.data.frame() %>%
  filter(condition != "random") %>%
  filter(variable == "CES" | variable == "AIES" | variable == "VAR") %>%
  ggplot(aes(x = variable, y = M*(-1), fill = condition))+
  geom_bar(alpha=0.6, stat = "identity", color = "black", position = position_dodge(width=0.9))+
  #geom_errorbar(aes(ymin = (M-SD)*(-1), ymax = (M+SD)*(-1)), width = 0.2, position = position_dodge(width=0.9))+  
  geom_hline(yintercept = 0, size = 0.5, linetype=1, color = "black")+
  theme_bw()+
  scale_fill_manual(labels = c("mean_extreme" = "Mean-Extreme","polarize" = "Polarize",
                               "scheduled" = "Scheduled"),
                    values=c("firebrick", "darkgoldenrod1", "cornflowerblue"))+
  scale_x_discrete(name ="Measure", limits=c("CES","AIES","VAR"))+
  labs(x = "Measure", 
       y = "Difference", 
       fill = "Network condition")+
  facet_grid(speci ~ sensi, labeller = label_both)+
  theme(
    panel.spacing = unit(0,"line"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 8), 
    legend.text = element_text(size = 8))
dev.off()
