# clear environment ----
rm(list=ls())

# install + load packages  ----
pkgs <- c("plyr", "igraph", "tidyverse", "reshape2", 
          "foreach", "scales", "cowplot", "e1071")

for (p in pkgs){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}
rm(p, pkgs)


# functions ----

# becker-degroot belief updating rule
becker_degroot <- function(prior,             # point belief estimate 
                           neighborsBeliefs,  # vector of neighbors' beliefs
                           truth){            # ground truth
  
  # if the agent has neighbors...
  if (!is.na(mean(neighborsBeliefs, na.rm = TRUE))){ 
    
    # calculate mean of neighbors' beliefs
    neighborsMean <- mean(neighborsBeliefs, na.rm = TRUE)
    
    # calculate absolute distance from the truth
    dist_truth <- (abs(prior-truth))
    
    # determine self-weight via regression from Becker et al., 2017, 
    # appendix, p. 33 + gaussian noise with added gaussion noise, 
    # the correlation between dist_truth and self_weight is approx -0.23
    # i.e., more accurate = more resistant to social influence
    self_weight <- 0.74-(0.05*(dist_truth))+rnorm(1, m = 0, sd = 0.06)
    
    
    # apply standard degroot equation
    posterior <- (self_weight*prior) + ((1-self_weight)*neighborsMean) 
    
  } else {                                                           
    posterior <- prior # if agent has no neighbors, do not update
  }
  
  posterior
  
}

# mean-extreme rewiring
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

# polarize rewiring
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

# read in empirical dataset ----
# read in csv file 
emp_df <- read.csv("prior_research.csv")

# select columms of interest 
emp_df <- emp_df %>% select(task_id, theta, pre_influence) 

# df_becker2019_Unemployment has two thetas listed in original dataset.
# we just consider obs where theta = 4, not those where theta = -46
emp_df <- emp_df[!(emp_df$task_id=="df_becker2019_Unemployment" & emp_df$theta==-46),]

# grab thetas for each task an melt
thetas <- emp_df %>% 
  group_by(task_id) %>% 
  slice(1) %>% 
  select(-pre_influence) %>% 
  melt(id = "task_id")

# remove theta column from main df and melt
emp_df <- emp_df %>% 
  select(-theta) %>% 
  melt(id = "task_id")

# bind togeter melted dfs  
emp_df <- rbind(emp_df, thetas)

# group by task_id and rescale estimates and theta, 0-1
emp_df <- emp_df %>% 
  group_by(task_id) %>% 
  mutate(value = rescale(value))

# split back out thetas and estimates
thetas <- emp_df %>% filter(variable == "theta") %>% select(-variable)
names(thetas) <- c("task_id", "theta")

emp_df <- emp_df %>% filter(variable == "pre_influence") %>% select(-variable)
names(emp_df) <- c("task_id", "pre_influence")

# join back together
emp_df <- inner_join(emp_df, thetas, by = "task_id")
rm(thetas)


# model ----
numeric_model <- function(task_index,
                          N = 16,
                          n_rounds = 4,
                          intervene_on_round = 1,
                          p = runif(1),
                          k = 1,
                          updating_rule = becker_degroot,
                          ur_lab = "becker_degroot"){
  
  # set results df to grow 
  res <- NULL
  
  # put arguments/parameters in global environment for subsidiary functions
  assign("N", N, envir = .GlobalEnv)
  assign("n_rounds", n_rounds, envir = .GlobalEnv)
  assign("intervene_on_round", intervene_on_round, envir = .GlobalEnv)
  assign("p", p, envir = .GlobalEnv)
  assign("k", k, envir = .GlobalEnv)
  
  #### initialization ####
  
  # sample 16 observations from selected task
  obs <- emp_df %>% 
    filter(task_id == unique(emp_df$task_id)[task_index]) %>%
    filter(!is.na(pre_influence)) %>%
    sample_n(N, replace = FALSE)
  
  # make starting dataframe
  data <- data.frame("agents" = c(1:N),
                     "t0" = obs$pre_influence)
  
  # set ground truth 
  truth <- obs$theta[1]
  
  # generate small world network and corresponding adjacency matrix 
  netSim <- sample_smallworld(dim = 1, nei = k, size = N, p = p) 
  adjMatrix <- as.matrix(as_adjacency_matrix(netSim, names = TRUE, edges = FALSE)) 
  
  #### static network ####
  
  # set up dataframe to work with
  controlData <- data
  
  # communicate + update
  for (i in 1:n_rounds){ # for each round...
    
    # overlay current beliefs with adjacency matrix; 
    # each row of 'mat' contains the beliefs visible to a given agent
    mat <- adjMatrix  * t(matrix(controlData[,(i+1)], nrow = N, ncol = N)) 
    mat[mat == 0] <- NA 
    
    # initialise null object to store all agents posteriors for a given round
    posts_all <- NULL
    
    for (j in 1:N){ # for each agent...
      
      # calculate agent j's posterior 
      post_j <- becker_degroot(prior = controlData[j, i+1],  
                               neighborsBeliefs = mat[j,], 
                               truth = truth)
      
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
  controlOutput <- data.frame("task" = task_index
                              , "condition" = "static"
                              , "GT" = truth
                              , "N" = N
                              , "p" = p
                              , "k" = k
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
  nonCommOutput <- data.frame("task" = task_index
                              , "condition" = "non_comm"
                              , "GT" = truth
                              , "N" = N
                              , "p" = p
                              , "k" = k
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
      adjMatrix2 <- me_rewire(data = expData, 
                              adjacency_matrix = adjMatrix, 
                              current_beliefs_col_index = ncol(expData))
    } else {
      adjMatrix2 <- adjMatrix
    }
    
    # overlay current beliefs with adjacency matrix; 
    # each row of 'mat' contains the beliefs visible to a given agent
    mat <- adjMatrix2  * t(matrix(expData[,(i+1)], nrow = N, ncol = N)) 
    mat[mat == 0] <- NA 
    
    # initialise null object to store all agents posteriors for a given round
    posts_all <- NULL
    
    for (j in 1:N){
      
      # calculate agent j's posterior 
      post_j <- becker_degroot(prior = expData[j, i+1],  
                               neighborsBeliefs = mat[j,], 
                               truth = truth)
      
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
  meOutput <- data.frame("task" = task_index
                         , "condition" = "mean_extreme"
                         , "GT" = truth
                         , "N" = N
                         , "p" = p
                         , "k" = k
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
      adjMatrix2 <- pol_rewire(data = polData, 
                               adjacency_matrix = adjMatrix, 
                               current_beliefs_col_index = ncol(polData))
    } else {
      adjMatrix2 <- adjMatrix
    }
    
    # overlay current beliefs with adjacency matrix; 
    # each row of 'mat' contains the beliefs visible to a given agent
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
  polOutput <- data.frame("task" = task_index
                          , "condition" = "polarize"
                          , "GT" = truth
                          , "N" = N
                          , "p" = p
                          , "k" = k
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
  schOutput <- data.frame("task" = task_index
                          , "condition" = "scheduled"
                          , "GT" = truth
                          , "N" = N
                          , "p" = p
                          , "k" = k
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
  
  res <- rbind(res, nonCommOutput, controlOutput, meOutput, polOutput, schOutput)
}


# simulation ----

# set seed for reproducibility
set.seed(2021)

res_full <- NULL
for (i in 1:500){
  output <- foreach(i = 1:54) %do% {numeric_model(task_index = i)}
  output <- do.call(rbind, output)
  row.names(output) <- NULL
  res_full <- rbind(res_full, output)
  rm(output)
}

# analysis + plots ----

# filter to final predictions
res <- res_full %>% filter(time == "t4" | is.na(time)) 

### average_effects ###
average_effects <- res %>%
  select(condition, CES) %>%
  melt(id = c("condition")) %>%
  group_by(condition, variable) %>%
  summarise(M=mean(value), SD = sd(value), .groups="drop") 
average_effects

### effects_by_task ###
# group res by task and summarise metrics for each condition
by_task_res <- res %>%
  select(task, condition, CES, CAE, CSRE, AIES, AIAE, AISRE) %>%
  melt(id = c("task", "condition")) %>%
  group_by(task, condition, variable) %>%
  summarise(M=mean(value), SD = sd(value), .groups="drop") 

# identify the best performing condition by task by metric
task_winners <- by_task_res %>% 
  group_by(task, variable) %>% 
  arrange(M, by_group = T) %>% 
  slice_head(n = 1)

# look at frequencies of each condition "winning" by task by metric
# across 500 iterations run with each task
task_winners %>%
  group_by(condition, variable) %>%
  summarise(count = n(), .groups="drop") %>%
  filter(variable == "CES")

# calculate task skewness
task_skewness <- emp_df %>%
  filter(!is.na(pre_influence)) %>%
  as.data.frame() %>%
  group_by(task_id) %>%
  summarise(skewness=skewness(pre_influence), .groups="drop")

# create task list - matches names with numbers used in simulation results df
task_list <- data.frame("task" = 1:54,
                        "task_id" = unique(emp_df$task_id))

# join omegas with task list
task_list <- inner_join(task_list, task_skewness, by = "task_id")

# join task_list with simulated competition results
task_winners <- inner_join(task_winners, task_list, by = "task")

# skewness distribution in tasks considered
p1 <- task_list %>%
  ggplot(aes(x=skewness))+
  geom_histogram(fill="darkgray", color="black", alpha=0.2)+
  theme_light()+  
  ylim(0,10)+
  labs(x = "Skewness", y = "Count")
p1

# skewness space preferences
p2 <- task_winners %>%
  filter(variable=="CES") %>%
  filter(condition != "static") %>%
  ggplot(aes(x = skewness, color = condition))+
  geom_density(alpha=0.1, size = 0.75)+
  geom_rug(alpha=0.5, size = 1.2)+
  scale_color_manual(labels = c("mean_extreme" = "Mean-Extreme","polarize" = "Polarize",
                                "scheduled" = "Scheduled", "static" = "Static"),
                     values=c("firebrick", "darkgoldenrod1", "cornflowerblue", "azure4"))+
  theme_light()+
  theme(legend.position = "right")+
  labs(
    y = "Density",
    x = "Skewness",
    color = "Network Condition",
    fill = "Network Condition")
p2

# get descriptives for each condition's skewness distribution
task_winners %>%
  group_by(condition, variable) %>%
  summarise(M_skew = mean(skewness), SD_skew = sd(skewness), n_tasks = n(), .groups = "drop") %>%
  filter(variable == "CES") %>% as.data.frame()

# same skewness space panel
#png("skewness_space.png", res = 500, width = 16, height = 6, units = "cm")
plot_grid(p1,p2, ncol = 2, rel_widths = c(0.65, 1), align = "h", labels = "AUTO")
#dev.off()


### inspect effects across skewness ###

# split out results by condition
noncomm <- res %>% filter(condition=="non_comm")
static <- res %>% filter(condition=="static")
mean_extreme <- res %>% filter(condition=="mean_extreme")
polarize <- res %>% filter(condition=="polarize")
scheduled <- res %>% filter(condition=="scheduled")

# function for getting comparative effects
get_effects <- function(condition, reference, name_string){
  output <- condition[,10:15]-reference[,10:15]
  output$condition <- name_string
  output$task <- condition$task
  output
}

# Compare network conditions to non-communicating agents:
static_noncomm_diff <- get_effects(static, noncomm, "static")
me_noncomm_diff <- get_effects(mean_extreme, noncomm, "mean_extreme")
pol_noncomm_diff <- get_effects(polarize, noncomm, "polarize")
sch_noncomm_diff <- get_effects(scheduled, noncomm, "scheduled")

# bind, melt, summarise over iterations, and join with task info
noncomm_diff <- rbind(me_noncomm_diff, pol_noncomm_diff, static_noncomm_diff, sch_noncomm_diff)
noncomm_diff <- noncomm_diff %>% melt(id = c("task", "condition"))
noncomm_diff <- noncomm_diff %>%
  group_by(task, condition, variable) %>%
  summarise(M=mean(value), .groups="drop")
noncomm_diff <- inner_join(noncomm_diff, task_list, by = "task")

# plot
p3 <- noncomm_diff %>%
  filter(variable == "CES") %>%
  ggplot(aes(x=skewness, y = M, color = condition, group = condition))+
  geom_smooth(method = "loess", se = F)+
  geom_point(alpha=0.3)+
  geom_hline(yintercept = 0, linetype = 2)+
  scale_color_manual(labels = c("mean_extreme" = "Mean-Extreme","polarize" = "Polarize",
                                "scheduled" = "Scheduled", "static" = "Static"),
                     values=c("firebrick", "darkgoldenrod1", "cornflowerblue", "azure4"))+
  theme_light()+
  coord_cartesian(ylim=c(-0.0075, 0.0075))+
  labs(x="Skewness", y = "Effect on CES", color="Network Condition")
p3

### inspect relative effects across skewness ###

# split out results by condition
noncomm <- res %>% filter(condition=="non_comm")
static <- res %>% filter(condition=="static")
mean_extreme <- res %>% filter(condition=="mean_extreme")
polarize <- res %>% filter(condition=="polarize")
scheduled <- res %>% filter(condition=="scheduled")

# function for getting relative effects
get_rel_effects <- function(condition, reference, name_string){
  output <- (condition[,10:15]-reference[,10:15])/reference[,10:15]
  output$condition <- name_string
  output$task <- condition$task
  output$iteration <- rep(c(1:500), each = 54)
  output <- output %>% 
    select(-condition) %>%
    melt(id = c("task","iteration")) %>% 
    group_by(task, variable) %>% 
    filter(value != -Inf) %>%
    summarise(rel_eff = median(value),.groups = 'drop')
  output$condition <- name_string
  output
}

# Compare network conditions to non-communicating agents:
static_noncomm_rel <- get_rel_effects(static, noncomm, "static")
mean_extreme_noncomm_rel <- get_rel_effects(mean_extreme, noncomm, "mean_extreme")
polarize_noncomm_rel <- get_rel_effects(polarize, noncomm, "polarize")
sch_noncomm_rel <- get_rel_effects(scheduled, noncomm, "scheduled")
noncomm_rel <- rbind(mean_extreme_noncomm_rel, polarize_noncomm_rel, static_noncomm_rel, sch_noncomm_rel)
noncomm_rel <- inner_join(noncomm_rel, task_list, by = "task")

# plot
p4 <- noncomm_rel %>%
  filter(variable == "CES") %>%
  ggplot(aes(x = skewness, y=rel_eff*100, color = condition, group = condition))+
  geom_point(alpha=0.4)+
  geom_smooth(method = "loess", se=F)+
  geom_hline(yintercept = 0, linetype=2)+
  scale_color_manual(labels = c("mean_extreme" = "Mean-Extreme","polarize" = "Polarize",
                                "scheduled" = "Scheduled", "static" = "Static"),
                     values=c("firebrick", "darkgoldenrod1", "cornflowerblue", "azure4"))+
  scale_fill_manual(labels = c("mean_extreme" = "Mean-Extreme","polarize" = "Polarize",
                                "scheduled" = "Scheduled", "static" = "Static"),
                     values=c("firebrick", "darkgoldenrod1", "cornflowerblue", "azure4"))+
  #coord_cartesian(ylim = c(-100, 100))+
  theme_light()+
  labs(x="Skewness", y = "Relative effect on CES (%)", 
       color = "Network Condition", fill = "Network Condition")
p4

### inspect "probability of improvement" across skewness ###

# recreate un-summarised df with absolute effects
noncomm_diff <- rbind(me_noncomm_diff, pol_noncomm_diff, static_noncomm_diff, sch_noncomm_diff)
noncomm_diff <- noncomm_diff %>% melt(id = c("task", "condition"))

# select cols of interest
noncomm_prob <- noncomm_diff %>%
  select(task, condition, variable, value) 

# create binary col for each effect -- 1 for improvement, 0 for no improvement
noncomm_prob$improve <- ifelse(noncomm_prob$value > 0, 0, 1)

# tally iterations where metric improved, task-by-task
noncomm_prob <- noncomm_prob %>% 
  group_by(task, condition, variable) %>%
  summarise(count_improve = sum(improve), .groups="drop")

# calculate proportion of 500 iterations where treatment led to improvement
noncomm_prob$prob_improve <- noncomm_prob$count_improve/500

# join with task data
noncomm_prob <- inner_join(noncomm_prob, task_list, by = "task")

# plot
p5 <- noncomm_prob %>%
  filter(variable=="CES") %>%
  ggplot(aes(x = skewness, y = prob_improve, color = condition, group = condition))+
  geom_point(alpha=0.3)+
  geom_smooth(method = "loess", se = F)+
  labs(x="Skewness", y="Probability of improvement", color = "Network Condition")+
  scale_color_manual(labels = c("mean_extreme" = "Mean-Extreme","polarize" = "Polarize",
                                "scheduled" = "Scheduled", "static" = "Static"),
                     values=c("firebrick", "darkgoldenrod1", "cornflowerblue", "azure4"))+
  ylim(0,1)+
  theme_light()
p5


# create panel of skewness-related plots
my_leg <- get_legend(p3+theme(legend.position = "bottom"))
prow <- plot_grid(p3+theme(legend.position = "none"),
                  p4+theme(legend.position = "none"),
                  p5+theme(legend.position = "none"),
                  rel_widths = c(1,1,1),
                  ncol = 3, labels = "AUTO")

#png(filename = "skewness_plots.png", res = 500, width = 16, height = 8, units = "cm")
plot_grid(prow, my_leg, nrow = 2, rel_heights = c(1, 0.1))
#dev.off()

