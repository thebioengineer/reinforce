library(keras)

source("R/snake.R")
source("R/memory.R")
source("R/DQN.R")

snake_model <- keras_model_sequential()

snake_model %>%
  layer_dense(units = 256, activation = 'relu',input_shape = c(12)) %>%
  layer_dropout(0.1) %>%
  layer_dense(256, activation='relu') %>%
  layer_dropout(0.1) %>%
  layer_dense(256,activation = "relu")%>%
  layer_dropout(0.1) %>%
  layer_dense(256,activation = "relu")%>%
  layer_dropout(0.1) %>%
  layer_dense(256,activation = "relu")%>%
  layer_dropout(0.1) %>%
  layer_dense(256,activation = "relu")%>%
  layer_dense(4,activation = "softmax")

optimizer <- optimizer_rmsprop(lr = 0.0005)

snake_model %>% compile(
  loss = "mse",
  optimizer = optimizer
)



snake_game<-new("snake")
dqn_agent<-new("DQN_agent")

dqn_agent$add_Model(snake_model)

counter_games<-1
record <- 0
best_game <- 0
records <- list()
fruitPos<- list()
while(counter_games < 2000){
  # Initialize classes
  snake_game$init()
  food1 = snake_game$food

  fruitRecord<-list(food1)
  # Perform first move
  state<-snake_game$run_iter(returnStatus = TRUE)
  dqn_agent$remember(state$state, state$reward, state$action, state$done, state$state_new)
  dqn_agent$train_on(state$state, state$reward, state$action, state$done, state$state_new)

  while(!snake_game$dead){
    #get old state
    state_old = state$state_new

    #perform random actions based on agent.epsilon, or choose the action
    prediction = dqn_agent$next_step(state_old)
    final_move = c("up","down","left","right")[which.max(prediction)]

    #perform new move and get new state
    state<-snake_game$run_iter(final_move,returnStatus = TRUE)
    if(food1!=snake_game$food){
      food1<-snake_game$food
      fruitRecord<-c(fruitRecord,list(food1))
    }

    #train short memory base on the new action and state
    dqn_agent$train_on(state$state, state$reward, state$action, state$done, state$state_new)
    # store the new data into a long term memory
    dqn_agent$remember(state$state, state$reward, state$action, state$done, state$state_new)

    score = snake_game$score_total
  }

  #retrain on all data available
  dqn_agent$train_long(3000)

  cat("Game", counter_games, "\tScore:", score,"\n")

  records<-c(records,list(list(game=counter_games,score=score,log=snake_game$log,fruit_positions=fruitRecord)))
  counter_games <- counter_games + 1
}

save_model_hdf5(dqn_agent$mode
                l[[1]],"snake_player_evenlonger.hd5")

saveRDS(dqn_agent,"snake_dqn_longest.rds")
saveRDS(records,"records_longest.rds")

bestScore<-max(sapply(records,`[[`,2))
bestPerf<-which.max(sapply(records,function(x)ifelse(x[[2]]==bestScore,length(x[[1]]),0)))

steps<-records[[bestPerf]][[3]]
fruit_locs<-records[[bestPerf]][[4]]


snake_game2<-new("snake")
dqn_agent2<-new("DQN_agent")
dqn_agent2$add_Model(dqn_agent$model[[1]])

counter_games<-1
record <- 0
best_game <- 0
records <- list()
fruitPos<- list()
while(counter_games < 500){
  # Initialize classes
  snake_game2$init()
  food1 = snake_game2$food

  fruitRecord<-list(food1)
  # Perform first move
  state<-snake_game2$run_iter(returnStatus = TRUE)
  dqn_agent2$remember(state$state, state$reward, state$action, state$done, state$state_new)
  dqn_agent2$train_on(state$state, state$reward, state$action, state$done, state$state_new)

  while(!snake_game2$dead){
    #get old state
    state_old = state$state_new

    #perform random actions based on agent.epsilon, or choose the action
    prediction = dqn_agent2$next_step(state_old,randguess=FALSE)
    final_move = c("up","down","left","right")[which.max(prediction)]

    #perform new move and get new state
    state<-snake_game2$run_iter(final_move,returnStatus = TRUE)
    if(food1!=snake_game2$food){
      food1<-snake_game2$food
      fruitRecord<-c(fruitRecord,list(food1))
    }

    #train short memory base on the new action and state
    dqn_agent2$train_on(state$state, state$reward, state$action, state$done, state$state_new)
    # store the new data into a long term memory
    dqn_agent2$remember(state$state, state$reward, state$action, state$done, state$state_new)

    score = snake_game2$score_total
  }

  cat("Game", counter_games, "\tScore:", score,"\n")

  dqn_agent2$train_long(100*counter_games)

  records<-c(records,list(list(game=counter_games,score=score,log=snake_game2$log,fruit_positions=fruitRecord)))
  counter_games <- counter_games + 1

}

bestScore<-max(sapply(records,`[[`,2))
bestPerf<-which.max(sapply(records,function(x)ifelse(x[[2]]==bestScore,length(x[[1]]),0)))

steps<-records[[bestPerf]][[3]]
fruit_locs<-records[[bestPerf]][[4]]

snake_game2$replay(steps[-1],fruit_locs,delay = .1)
res<-saveGIF(
  snake_game2$replay(steps[-1],fruit_locs,delay = .001),
  movie.name = "init_animation_longer_725.gif",
  interval= .07)

save_model_hdf5(dqn_agent2$model[[1]],"snake_player_larger2.hd5")

saveRDS(dqn_agent2,"snake_dqn2.rds")
saveRDS(records,"snake_dqn2_records.rds")



