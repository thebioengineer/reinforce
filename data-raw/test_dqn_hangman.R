library(keras)
library(rjson)

source("R/hangman.R")
source("R/memory.R")
source("R/DQN.R")


getword<-function(){
  jsonURL<-"https://www.randomlists.com/data/words.json"
  json_data <- fromJSON(file=jsonURL)$data
  sample(json_data,1)
}


hangman_model <- keras_model_sequential()

hangman_model %>%
  layer_dense(units = 256, activation = 'relu',input_shape = c(3*length(letters))) %>%
  layer_dropout(0.2) %>%
  layer_dense(256, activation='relu') %>%
  layer_dropout(0.2) %>%
  layer_dense(256,activation = "relu")%>%
  layer_dropout(0.2) %>%
  layer_dense(256,activation = "relu")%>%
  layer_dropout(0.2) %>%
  layer_dense(256,activation = "relu")%>%
  layer_dropout(0.2) %>%
  layer_dense(256,activation = "relu")%>%
  layer_dense(length(letters),activation = "softmax")

optimizer <- optimizer_rmsprop(lr = 0.0001)

hangman_model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer
)



hangman_game<-new("hangman")
dqn_agent<-new("DQN_agent")

dqn_agent$add_Model(snake_model)

counter_games<-1
record <- 0
best_game <- 0
records <- list()
fruitPos<- list()
while(counter_games < 150){
  # Initialize classes
  hangman_game$init(getword())
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
      fruitRecord<-c(fruitRecord,food1)
    }

    #train short memory base on the new action and state
    dqn_agent$train_on(state$state, state$reward, state$action, state$done, state$state_new)
    # store the new data into a long term memory
    dqn_agent$remember(state$state, state$reward, state$action, state$done, state$state_new)

    score = snake_game$score_total
  }

  dqn_agent$train_long()

  cat("Game", counter_games, "\tScore:", score,"\n")

  records<-c(records,list(list(game=counter_games,score=score,log=snake_game$log,fruit_positions=fruitRecord)))
  counter_games <- counter_games + 1

}

save_model_hdf5(dqn_agent$model[[1]],"snake_player_larger.hd5")

saveRDS(dqn_agent,"snake_dqn2.rds")

bestScore<-max(sapply(records,`[[`,2))
bestPerf<-which.max(sapply(records,function(x)ifelse(x[[2]]==bestScore,length(x[[1]]),0)))

steps<-records[[bestPerf]][[3]]
res<-saveGIF(
  snake_game$replay(steps[-1],as.numeric(steps[1]),delay = .1),
  movie.name = "init_animation2.gif",
  interval= .25)
