DQN<-setRefClass("DQN_agent",
                 fields=list(
                   model="list",
                   mem="memory",
                   gamma="numeric",
                   epsilon="numeric",
                   e_decay="numeric"
                 ),
                 methods=list(
                   initialize=function(gamma=.9,epsilon=1.0,epsilon_decay=400){
                     gamma<<-gamma
                     epsilon<<-epsilon
                     e_decay<<-epsilon_decay
                     mem<<-new("memory")
                   },

                   add_Model=function(model){
                     model<<-list(model)
                   },

                   remember=function(s1,r,a,d,s2){
                     mem$add(s1,r,a,d,s2)
                   },


                   train_long=function(len=1000){

                     if( mem$nstates > len){
                       minibatch = mem$sample(len)
                     }else{
                       minibatch = mem$sample("all")
                     }

                     for(x in minibatch){
                       target = x$reward
                       if(!x$done){
                         target = x$reward + gamma * max(next_step(x$next_state))
                       }
                       target_f = next_step(x$state)
                       target_f[which.max(x$action)] = target

                       tempModel<-model[[1]]

                       fit(tempModel,x$state,array(target_f,dim=dim(x$action)),epochs=1,verbose=0)

                       model<<-list(tempModel)
                     }
                   },

                   train_on=function(s1,r,a,d,s2){
                     target = r
                     if(!d){
                       target = r + gamma * max(next_step(s2))
                     }
                     target_f = next_step(s1)
                     target_f[which.max(a)] = target
                     tempModel<-model[[1]]
                     fit(tempModel,s1,array(target_f,dim=dim(a)),epochs=1,verbose=0)
                     model<<-list(tempModel)

                   },

                   next_step=function(state,randguess=TRUE){
                     if(runif(1)<epsilon && randguess){
                       predictions<-runif(4)
                     }else{
                       predictions<-predict(model[[1]],state)
                     }
                     epsilon<<-max(.1,(epsilon)-(epsilon/e_decay))
                     return(predictions)
                   }

                 ))
