#' game<-new("snake")
#' game$run(return_info = TRUE,plot_board = TRUE)
#'
#' game$run_iter("up")
#'
#' game$replay(c(rep("left",2),rep("down",7),rep("right",5),"down",rep("left",2),"up",rep("left",5)),seed=10)


snake<-setRefClass("snake",
                   fields=list(
                     # System variables
                     body="matrix",
                     food = "numeric",
                     direction='character',
                     length="numeric",
                     dead="logical",

                     #game info
                     score_total="numeric",
                     reward="numeric",
                     height="numeric",
                     width="numeric",
                     board="matrix",
                     log="character",
                     fruit_locations="list",

                     state_new="array"
                   ),

                   methods=list(

                     init = function(height=20,width=20,seed=floor(runif(1)*100), fruit_locs){
                       set.seed(seed)
                       body<<-matrix(c(floor(width/2),floor(height/2),floor(width/2),floor(height/2)-1),byrow = FALSE,ncol=2)
                       direction<<-sample(c("up","left","right"),1)
                       reward<<-0
                       score_total<<-0
                       length<<-2
                       height<<-height
                       width<<-width
                       dead<<-FALSE
                       log<<-as.character(seed)
                       if(!missing(fruit_locs)){
                         fruit_locations<<-fruit_locs
                       }
                       updatefood()
                       updateboard()
                       get_state()

                     },
                     stepforward = function(){
                       reward<<-0
                       nextloc<-nextstep()

                       dist_fruit_orig<-sqrt(((food[1]-body[1,1])^2) + (food[2]-body[1,2])^2)
                       dist_fruit_next<-sqrt(((food[1]-nextloc[1])^2) + (food[2]-nextloc[2])^2)

                       # reward<<-(dist_fruit_orig-dist_fruit_next)*(29-dist_fruit_next)
                       reward<<-ifelse(dist_fruit_orig>dist_fruit_next,10,-10)



                       #check if nextLoc is body or wall, if so, fail
                       isbody<-any(do.call('c',lapply(1:nrow(body),function(x){all(body[x,]==nextloc)})))
                       iswall<-any(nextloc%in%c(-1,height+1,width+1))
                       if(isbody || iswall){die()}

                       #check if nextloc is food
                       isfood=all(food==nextloc)
                       if(isfood){
                         length<<-length+1
                         score_total  <<- score_total + ((floor(log(length))+1) * 5)
                         reward <<- 20
                         updatefood()
                       }

                       updatebody(nextloc)
                       if(!dead){
                         updateboard()
                       }
                     },
                     nextstep = function(direct){
                       switch(direction,
                              "up"=c(0,1),
                              "down"=c(0,-1),
                              "left"=c(-1,0),
                              "right"=c(1,0))+body[1,]

                     },
                     updatebody = function(nextloc){
                       bod<-as.numeric(body)
                       if(length>nrow(body)){
                         body<<-matrix(c(nextloc[1],bod[1:nrow(body)],
                                         nextloc[2],bod[(nrow(body)+1):(2*nrow(body))]),byrow=FALSE,ncol=2)
                       }else{
                         body<<-matrix(c(nextloc[1],bod[1:(nrow(body)-1)],
                                         nextloc[2],bod[(nrow(body)+1):((2*nrow(body))-1)]),byrow=FALSE,ncol=2)
                       }
                     },
                     updatefood = function(){
                        #figure out how to pre-specify food?
                       if(length(fruit_locations)==0){
                       pos<-sample(1:(width*height),1)
                       col<-ceiling(pos/height)
                       row<-pos%%height

                       isbody<-any(do.call('c',lapply(1:nrow(body),function(x){all(body[x,]==c(col,row))})))
                       if(isbody){
                         updatefood()
                       }else{
                         food<<-c(col,row)
                       }}else{
                         food<<-fruit_locations[[1]]
                         fruit_locations<<-fruit_locations[-1]
                       }
                     },
                     updateboard = function(){
                       boardtemp<-matrix(rep(0,width*height),nrow=width)
                       boardtemp[body[1,2],body[1,1]]<-2
                       for(snake_segment in seq(2,length)){
                         boardtemp[body[snake_segment,2],body[snake_segment,1]]<-1
                       }
                       boardtemp[food[2],food[1]]<-3
                       board<<-boardtemp
                     },
                     plotboard = function(){
                       par(mar=c(0, 0, 1, 0), xaxs='i', yaxs='i')
                       plot(-1,-1,xlim=c(0,width+1),ylim=c(0,height+1),type='n',axes=FALSE, frame.plot=TRUE)
                       Axis(side=1, labels=FALSE)
                       Axis(side=2, labels=FALSE)
                       points(body[,1],body[,2],col=c("blue",rep('black',length-1)),pch=15,cex=2)
                       points(food[1],food[2],col="red",pch=16,cex=2)
                       title(main = paste("SNAKE! score -",score_total))
                     },
                     returnstatus = function(){

                       state<-state_new

                       get_state()

                       list(
                         state=state,
                         reward=reward,
                         action=array(as.numeric(c("up","down","left","right")%in%direction),dim = c(1,4)),
                         done=dead,
                         state_new=state_new)
                     },

                     die = function(){
                       dead<<-TRUE
                       reward<<-(-20)
                     },

                     updatedirection = function(dir){
                       if(missing(dir)){
                         line<-readline("snakedir: a,w,s,d")
                         print(line)
                         dir<-switch(tolower(line),"a"="left","w"="up","s"="down","d"="right",direction)
                       }
                       if(dir!=direction & okayDir(dir)){
                         direction<<-dir
                       }
                     },

                     updateLog = function(){
                       log<<-c(log,direction)
                     },

                     okayDir=function(dir){
                       dir!=switch(direction,"right"="left","down"="up","up"="down","left"="right")
                     },

                     get_state=function(){
                       state_new<<-array(board,dim = c(1,20,20,1))
                     },


                     run = function(return_info=TRUE,plot_board=FALSE,delay=.3){
                       init()
                       if(plot_board){plotboard()}
                       if(return_info){returnstatus()}
                       updatedirection()
                       while(!dead){

                         stepforward()
                         plotboard()
                         if(plot_board){plotboard()}
                         if(return_info){returnstatus()}
                         if(!dead){updatedirection()}
                       }
                       text(floor(width/2),floor(height/2),labels = "GAME OVER",col="red",cex=3)
                     },

                     run_iter = function(dir,returnStatus=FALSE){
                       if(missing(dir)){
                         dir<-direction
                       }
                       if(!dead){
                         updatedirection(dir)
                         stepforward()
                         updateLog()
                       }else{
                         return("DEAD")
                       }

                       if(returnStatus){
                         returnstatus()
                       }
                     },

                     replay = function(steps,x2,delay=.5){
                       if(is.numeric(x2)){
                         init(seed = x2)
                       }else{
                         init(fruit_locs=x2)
                       }
                       plotboard()
                       for(move in steps){
                         run_iter(move)
                         plotboard()
                         Sys.sleep(delay)
                       }
                     }
                   )
)




