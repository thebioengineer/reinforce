
#' memory class to hold all of the states
#'
#' @importFrom abind abind

memory<-setRefClass("memory",
            fields=list(
              state="list",
              next_state="list",
              reward="list",
              action="list",
              done="list",
              nstates="numeric"),
            methods=list(
              initialize=function(){
                nstates<<-0
              },
              add=function(s1,r,a,d,s2){
                state<<-c(state,list(s1))
                reward<<-c(reward,list(r))
                action<<-c(action,list(a))
                done<<-c(done,list(d))
                next_state<<-c(next_state,list(s2))
                nstates<<-nstates+1
              },
              sample=function(x){
                if(x=="latest"){
                  minibatch<-nstates
                }else if(x=="all"){
                  minibatch<-seq(nstates)
                }else{
                  minibatch<-base::sample(seq(nstates),x,replace = FALSE)
                }
                lapply(minibatch,function(idx){
                  list(
                    state = state[[idx]],
                    action = action[[idx]],
                    reward = reward[[idx]],
                    done = done[[idx]],
                    next_state = next_state[[idx]]
                    )
                })
              }
            ))

mem<-new("memory")
for (i in 1:100) {
  s <- runif(4)
  a <- sample.int(2, size = 1) - 1L
  r <- sample(-10:10, size = 1)
  d <- sample(c(T, F), size = 1)
  s2 <- runif(4)
  mem$add(s, a, r, d, s2)
}
mem$sample(4)
mem$sample("latest")

