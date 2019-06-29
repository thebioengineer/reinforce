hangman<-setRefClass("hangman",
                   fields=list(
                     # System variables
                     word="character",
                     word_show="character",
                     guesses_left = "numeric",
                     letters_avail='character',
                     letters_guessed='character',
                     guess_order="character",
                     dead="logical",
                     state_old="numeric",
                     state="numeric",

                     #game info
                     reward="numeric"

                   ),
                   methods=list(
                     init=function(inputWord){
                       word<<-strsplit(tolower(inputWord))
                       word_show<<-rep("-",length(word))
                       guesses_left<<-7
                       letters_avail<<-letters
                       letters_guessed<<-c()
                       dead<<-FALSE
                       calc_state()
                     },

                     guess_letter=function(guess){
                       if(dead){
                         stop("you dead fool, you gotta restart")
                       }
                       if(guess%in%letters_guessed){
                         reward<<-(-10)
                       }else{

                       letters_guessed<<-c(letters_guessed,guess)
                       letters_avail<<-setdiff(letters_avail,guess)

                       if(guess %in% word){
                         reward<<-sum(word%in%guess)*5
                         word_shown<-word_show
                         word_shown[[word%in%guess]]<-guess
                         word_show<<-word_shown
                       }else{
                         reward<<-(-5)
                         guesses_left<<-guesses_left-1
                         if(guesses_left==0){
                           dead<<-TRUE
                         }
                       }
                       }
                     },

                     step_next=function(letter){
                       if(missing(letter)){
                         letter<-readline("Enter a letter!")
                       }
                       guess_letter(letter)
                       # update_image()
                       if(dead){
                         print("YOU DEAD!!")
                       }
                     },

                     calc_state=function(){

                       state_old<<-state

                       letters_still_avail=letters%in%letters_avail
                       letters_wrong=letters%in%setdiff(letters_guessed,word)
                       letters_right=letters%in%letters_guessed[letters_guessed%in%word]
                       state<<-array(c(letters_still_avail,letters_wrong,letters_right),dim=c(1,3*length(letters)))

                     }

                     update_image=function(){
                         plot(1:100,1:100, type="n", axes=F, xlab="", ylab="")
                         rect(0,0,2,90,bg="brown",pch=NULL, col="brown")
                         rect(0,88,25,90,bg="brown",pch=NULL, col="brown")
                         segments(2,70,15,89,lwd=4, col = "brown")


                         if(guesses_left > 0) {
                           ## Strick
                           segments(25,81,25,90,lwd=2)
                         }
                         if(guesses_left > 1) {
                           radius <- 7
                           theta <- seq(0, 2 * pi, length = 200)
                           # draw the circle
                           lines(x = 25 + radius * cos(theta), y = 81 - radius + radius * sin(theta))
                         }
                         if(guesses_left > 2) {
                           segments(25,81-(2*radius),25,81-(2*radius)-30,lwd=2)
                         }
                         if(guesses_left > 3) {
                           segments(25,81-(2*radius)-5,30,81-(2*radius)-15,lwd=2)
                         }
                         if(guesses_left > 4) {
                           segments(25,81-(2*radius)-5,20,81-(2*radius)-15,lwd=2)
                         }
                         if(guesses_left > 5) {

                           segments(25,81-(2*radius)-30,30,81-(2*radius)-50,lwd=2)
                         }
                         if(guesses_left > 6) {
                           segments(25,81-(2*radius)-30,20,81-(2*radius)-50,lwd=2)
                         }

                     }

))
