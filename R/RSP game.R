play <- function(){
  
    #object 
    win  <- 0
    lose <- 0
    tie  <- 0
  
  while(TRUE) {
    
    #object
    gesture <- c("hammer","scissor","paper")
    com <-sample(gesture, 1)
    
    #Player 
    user_input <- readline("Please select hammer, scissor, paper or quit : ")
    
    #Game 
    if (user_input == com) {print("tie")
      tie <-tie + 1 }
    #Player_Win
    else if((user_input == "hammer" & com == "scissor")|(user_input == "scissor" & com == "paper")|(user_input == "paper" & com == "hammer"))
    {print("Player Win")
      win <-win + 1 }
    #Player_Lose
    else if ((user_input == "scissor" & com == "hammer")|(user_input == "paper" & com == "scissor")|(user_input == "hammer" & com == "paper"))
    {print("Player Lose")
      lose <-lose+ 1 }
    #Player Quit, NA
    else if (user_input == "quit")
    {result <- data.frame(win,lose,tie)
    print(result) 
      break }
    else 
    {print("please select again")}
  }
}
