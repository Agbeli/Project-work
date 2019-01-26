
#############################################################################
############ Tennis game Implemetation############################################
#############################################################################

#Centre takes 0
#Player_ 1 takes the position of c(1,2,3)
#Player_2 takes the position of c(-1,-2,-3)
#The starting point maxpoint = 50
##### Takes three input variable 

###Human draw function
Human_bid <- function(maxpoint){
  while(TRUE){
    human<-as.numeric(readline("Enter a bid: "))
    if(human > maxpoint){ #When draw is greater than current point
      cat("Make a bid between 1 and ",maxpoint)
    }else{
      FALSE
      break
    }
  }
  return(human)
}


#### Implementation of 15 differents non-learning machine using 
#### probability distributions 

### 
Machine_1 <- function(Point_pl,Point_opp,ball,output){
  if(Point_pl <= 0){
    y = 0
  }else{
    y = floor(runif(1,1,Point_pl)) # Random generate a value using uniform distribution.
  }
  return(y)
}

Machine_2 <- function(Point_pl,Point_opp,ball,output){
  if(Point_pl <= 0){
    y = 0
  }else if(Point_pl==1){
    y =1
  }else{
    First = ceiling(Point_pl*0.2)
    Second = ceiling(Point_pl*0.8)
    y = rhyper(1,First,Second,Point_pl)
    if(y == 0 || y > Point_pl){
      y = 1
    }else{
      y
    }
  }
  return(y)
}


# Second machine with probability of 0.35 generates a draw.
Machine_3 <- function(Point_pl,Point_opp,ball,output){
  if(Point_pl <= 0){
    y =0
  }else {
    y = ceiling(Point_pl*0.35)
  }
  return(y)
}

# Second machine with quantile binomial function to generate a value 
Machine_4 <- function(Point_pl,Point_opp,ball,output){
  if(Point_pl <= 0){
    y =0
  }else if(Point_pl==1){
    y =1
  }else {
    y = qbinom(0.8,Point_pl,0.2)
  }
  return(y)
}


#### The use of random binomial function with a probability of 0.5
Machine_5 <- function(Point_pl,Point_opp,ball,output){
  if(Point_pl <= 0){
    y = 0
  }else{
    y = rbinom(1,Point_pl,0.5)
    if(y == 0){
      y = 1
    }else{
      y
    }
  }
  return(y)
}

### Random binomial distribtion with conditons
Machine_6 <- function(Point_pl,Point_opp,ball,output){
  if(Point_pl <= 0){
    y <- 0
  }else{
    if(Point_pl >= 49){#plays only 1 the first two rounds
      y <- 1
    }else{
      y = max(1,rbinom(1,Point_pl,0.7),1)#plays very high later 
    }
  }
  return(y)
}




Machine_7 <- function(Point_pl,Point_opp,ball,output){
  if(Point_pl <= 0){
    y = 0
  }else{
    y = rbinom(1,Point_pl,0.35)
    if(y == 0){
      y = 1
    }else{
      y
    }
  }
  return(y)
}




Machine_8 <- function(Point_pl,Point_opp,ball,output){
  if(Point_pl <= 0){
    y <- 0
  }else{
    if(Point_pl == 50){   # plays small the first round
      y <- max(rbinom(1,Point_pl,0.05),1)
    }else{
      y <- max(rbinom(1,Point_pl,0.4),1)# plays very high later 
    }
  }
  return(y)
}


Machine_9 <- function(Point_pl,Point_opp,ball,output){
  if(Point_pl <= 0){
    y = 0
  }else {
    y = ceiling((Point_pl+1)/3)
  }
  return(y)
}

# In this function random poisson distribution was used.
Machine_10 <- function(Point_pl,Point_opp,ball,output){
  if(Point_pl <= 0){
    y = 0
  }else{
   y = rpois(Point_pl,2)
   y = max(Com10)
   if(Com10 == 0 || Com10 > Point_pl){
     y = 1
   }else{
     y
   }
  }
  return(y)
}

Machine_11 <- function(Point_pl,Point_opp,ball,output){
  if(Point_pl <= 0){
    y = 0
  }else {
    y = rpois(Point_pl,5)
    y = max(Com11)
    if(y == 0 || y > Point_pl){
      y = 1
    }else{
      y
    }
  }
  return(y)
}

Machine_12 <- function(Point_pl,Point_opp,ball,output){
  if(Point_pl <= 0){
    y = 0
  }else {
    y = rpois(Point_pl,8)
    y = ceiling((max(Com12)-min(Com12))/2)
    if(y == 0 || y > Point_pl){
      y = 1
    }else{
      y
    }
  }
  return(y)
}

Machine_13 <- function(Point_pl,Point_opp,ball,output){
  if(Point_pl <= 0){
    y <- 0
  }else{
    if( (ball <= -2) &&(Point_pl>=Point_opp)){#plays winning move when it can
      y <- max(Point_opp,1)
    }else{
      if(ball <= 1){#plays small when does not need to play high
        y  <- min(Point_pl,max(rbinom(1,10,0.03),1))
      }else{
        y <- min(Point_pl,max(rbinom(1,Point_opp,0.98),1))#plays very high when in danger, but not higher than points of opponent 
      }
    }
  }
  return(y)
}




Machine_14 <- function(Point_pl,Point_opp,ball,output){
  if(Point_pl <= 0){
    y = 0
  }else if(Point_pl==1){
    y =1
  }else{
    First = ceiling(Point_pl*0.4)
    Second = ceiling(Point_pl*0.6)
    y = rhyper(1,First,Second,Point_pl)
    if(y ==0 || y > Point_pl){
      y = 1
    }else{
      y
    }
  }
  return(y)
}


Machine_15 <- function(Point_pl,Point_opp,ball,output){
  if(Point_pl <= 0){
    y = 0
  }else {
    y = rnbinom(1,Point_pl,0.7)
    if(y == 0 || y >Point_pl){
      y =1
    }else{
      y
    }
  }
  return(y)
}


#### The repeated function of 13 for self-play purpose.
Machine_17 <- function(Point_pl,Point_opp,ball,output){
  if(Point_pl <= 0){
    y <- 0
  }else{
    if( (ball <= -2) &&(Point_pl>=Point_opp)){#plays winning move when it can
      y <- max(Point_opp,1)
    }else{
      if(ball <= 1){#plays small when does not need to play high
          y <- min(Point_pl,max(rbinom(1,10,0.03),1))
      }else{
        y <- min(Point_pl,max(rbinom(1,Point_opp,0.98),1))#plays very high when in danger, but not higher than points of opponent 
      }
    }
  }
  return(y)
}


maxpoint =50	

# Define an initial strategy for the machine 
defstrat<-function(INITPUNKT=1){
  stratyx=rep(0,(maxpoint +1)*(maxpoint +1)*5*(maxpoint +1))
  dim(stratyx)<-c(maxpoint +1,maxpoint +1,5,maxpoint +1) ## The four dimensional array 
  
  for (v4 in 2:maxpoint )					
    stratyx[(maxpoint +1),(maxpoint +1),3,v4] = maxpoint +1+INITPUNKT-v4  
  
  # allowed trains, except zeros
  for (v1 in 2:(maxpoint))
    for (v2 in 1:(maxpoint))
      for (v4 in 2:(maxpoint))
        if (v4<=v1 && v4<=v2+1)		# Only permitted moves and never more than enemies + 1 (Efficient!)
          for (v3 in 1:5)
            if (v3!=3)			
              stratyx[v1,v2,v3,v4]=INITPUNKT
  else if(v1==v2)	
    stratyx[v1,v2,v3,v4]=INITPUNKT

  for (v2 in 1:(maxpoint))
    for (v3 in 1:5)
      stratyx[1,v2,v3,1]=INITPUNKT			
  
  return(stratyx)                     
}

INPOINT = 1
MINPOINTS = 1
LEARNPOINTS = rep(0,3)
dim(LEARNPOINTS) <- c(1,3)
LEARNPOINTS[1] = 1
LEARNPOINTS[2] = 1
LEARNPOINTS[3] = 1

### To strigger the initial strategy 
erstinit<-function(){
  zuz = defstrat(INITPUNKT=1)
  straty <<- rep(0,dim(zuz)[1]*dim(zuz)[2]*dim(zuz)[3]*dim(zuz)[4])
  dim(straty) <<- c(1,dim(zuz))
  
  straty[1,,,,] <<- defstrat(INITPUNKT=INPOINT)
  NumberofPlay <<- 0			
}

### Initialize function to acquire initial strategy.
erstinit()


#### Next move of the learning machine
Machine_16=function(Point_pl,Point_opp,ball,output="Yes"){
  #### Vector of possible draws
  veccy=c(0,straty[1, Point_pl+1, Point_opp+1, ball+3,])
  if(output=="Yes")
    cat(veccy,"\n")
  
  choice=ceiling(runif(1,min=0,max=sum(veccy)))   
  vecu=veccy
  lvec=length(veccy)
  for (ii in 1:lvec)
    vecu[ii]=sum(veccy[1:ii])
  y = max((1:lvec)[vecu<choice])-1       
  return(y)  
}

# Update the strategy after each round of play 
updatestrat = function(chain,value){
  chainy = colSums(abs(chain))	
  x = max((1:length(chainy))[chainy>0])
  {
    LEARN <<- LEARN +1
    pointy=(LEARNPOINTS[abs(value)])*(-value)
    for(ii in 1:x){
      coly=chain[,ii]
      straty[1,coly[1]+1,coly[2]+1,coly[3]+3, coly[4]+1] <<-
        max(c(straty[1,coly[1]+1,coly[2]+1,coly[3]+3, coly[4]+1]+pointy, MINPOINTS))
      straty[1,coly[2]+1,coly[1]+1, -coly[3]+3, coly[5]+1] <<-
        max(c(straty[1,coly[2]+1,coly[1]+1, -coly[3]+3, coly[5]+1]-pointy, MINPOINTS))
    }
  }
}




### Output function after each draws by both players.

Output <- function(Player_1,Player_2,ball,maxpoint1,maxpoint2,output){
if(output=="Yes"){
if(ball==0){
  cat("Status = ",ball,"(center)","\n")
  cat("Current points for Player_1 = ",maxpoint1,"\n")
  cat("Current points for Player_2 =  ",maxpoint2,"\n")  
}else if(ball %in% c(-3,-2,-1)){
cat("Player_1 bid = ",Player_1,"\n")
cat("Player_2 bid = ",Player_2,"\n")
cat("state= ",ball,"(Player_2 side)","\n")
cat("Current points for Player_1 = ",maxpoint1,"\n")
cat("Current points for Player_2 = ",maxpoint2,"\n")
}else if(ball %in% c(3,2,1)){
  cat("Player_1 bid = ",Player_1,"\n")
  cat("Player_2 bid = ",Player_2,"\n")
  cat("state= ",ball,"(Player_1 side)","\n")
  cat("Current points for Player_1 = ",maxpoint1,"\n")
  cat("Current points for Player_2 = ",maxpoint2,"\n")
}
}
}



#The Evaluation function is used to check the position of the ball after every 
# bid of each players 
Evaluation <- function(Player1,Player2,ball){
  if(Player1 == Player2)
    bz=ball
  else 
    if(Player1 < Player2)
      if(ball <= 0)         # when the draw of Player1 is greater than Player2
        bz=1
      else
        bz=ball+1
    else if(Player1 > Player2)
      if(ball >= 0)  # when the draw of Player1 is smaller than Player2
        bz=-1
      else
        bz=ball-1
  return(bz)
}




#Evaluation of winner of the game.
Winner <- function(ball,output){
  if(ball ==  -3){
    if(output == "Yes"){
      cat("=====Victory for Player_1=====","\n")}
  }else if(ball == 3){
    if(output == "Yes"){
      cat("=====Victory for Player_2=====","\n")
  }
}
}



###### Call function of the type of machine 
Computer_bid <- function(Point_pl,Point_opp,ball,output,Player){ #Select type of machine 
  get(paste('Machine_',as.character(Player),sep=""))(Point_pl,Point_opp,ball,output)
}



###Maxpoint = 50 points
# auto =0: Human vrs Human tournament
# auto =1: Human vrs Computer tournament 
# auto =2: Computer vrs Computer tournament
######## The play function execute the tournament match between two strategies 

Play <- function(maxpoint,auto,Player1,Player2,output="No"){
  recordstatus=rep(0,5*maxpoint)
  dim(recordstatus)<-c(5,maxpoint) ## Matrix of recorded outcome.
  timeline <- 1
  ball = 0
  maxpoint1 <- maxpoint
  maxpoint2 <- maxpoint
  Output1 = Output(Player_1,Player_2,ball,maxpoint1,maxpoint2,output)
  if(auto==0){
    while (ball !=-3 && ball!=3) {
      Player_1 <- Human_bid(maxpoint1)  
      Player_2 <- Human_bid(maxpoint2)
      maxpoint1 = maxpoint1-Player_1 
      maxpoint2 = maxpoint2-Player_2
      ball = Evaluation(Player_1, Player_2,ball)
      output2 = Output(Player_1,Player_2,ball,maxpoint1,maxpoint2,output)
      Winner(ball,output)
      if (maxpoint1 == 0 && maxpoint2 == 0){
        if(output == "Yes"){cat("Game over","\n")}
        break
      }
  }
  }else if(auto==1){
    while (ball !=-3 && ball!=3) {
      Player_1 <- Human_bid(maxpoint1)  
      Player_2 <- Computer_bid(Point_pl=maxpoint2,Point_opp=maxpoint1,ball,output,Player2)
      recordstatus[,timeline]=c(maxpoint1,maxpoint2,ball,Player_1,Player_2)
      maxpoint1 = maxpoint1-Player_1 
      maxpoint2 = maxpoint2-Player_2
      ball = Evaluation(Player_1, Player_2,ball)
      timeline = timeline +1
      output2 = Output(Player_1,Player_2,ball,maxpoint1,maxpoint2,output)
      Winner(ball,output)
      if (maxpoint1 == 0 && maxpoint2 == 0){
        if(output == "Yes"){cat("Game over","\n")}
        break
      }
      
    }
    NumberofPlay<<- NumberofPlay + 1 
    if(ball != 0){
      updatestrat(chain = recordstatus,value = ball)
    }
  }else if(auto==2){
    while (ball !=-3 && ball !=3) {
      Player_1 <- Computer_bid(Point_pl=maxpoint1,Point_opp=maxpoint2,ball,output,Player1)
      Player_2  <- Computer_bid(Point_pl=maxpoint2,Point_opp=maxpoint1,ball,output,Player2)
      recordstatus[,timeline]=c(maxpoint1,maxpoint2,ball,Player_1,Player_2)
      maxpoint1 = maxpoint1-Player_1
      maxpoint2 = maxpoint2- Player_2
      ball = Evaluation(Player_1, Player_2,ball)
      timeline = timeline +1
      output2 = Output(Player_1,Player_2,ball,maxpoint1,maxpoint2,output)
      Winner(ball,output)
      if (maxpoint1 == 0 && maxpoint2 == 0){
        if(output == "Yes"){cat("Game over","\n")}
        break
      }
    }
    if(Player_1 == 16 || Player2 == 16){
    NumberofPlay <<- NumberofPlay + 1
    if(ball != 0){ # Update the strategy if the outcome of the ball is different from 0.
      updatestrat(chain = recordstatus,value = ball)
      if(ball > 0)
        POINTS<<-POINTS +ball
      else
        POINTS<<-POINTS -ball
    }
    }
  }
  return(ball)
}


###### The function Playchain call indicates  the number of times the play function is executed #####
##################################################################################################

Playchain <- function(maxpoint=10,auto=2,Player=1:14,Game_number=100,output="No"){
  Number_of_Players <- length(Player)
  Matrix_Wins <- matrix(nrow = Number_of_Players,ncol = Game_number,byrow = TRUE,
                        dimnames = list(Player,1:Game_number)) # Empty matrix of scores 
  Counter_win <- 0      #### Initialize the win and lose
  Counter_lose <- 0
  Mac_win <- 0
  Mac_lose <- 0
  H_vector <- c()
  M_vector <- c()
  Empty_list <- list()
  if(auto==1){
    if(Number_of_Players==1){ #### When the number of players is one 
      for(i in 1:Game_number){
      Player2 = Player
      State = Play(maxpoint,auto,Player1,Player2,output)
      if(State %in% c(-1,-2,-3)){
        H_vector = c(H_vector,1)
        M_vector = c(M_vector,-1)
      }else if(State %in% c(1,2,3)){
        H_vector = c(H_vector,-1)
        M_vector = c(M_vector,1)
      }
      }
      for(y in H_vector){
        if(y==1){
          Counter_win = Counter_win+1
        }else{
          Counter_lose = Counter_lose+1
        }
      }
      for (x in M_vector){
        if(x == 1)
          Mac_win = Mac_win +1
        else
          Mac_lose = Mac_lose +1
      }
      total = Mac_win + Mac_lose
      Total = Counter_win+Counter_lose
      Rate = round((Counter_win/Total)*100,1)
      rate = round((Mac_win/total)*100,1)
      cat("Machine_",Player2,"wins",rate,"%","(",Mac_win,"out of ",total,")","\n")
      cat("Human:","Wins",Rate,"%","(",Counter_win,"out of ",Total,")","\n")
    }else {
    for(i in 1:Game_number){
      Player2 <- sample(Player,1,replace = FALSE)
      State = Play(maxpoint,auto,Player1,Player2,output) # Return the ball state 
      if(State %in% c(-1,-2,-3)){
        Player2 = as.character(Player2)
        H_vector <- c(H_vector,1) # Represent win for Player1
        Matrix_Wins[as.character(Player2),i]=-1 #Represents loss for Player2
      }else if(State %in% c(1,2,3)){
        H_vector <- c(H_vector,-1) #Represents loss for Player1
        Matrix_Wins[as.character(Player2),i]=1  # Represents win for Player2
      }
    }
    Un <- sort(unique(c(Matrix_Wins))) 
    Result <- t(apply(Matrix_Wins,1, function(x) table(factor(x,Un)))) #column_1: -1 Column_2: 1
    Final <- as.array(Result)
    for(x in 1:nrow(Final)){ # Loop over the matrix
      Per_rate <- round((Final[x,2]/(Final[x,1]+Final[x,2]))*100,1)
      Sum <- Final[x,1]+Final[x,2]
      cat("Player",Player[x],"wins: ",Per_rate,"%","(",Final[x,2],"out of ",Sum,"games",")","\n") 
    }
    for(y in H_vector){
      if(y==1){
        Counter_win = Counter_win+1
      }else{
        Counter_lose = Counter_lose+1
      }
    }
    Total = Counter_win+Counter_lose
    Rate = round((Counter_win/Total)*100,1)
    cat("Human:","Wins",Rate,"%","(",Counter_win,"out of ",Total,")","\n")
    }
  }else if(auto==2){
  for(i in 1:Game_number){
    Select <- sample(Player,2,replace = FALSE)
    Player1 <- Select[1]
    Player2 <- Select[2]
    if(Player1 < Player2){
    Empty_list[[i]] <- c(Player1,Player2)
    }else{
      Empty_list[[i]] <- c(Player2,Player1) 
    }
   State = Play(maxpoint,auto,Player1,Player2,output) # Return status of ball 
   if(Player1==Player2){
     if(State %in% c(-1,-2,-3)){
       Matrix_Wins[1,i]=1 # Represent win for Player1
       Matrix_Wins[2,i]=-1 #Represents loss for Player2
     }else if(State %in% c(1,2,3)){
       Matrix_Wins[1,i]=-1 #Represents loss for Player1
       Matrix_Wins[2,i]=1  # Represents win for Player2
     }
   }else{
   if(State %in% c(-1,-2,-3)){
     Matrix_Wins[as.character(Player1),i]=1 # Represent win for Player1
     Matrix_Wins[as.character(Player2),i]=-1 #Represents loss for Player2
   }else if(State %in% c(1,2,3)){
     Matrix_Wins[as.character(Player1),i]=-1 #Represents loss for Player1
     Matrix_Wins[as.character(Player2),i]=1  # Represents win for Player2
   }
   }
  }
  Un <- sort(unique(c(Matrix_Wins))) # Sort the matrix of reward 
  Result <- t(apply(Matrix_Wins,1, function(x) table(factor(x,Un)))) #column_1: -1 Column_2: 1
  Final <- as.array(Result)
  for(x in 1:nrow(Final)){ # Loop over the matrix
    Per_rate <- round((Final[x,2]/(Final[x,1]+Final[x,2]))*100,1)
    Sum <- Final[x,1]+Final[x,2]
 cat("Player",Player[x],"wins: ",Per_rate,"%","(",Final[x,2],"out of ",Sum,"games",")","\n") 
  }
  print(as.matrix(table(sapply(Empty_list, deparse))))
  }
  if(Number_of_Players > 1)
    return(Final)
} 



######### Multiple times of play by machines 
######### Return matrix of victory 

ChainsofPlay <- function(maxpoint=50,auto=2,Player,PlayNumber,outname="1_2",output="No"){
  Number <- length(Player)
  Matrix_Plays <- matrix(nrow = Number,ncol = PlayNumber,byrow = TRUE,
                         dimnames = list(Player,1:PlayNumber)) # Empty matrix
  Store <- list() # Stores the outcome of each machines
  for(n in 1:PlayNumber){
    States = Playchain(maxpoint,auto,Player,Game_number = 10000,output = "No")
    Store[[n]] <- States # List of matrices of scores for each play.
  }
  Vic = length(Store)
  if(Player[1]==Player[2]){
    for(x in 1:Vic){
      for(y in 1:length(Player)){
        Matrix_Plays[y,x]=round((Store[[x]][y,2]/(Store[[x]][y,1]+Store[[x]][y,2]))*100,3)
      }
    }
    for (v in 1:nrow(Matrix_Plays)) {
      a <- sum(Matrix_Plays[v,])/PlayNumber
      cat("Average winning probability of","Player",Player[v],":",a,"\n")
    }
  }else{
  for(x in 1:Vic){
    for(y in Player){
      Post=as.character(y)
      Matrix_Plays[Post,x]=round((Store[[x]][Post,2]/(Store[[x]][Post,1]+Store[[x]][Post,2]))*100,3)
    }
  }
    for (v in 1:nrow(Matrix_Plays)) {
      a <- sum(Matrix_Plays[v,])/PlayNumber
      cat("Average winning probability of ","Player",Player[v],":",a,"\n")
    }
  }
  path="Folder name" ### Path for the display of plots
  output1 <- paste(path,outname,".pdf",sep="")
  pdf(file = output1,  width = 11, height = 8)
  for(w in 1:nrow(Matrix_Plays)){
    xdxdxd <- Matrix_Plays[w,]
    mid_x <- (min(xdxdxd)+max(xdxdxd))/2
    low_x <- floor(min(c(mid_x-2.45,xdxdxd))*10)/10
    upp_x <- ceiling(max(c(mid_x+2.45,xdxdxd))*10)/10
   print(c(low_x,upp_x,mid_x))
    plot(xdxdxd,type = "l",ylab = "Percentage of wins",xlab = "Number of Play",lwd=2,
        cex.axis=2,cex.main=2,cex.lab=1.8, ylim=c(0 ,100),
         main = substitute(paste("Number of wins by: ","Machine_",a),list(a=Player[w])))
  }
  #dev.off() 
  return(Matrix_Plays)
}






### Final Run code:
#erstinit()
#ChainsofPlay(maxpoint=50,auto=2,c(2,16),PlayNumber=150,output="No",outname = "2_16")


par(mfrow=c(1,3))
plot(D1[2,],type = "l",ylab = "Percentage of wins",xlab = "Number of Play",
     lwd=2, cex.axis=2,cex.main=2,cex.lab=1.8,xlim = c(0,50),ylim = c(23,100),
     main = "machine 8 ")
plot(D2[2,],type = "l",ylab = "Percentage of wins",xlab = "Number of Play",
     lwd=2, cex.axis=2,cex.main=2,cex.lab=1.8,xlim = c(0,100),ylim = c(40,80),
     main = "machine 11 ")
plot(D3[2,],type = "l",ylab = "Percentage of wins",xlab = "Number of Play",
     lwd=2, cex.axis=2,cex.main=2,cex.lab=1.8,xlim = c(0,200),ylim = c(11.9,56.1),
     main = "machine 13 ")


#### Sample experiments of machines.

plot(x = x,y = d1[2,], type="l",col="red",ylim = c(0,100),lwd=2,
     main = "Comparison of untrained and trained machine",ylab = "Percentage of win",
     xlab = "Number of play")
lines(x,d2[2,],col="blue",lwd=2)
legend("bottomright", legend = c("Untrained machine","Trained machine"), col=c("red","blue"), pch=1,angle = 90,
       pt.cex = 1,cex = 1,lwd = 2)

plot(x = x,y = d1[2,], type="l",col="red",ylim = c(0,100),lwd=2,
     main = "Comparison of untrained and trained machine",ylab = "Percentage of win",
     xlab = "Number of play")
lines(x,d3[2,],col="blue",lwd=2)
legend("bottomright", legend = c("Untrained machine","Trained machine"), col=c("red","blue"), pch=1,angle = 90,
       pt.cex = 1,cex = 1,lwd = 2)




##### Expected output with outcome curves.....


#Average winning probability of Player 16 : 80.15279 
#Average winning probability of Player 16 : 19.84721 
#[1] 69.800 84.700 77.249
#[1] 15.300 30.200 22.751


#Average winning probability of  Player 13 : 27.65107 
#Average winning probability of  Player 16 : 72.34893 
#[1] 21.90 56.40 39.18
#[1] 43.60 78.10 60.82



#Average winning probability of Player 16 : 74.64762 
#Average winning probability of Player 16 : 25.35238 
#[1] 57.0000 79.9000 68.4425
#[1] 20.1000 43.0000 31.5575

#Average winning probability of  Player 13 : 53.16627 
#Average winning probability of  Player 16 : 46.83373 
#[1] 45.00 75.40 60.19
#[1] 24.60 55.00 39.81





