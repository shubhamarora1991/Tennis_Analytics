ratings <- rbind(atp_rating,wta_rating)
ratings$player_id <- as.character(ratings$player_id)
data$Player_ID.x <- as.character(data$Player_ID.x)
data$Player_ID.y <- as.character(data$Player_ID.y)
data$Scheduled <- as.POSIXct(data$Scheduled)
attri = "Breakpoints_won.x"
datestart1 = as.POSIXct("2016-08-01")
datestart2 = as.POSIXct("2018-08-01")

playerdt <- function(playername,datestart1,datestart2){

  
  subdata = data[(data$Scheduled > datestart1 & data$Scheduled < datestart2),]
  player_matches <- data[subdata$Player_name.x == playername |subdata$Player_name.y == playername ,]
  pid <- ratings[ratings$Player_Name == playername,'player_id'][1]
  player_country <- ratings[ratings$player_id == pid,'Nationality_Country_Full']
  current_points <- ratings[ratings$player_id == pid,'Rating_Points']
  current_rank <- ratings[ratings$player_id == pid,'Rating_rank']
  aces_1 <- subdata[subdata$Player_name.x == playername,'Aces.x']
  aces_2 <- subdata[subdata$Player_name.y == playername,'Aces.y']
  total_aces = sum(c(aces_1,aces_2),na.rm = T)
  bp_1 <- subdata[subdata$Player_name.x == playername,'Breakpoints_won.x']
  bp_2 <- subdata[subdata$Player_name.y == playername,'Breakpoints_won.y']
  total_bp = sum(c(bp_1,bp_2),na.rm = T)
  
  df <- data.frame(playername,player_country,current_points,current_rank,dim(player_matches)[1],total_aces,total_bp)
  colnames(df) <- c('Player Name','Player Country','Current Points','Current Ranking','Number of Matches','Total Aces','Total Breakpoints')
  return(df)
  
}


getplayerinfo <- function(Player_Data, player_name){
  
  return(Return_stats <- Player_Data[Player_Data$Player.Name == player_name,])
}

pred_gamespermatch <- function(Aces,Breakpoints_won,double_faults,first_serve_points_won,first_serve_successful,
                               Reciever_points_won,second_serve_successful,service_games_won,tiebreaks_won)
{
  pred_val <- -0.4174 + 1.3626*Aces+ 3.9073*Breakpoints_won - 0.2096*double_faults - 2.0816*first_serve_points_won+
    5.1657*first_serve_successful + 2.1799*Reciever_points_won + 0.6149*second_serve_successful+
    8.7373*service_games_won + 0.8727*tiebreaks_won
  return(pred_val)
  
}  
