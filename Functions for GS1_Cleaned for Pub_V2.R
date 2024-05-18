

WrangleTracks=function(df, Center_X, Center_Y, AngleBeach){
  
  #Select the tracks before they hit the wall
  df1=df %>%
    dplyr::select(paste0("Prewall","_X"),paste0("Prewall","_Y"),Time) %>%
    rename(X=paste0("Prewall","_X"),Y=paste0("Prewall","_Y"))
  
  #cut from where they start moving down
  start=which(df$Latency=="Start")
  df2=df1[start:nrow(df1),]
  
  #Interpolate, this also removes excess NAs at the end
  df.tmp=data.frame("X"=na.approx(df2$X),"Y"=na.approx(df2$Y)) %>%
    #mean-center the data such that center is always 0,0. These are also now the distance between x,y and centerx,y
    mutate(Center_X=X-Center_X,
           Center_Y=Y-Center_Y) |> 
    #Get the distance between each point and the center
   mutate(Dist_Center=sqrt(Center_X^2+Center_Y^2)) |> 
    
    #Calculate the angle between each point and center
    
    mutate(AngleCenter=as.circular(atan2(Center_Y, Center_X),
                                            type="angles",units="radians",
                                            template="none",modulo="asis",
                                            zero=0,rotation="counter")) |> 
    #The distance between each step 
    
    mutate(MinPrev_X=X-lag(X),
           MinPrev_Y=Y-lag(Y),
           Dist_Each=sqrt(MinPrev_X^2+MinPrev_Y^2)) |> 
    #Rotate the angles such that the beach is now at 0
    mutate(AngleCenter_Transformed=AngleCenter-AngleBeach) |> 
    #Calculate the new X and Y given this rotation
    mutate(X.Transformed=Dist_Center*cos(AngleCenter_Transformed),
           Y.Transformed=Dist_Center*sin(AngleCenter_Transformed)) |> 
    #Calculate their HEADING using this new rotated X, Y (a heading of 0 should be towards the beach)
    mutate(Heading=as.circular(atan2(Y.Transformed-lag(Y.Transformed),X.Transformed-lag(X.Transformed)),type="angles",units="radians",
           template="none",modulo="asis",
           zero=0,rotation="counter")) |> 
  
    ##############################################################################################################
    ######################################Comment out this section to remove the "jitter" ########################

    ##This is to prevent the problem where atan(0,0)=0 when it should be nothing. Instead, make it a random number 0-360
    rowwise() |>
   # mutate(
     # Heading=if_else(MinPrev_X==0 && MinPrev_Y==0,
     #                                as.circular(runif(1,0,2*pi),type="angles",units="radians",
     #                                            template="none",modulo="asis",
     #                                            zero=0,rotation="counter"),
     #                                Heading)) |>
    #Or set it as NA instead
     mutate(
       Heading=if_else(MinPrev_X==0 && MinPrev_Y==0,NA,Heading)) |>
    ungroup()
    ##############################################################################################################

   
  #add back in the time row
  df.tmp %>%  mutate(Time=df2$Time[1:nrow(df.tmp)])
}


sandhoppers.GS1.data=function(df){
  
  #Add a time column, which is the row number (frame) minus the time the cup is lifted
  #This gives us Time as "time since cup was lifted", in frames. Divide by 30 for seconds
  cup=which(df$Latency=="Cup")
  df=df %>% 
    mutate(Time=row_number(),
           Time=Time-cup)
  
  #Get the center point. Note there might be multiple #s here, so make sure to use just the 
  #first row for calculations
  Center_X=as.numeric(last(select(df,Center_X) |> drop_na()))
  Center_Y=as.numeric(last(select(df,Center_Y) |> drop_na()))

  #Beach Location. First row should be center, second the direction of the beach.
  Beach_X=as.numeric(select(df,Beach_X) |> drop_na() |> last())
  Beach_Y=as.numeric(select(df,Beach_Y) |> drop_na() |> last())
  AngleBeach=as.circular(atan2((Beach_Y-Center_Y), (Beach_X-Center_X)),
                         type="angles",units="radians",
                         template="none",modulo="asis",
                         zero=0,rotation="counter")
  

 #Get the trial-specific info
  df.extra=data.frame(GS=df$GS[1],Trial=df$Trial[1],
                      Center_X,Center_Y,Beach_X,Beach_Y,
                      AngleBeach,
                      Date=df$Date[1],Weather=df$Weather[1],
                      Temp=df$Temp[1],TOD=df$TOD[1],
                      Dig=df$Dig[1])
  
  #Clean the tracks
  dftracks=WrangleTracks(df, Center_X, Center_Y, AngleBeach) |> 
    mutate(Trial=rep(df.extra$Trial),
           Animal=rep(1)) |> 
    #Filter to the first NA value for heading (remove cases where there were several non-movement points at the beginning)
    filter(lead(cummax(!is.na(Heading)),default = 1)>0)
    
    ##############################################################################################################
    ######################################Comment out this section to remove the "jitter" ########################
    #ADDING IN JITTER FOR HOLDING STILL
    #IF THEY DIDN'T MOVE (DIST_EACH=0), THEN ADD NOISE PULLED FROM AN EXPONENTIAL DISTRIBUTION WITH RATE=8
    # rowwise() |> 
    # mutate(Dist_Each=if_else(Dist_Each==0,Dist_Each + rexp(1,8),Dist_Each)) |> 
    # ungroup()
    ################################################################################################################
    
   Final=dftracks |> 
      mutate(Dist_Each=if_else(is.na(Dist_Each),0,Dist_Each)) |> 
      summarise(Dist_Actual=(sum(Dist_Each,na.rm=T)),
                Dist_StrtLine=(sqrt( (first(X)-last(X))^2+(first(Y)-last(Y))^2  )),
                Sinuosity=Dist_StrtLine/Dist_Actual,
                Time=last(Time)-first(Time),
                FinalHeading=as.circular(last(AngleCenter_Transformed),type="angles",units="radians",
                                         template="none",modulo="asis",
                                         zero=0,rotation="counter"),
                HeadingMean=weighted.mean.circular(Heading,Dist_Each,na.rm=T),
                StartHeading=nth(Heading,2),
                Velocity=(Dist_Actual)/Time,
                "Start_X" = first(X),
                "Start_Y" = first(Y),
                "Start_X.T" = first(X.Transformed),
                "Start_Y.T" = first(Y.Transformed),
                StartLocation=as.circular(atan2(Start_Y.T,Start_X.T),
                                          type="angles",units="radians",
                                          template="none",modulo="asis",
                                          zero=0,rotation="counter"),
                "End_X"=last(X),
                "End_Y"=last(Y),
                "Latency"=which(df$Latency=="Start")-which(df$Latency=="Cup")
      )


  
  dfall=cbind(df.extra,Final)
  
  list(dfall,dftracks)
  
}



