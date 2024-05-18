## Function to make circular plots


circularplot=function(Variable,basesize=14, xlab=Variable,  R=300, df=GSall.alltrials,numbins=70, scaler=10)
{
  #Variable is the variable to plot (angles in radians)
  #basesize lets you control the size of the text
  #xlab lets you set the xaxis title
  #R is the radius of the arena
  #df is the data frame to plot
  #numbins is the total number of bins the headings get grouped into
  #scaler sets the size gap between stacked dots
  
  
  bins=seq(from=-pi, to=pi, length.out=numbins)
  
  df=df |>
    #Filter to the data we want
    filter(Weather=="Sunny", Smoky=="Not Smoky", Time<18) |> 
    rename(Variable=!!Variable) |> 
    group_by(GS.Trial) |> 
    #The variable to plot
    summarise(Variable=mean.circular(Variable)) |> 
    mutate(
      Variable=(Variable+(2*pi))%%(2*pi),
      Variable=if_else(Variable<pi,Variable,Variable-(2*pi))
    ) |> 
    #Break the angles into bins such that you end up with n total bins
    #Set the labels to be the first angle of the bin (this results in a label that is n-1 length compared to the seq used to build the breaks)
    mutate(headingbins=as.numeric(as.character(cut(Variable,
                                                   breaks = bins,
                                                   labels = bins[1:(length(bins)-1)] )))) |> 
    #Group by these bins
    group_by(headingbins) |> 
    #within groups set a row number
    mutate(n=row_number()) |> 
    #For each row number, set the radius at which that will be plotted to be R + (the row number*a scaler to space them nicely)
    mutate(Radius=R+(n*scaler)) |> 
    #Recalculate the new x y points along the outside of the plotted circle
    mutate(XPlot=Radius*cos(headingbins),
           YPlot=Radius*sin(headingbins)) |>
    ungroup()
  
  df.summary=df |> 
    #calculate the mean heading, rho, p-value, and CI
    summarise(MeanHeading=mean.circular(Variable),
              Rho=rho.circular(Variable),
              pvalue=round(rayleigh.test(Variable)$p.value,3),
              CI1=mle.vonmises.bootstrap.ci(Variable)$mu.ci[1],
              CI2=mle.vonmises.bootstrap.ci(Variable)$mu.ci[2]
    ) |> 
    mutate(pvalue=if_else(pvalue==0,"p<0.001",paste0("p=",pvalue)))
  
  

  ggplot()+
    theme_classic(base_size = basesize)+
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank(), axis.line = element_blank(),
          legend.position = "none")+
    #Make the circles for the outer circle
    annotate("path",x=R*cos(seq(0,2*pi,length.out=100)),y=R*sin(seq(0,2*pi,length.out=100)),linewidth=1.2)+
    
    #Plot the points
    geom_point(data=df,aes(XPlot,YPlot), size=2)+
    
    #Add a center +
    annotate(geom="point", x=c(0),y=c(0), shape=3)+
    
    #Add the direction markers (0-pi)
    annotate(geom="text",x=c(R-(R/7.5)),y=c(0),label="0")+
    annotate(geom="text",x=c(-R+(R/7.5)),y=c(0),label="~pi", parse=T)+
    annotate(geom="text",x=c(0),y=c(R-(R/7.5)),label="~pi/2", parse=T)+
    annotate(geom="text",x=c(0),y=c(-R+(R/4.3)),label="3~pi/2", parse=T)+
    
    #Add the Mean Heading arrow
    geom_segment(data=df.summary,aes(x=0,y=0,xend=0+as.numeric((R*Rho)*cos(MeanHeading)),yend=0+as.numeric((R*Rho)*sin(MeanHeading))),
                 arrow = arrow( length = unit(0.15, "inches")),linewidth=1.5, lineend = "butt",linejoin = "mitre", color="burlywood")+
    #y axis label and scale
    labs(x=paste0(xlab))+

    #Add pvalue from the rayleigh test
    geom_label(data=df.summary,aes(x=0,y=R+(R/4), label=paste0(pvalue)))+
    #Add CI lines
    geom_segment(data=df.summary,aes(x=0, y=0, xend=0+as.numeric(R*cos(CI1)),yend=0+as.numeric(R*sin(CI1))),
                 linewidth=1.3,lty="11")+
    geom_segment(data=df.summary,aes(x=0, y=0, xend=0+as.numeric(R*cos(CI2)),yend=0+as.numeric(R*sin(CI2))),
                 linewidth=1.3,lty="11")+
    #fixed coordinates to make sure the 
    coord_fixed(clip="off")
}



