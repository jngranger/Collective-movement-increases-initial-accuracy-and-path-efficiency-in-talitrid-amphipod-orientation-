circular.violin.plot=function(Variable,Variable2="",GS5=F, basesize=15, ylab=Variable, psize=5,R=300,scaler=10, Center2=800,
                              numbins=70, GSsize=4.5)
{
  #Variable is the Variable of interest to plot
  #GS5 is either true if you are plotting the GS5 data or false if you are plotting the GS10 data
  #basesize lets you control the size of the text
  #ylab controls the title for the y axis
  #psize lets you control the size of the text specifically for the pvalue
  #R is the radius of the arena
  #scaler sets the size gap between stacked dots
  #Center2 sets the x coordinates of the center of the second circle
  #numbins is the total number of bins the headings get grouped into
  #GSsize is the size of the GS labels
  
  bins=seq(from=-pi, to=pi, length.out=numbins)
  
  # # we want to plot both the mean and rho pvalue on this plot, so set Variable2 to be rho_"Variable Name"
  # Variable2=paste0("rho_",Variable)
  
  #filter and rename the variable of interest
  tmp=GSall.alltrials |> 
    filter(Weather=="Sunny", Smoky=="Not Smoky") |> 
    rename(Variable=!!Variable) 
  
  #select either GS5 or GS10
  
  if (GS5==T) {
    UDm=GS1.5[[Variable]]
    UDr=GS1.5[[Variable2]]
    color="#AF4E4E"
    df=tmp |> 
      filter(GS!=10) |> 
      mutate(Center_X=if_else(GS==1,0,Center2)) |>  #Set a new center variable for each GS so that when you go to plot it they plot separately
      mutate(GS=recode(GS,"1"="GS1", "5"="Group")) #Rename the facets
    label=c("GS1","GS5")
  }
  
  if (GS5==F) {
    UDm=GS1.10[[Variable]]
    UDr=GS1.10[[Variable2]]
    color="#FBAD4F"
    df=tmp |> 
      filter(GS!=5) |> 
      mutate(Center_X=if_else(GS==1,0,Center2)) |> #Set a new center variable for each GS so that when you go to plot it they plot separately
      mutate(GS=recode(GS,"1"="GS1", "10"="Group"))#Rename the facets
    label=c("GS1","GS10")
  }
  
  
  # Calculating the p-value for the circular variable
  MC_UD=UDm-mean(UDm)
  MC_V=(df.GS1.mean[[Variable]]-mean(UDm))
  pd=ifelse(MC_V>0,
            (sum(MC_UD>=(MC_V)))/length(MC_UD),
            (sum(MC_UD<=(MC_V)))/length(MC_UD)
  )
  
  # Calculating the p-value for the rho variable
  MC_UD=UDr-mean(UDr)
  MC_V=(df.GS1.mean[[Variable2]]-mean(UDr))
  pMRV=ifelse(MC_V>0,
              (sum(MC_UD>=(MC_V)))/length(MC_UD),
              (sum(MC_UD<=(MC_V)))/length(MC_UD)
  )
  
 
  #The plot 
  df |>
    ungroup() |> 
    #Break the angles into bins such that you end up with n total bins
    #Set the labels to be the first angle of the bin (this results in a label that is n-1 length compared to the seq used to build the breaks)
    mutate(headingbins=as.numeric(as.character(cut(Variable,
                                                   breaks = bins,
                                                   labels = bins[1:(length(bins)-1)] )))) |> 
    #Group by these bins
    group_by(GS,headingbins) |> 
    #within groups set a row number
    mutate(n=row_number()) |> 
    #For each row number, set the radius at which that will be plotted to be R + (the row number*a scaler to space them nicely)
    mutate(Radius=R+(n*scaler)) |> 
    #Recalculate the new x y points along the outside of the plotted circle
    mutate(XPlot=Center_X+Radius*cos(headingbins),
           YPlot=Radius*sin(headingbins)) |>
    
    #calculate the mean heading and rho for each group
    group_by(GS) |> 
    mutate(MeanHeading=mean.circular(Variable),
           Rho=rho.circular(Variable)) |> 
    
    #plotting
    ggplot()+
      theme_classic(base_size = basesize)+
      theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.x = element_blank(), axis.line = element_blank(),
            legend.position = "none")+
      
      #Make the circles for the outer circle
      annotate("path",x=R*cos(seq(0,2*pi,length.out=100)),y=R*sin(seq(0,2*pi,length.out=100)),linewidth=1.2)+
      annotate("path",x=Center2+R*cos(seq(0,2*pi,length.out=100)),y=R*sin(seq(0,2*pi,length.out=100)),linewidth=1.2)+
      
      #Plot the points
      geom_point(aes(XPlot,YPlot, fill=GS), shape=21, alpha=0.5)+
      #Add a center +
      annotate(geom="point", x=c(0,Center2),y=c(0,0), shape=3)+
    
      #Add the direction markers (0-pi)
      annotate(geom="text",x=c(R-40,Center2+R-40),y=c(0,0),label="0")+
      annotate(geom="text",x=c(-R+40,Center2-R+40),y=c(0,0),label="~pi", parse=T)+
      # annotate(geom="text",x=c(0,Center2,Center3),y=c(R-40,R-40,R-40),label="~pi/2", parse=T)+
      # annotate(geom="text",x=c(0,Center2,Center3),y=c(-R+70,-R+70,-R+70),label="3~pi/2", parse=T)+
      
      #Add the Mean Heading arrow
      geom_segment(aes(x=Center_X,y=0,xend=Center_X+as.numeric((R*Rho)*cos(MeanHeading)),yend=0+as.numeric((R*Rho)*sin(MeanHeading))),
                   arrow = arrow( length = unit(0.1, "inches")),linewidth=1, lineend = "butt",linejoin = "mitre")+
    
      #y axis label and scale
      labs(y=paste0(ylab))+
      coord_fixed(clip="off")+
      #yaxis scale
      scale_y_continuous(limits = c(-R-110,R+280))+
      
      #pvalues and line
      annotate(geom="segment",x=0,xend=Center2,y=R+70,yend=R+70)+
      annotate(geom = "text", x=Center2/2, y=R+70, label=paste0("~p[d]==~", round(pd,4),"~','~","~p[MRV]==~", round(pMRV,4)), vjust=-0.4, parse=T,size=psize)+
      
      #Add GS labels
      annotate(geom="text",x=c(0,Center2),y=c(-R-100,-R-100),label=label, alpha=0.7, size=GSsize)+
      #Add colors
      scale_fill_manual(values = c(color,"black"))
      
  
}
