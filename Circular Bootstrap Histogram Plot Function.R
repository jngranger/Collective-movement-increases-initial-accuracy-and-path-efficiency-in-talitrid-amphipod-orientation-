bootstrap.histogram.plot.circular=function(Variable, GS, color="#AF4E4E",R=300, title="", xaxis="", basesize=16,numbins=400, scaler=1)
{
  
  #Variable is the Variable of interest to plot
  #GS is the GS of the underlying distribution
  #color sets the plot color
  #R is the radius of the arena
  #title is the plot title
  #xaxis controls the title for the x axis
  #basesize lets you control the size of the text
  #numbins is the total number of bins the headings get grouped into
  #scaler sets the size gap between stacked dots

  bins=seq(from=-pi, to=pi, length.out=numbins)

  #calculate the p-value
  #mean center the underlying distribution and make sure the scale runs from -pi to pi  
  meanheading=(mean.circular(GS[[Variable]]))
  meancentered_Dist=((GS[[Variable]]-meanheading)+(2*pi))%%(2*pi)
  meancentered_Dist=if_else(meancentered_Dist<pi,meancentered_Dist,meancentered_Dist-(2*pi))
  
  #mean center the GS1 value and scale in the same way
  meancentered_Value=((df.GS1.mean[[Variable]]-mean.circular(GS[[Variable]])) +(2*pi))%%(2*pi)
  meancentered_Value=if_else(meancentered_Value<pi,meancentered_Value,meancentered_Value-(2*pi))
  
  #calculate the pvalue as the #of points above or below the GS1value
  pvalue=round(ifelse(meancentered_Value>0,
                length(meancentered_Dist[meancentered_Dist>=(meancentered_Value)])/length(meancentered_Dist),
                length(meancentered_Dist[meancentered_Dist<=(meancentered_Value)]) / length(meancentered_Dist)
  ),4)
  
  if (pvalue !=0) {pvalue=paste0("p=",pvalue)}
  if (pvalue==0) { pvalue="p<0.0001" }
  
  # The plot
  
  #wrangle the data
  GS |>
    #The variable to plot
    rename(Variable=!!Variable) |> 
    ungroup() |> #make sure there's no grouping
    
    #Break the angles into bins such that you end up with 100 total bins
    #Set the labels to be the first angle of the bin (this results in a label that is n-1 length compared to the seq used to build the breaks)
    mutate(headingbins=as.numeric(as.character(cut(Variable,
                                                   breaks = bins,
                                                   labels = bins[1:(length(bins)-1)]
    )))) |> 
    #Group by these bins
    group_by(headingbins) |> 
    #within groups set a row number
    mutate(n=row_number()) |> 
    #For each row number, set the radius at which that will be plotted to be R + (the row number*a scaler to space them nicely)
    mutate(Radius=R+(n*scaler)) |> 
    #Recalculate the new x y points along the outside of the plotted circle
    mutate(XPlot=Radius*cos(headingbins),
           YPlot=Radius*sin(headingbins)) |>
    ungroup() |> 
    
    #plotting
    ggplot()+
      theme_classic(base_size = basesize)+
      theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank(), axis.line = element_blank())+
      #Make the circles for the outer circle
      annotate("path",x=R*cos(seq(0,2*pi,length.out=100)),y=R*sin(seq(0,2*pi,length.out=100)),linewidth=1.2)+
      #Plot the points
      geom_point(aes(XPlot,YPlot), color=color)+
      #Add a center +
      annotate(geom="point", x=c(0),y=c(0), shape=3)+
      #Add the direction markers (0-pi)
      annotate(geom="text",x=c(R-(R*0.1)),y=c(0),label="0")+
      annotate(geom="text",x=c(-R+(R*0.1)),y=c(0),label="~pi", parse=T)+
      # annotate(geom="text",x=c(0),y=c(R-30),label="~pi/2", parse=T)+
      # annotate(geom="text",x=c(0),y=c(-R+30),label="3~pi/2", parse=T)+
      
      #Add the GS1 value
      annotate(geom="segment",x=0,y=0,xend=0+as.numeric((R)*cos(df.GS1.mean[[Variable]])),yend=0+as.numeric((R)*sin(df.GS1.mean[[Variable]])),
               size=2, lty="11")+
      
      #label and scale
      labs(title = paste0(title), x=paste0(xaxis))+
      scale_y_continuous(limits = c(-R-30,R+210))+
      
      #Add pvalue
      annotate(geom = "label", x=-R, y=R, label=paste0(pvalue), hjust=0, vjust=-0.1,size=5)+
      coord_fixed(clip="off")
  
}