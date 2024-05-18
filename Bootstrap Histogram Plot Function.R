bootstrap.histogram.plot=function(Variable,GS,bins=30,maxy=0.25,yaxis=TRUE,xaxis=Variable, title="", basesize=16)
{
  #Variable is the Variable of interest to plot
  #GS is the GS of the underlying distribution
  #bins is the number of bins for the histogram
  #maxy is the maximum limit for the y axis
  #yaxis is a binary variable of whether or not to include the yaxis text, and ticks
  #xaxis controls the title for the x axis
  #title is the plot title 
  #basesize controls the plot's base text size
  
  
  #Get the correct df
  if(GS=="GS5"){UnderlyingDist=(GS1.5[[Variable]])}
  if(GS=="GS10"){UnderlyingDist=(GS1.10[[Variable]])}
  
  #Calculate the pvalue
  meancentered_Dist=UnderlyingDist-mean(UnderlyingDist)
  meancentered_Value=(df.GS1.mean[[Variable]]-mean(UnderlyingDist))
  pvalue=round(
    ifelse(meancentered_Value>0,
                (sum(meancentered_Dist>=(meancentered_Value)))/length(meancentered_Dist),
                (sum(meancentered_Dist<=(meancentered_Value)))/length(meancentered_Dist)
  ),4)
  if (pvalue !=0) {pvalue=paste0("p=",pvalue)}
  if (pvalue==0) { pvalue="p<0.0001" }
  
  #color for the histogram
  color=ifelse(GS=="GS5","darkred","darkorange")
  
  #Max and min x value
  max.x=max(c(max(GS1.5[[Variable]]),max(GS1.10[[Variable]]),(df.GS1.mean[[Variable]])))
  min.x=min(c(min(GS1.5[[Variable]]),min(GS1.10[[Variable]]),(df.GS1.mean[[Variable]])))
  
  
 #The plot
  p=ggplot()+
    theme_classic(base_size = basesize) +
    geom_histogram(aes(x = UnderlyingDist), 
                   bins = bins, fill = color,col = "black", alpha = 0.7, position = "identity")+
    geom_vline(xintercept = df.GS1.mean[[Variable]], col = "black", linewidth = 1.7, lty="22") +
    labs(x = paste0(xaxis), y = "Percent", title = paste0(title))+
    annotate(geom="label",label=paste0(pvalue),x=min.x,y=maxy,hjust=0,vjust=1,fill="white", size=5)+
    aes(y = after_stat(count)/sum(after_stat(count))) + 
    scale_y_continuous(labels = scales::percent,limits=c(0,maxy))+
    coord_cartesian(xlim = c(min.x,max.x), clip="off")+
    theme(axis.title.y = element_blank())
  
  #changing the axes as needed
  if (yaxis==F) {
    p=p+theme( axis.text.y = element_blank(),
               axis.ticks.y = element_blank())
  }

  
  p
  
  
}