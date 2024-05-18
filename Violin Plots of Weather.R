require(ggforce)
environmentalplots=function(yvar,xvar="Weather",pvalue=filter(TableS1,Response==yvar,Main.Effect==xvar)$SigCode,ylab=yvar,xlab=xvar,no.x=T,colors=c("grey","yellow"), a=0.5, df=GSall.alltrials, textsize=13, psize=5){
  #yvar is the y variable to plot
  #xvar is the x variable to plot (in this case, always weather)
  #pvalue is the pvalue from the MEM's. Pull from table S1. Will need to be slightly different for Latency since we used the Latency2 value in the plot but not in the MEM and Velocity since we renamed it to speed
  #ylab and xlab are the y and x titles
  #no.x is true if you want to get rid of the x axis text, title, and ticks
  #colors are the colors for the plot
  #a is the alpha/transparency level
  #df is the dataframe to plot (will be different for rho y variables)
  #textsize is the size of the text
  #psize is the size of the pvalue text
  
  p=df |> 
    #rename the variables to plot
    rename(yvar=!!yvar,
           xvar=!!xvar) |> 
    #the plot
    ggplot()+
    theme_classic()+
    theme(text=element_text(size=textsize),
          legend.position = "none")+
    labs(x=paste0(xlab),y=paste0(ylab))+
    geom_violin(aes(xvar,yvar,fill=xvar),alpha=a)+
    ggforce::geom_sina(aes(xvar, yvar,fill=xvar),shape=21,alpha=0.5)+ #points
    scale_fill_manual(values = colors)
  
  if (no.x==T) {
    p=p+
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank())
  }
  
  #add pvalue
  p+annotate(geom="label",label=paste0(pvalue),x=1.5,y=max(df[[yvar]]),vjust=1,fill="white",size=psize)
}
