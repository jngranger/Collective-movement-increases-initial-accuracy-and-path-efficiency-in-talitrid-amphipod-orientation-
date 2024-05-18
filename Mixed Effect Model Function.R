new.lmer=function(Response,Predictor,randomeffects,df=GSall.alltrials){
  #Response is the Response Variable
  #Predictor is the Predictor Variable
  #Random Effects controls how many random effects to include. 
    ## 1 = GS only. Used for the models where the predictor is weather and the response is rho since there is no longer a trial variable
    ## 2 = GS and Trial. Used for all other models where the predictor is weather
    ## 3 = GS, Trial and Weather. Used for all models where the predictor is Year, except for when the response is rho
    ## 4 = GS and Weather. Used for models where the predictor is Year, and the response is rho since there is no longer a trial variable
  #df is the dataframe to use. Different for the models using rho
  
  #get response and predictor
  y=df[[Response]]
  x=df[[Predictor]]
  
  # Full Model for calculating the main effect estimate and standard error
  if(randomeffects == 1) { model.tmp=lmer(y~x + (1|GS),data=df)}
  if(randomeffects == 2) { model.tmp=lmer(y~x + (1|GS) + (1|GS.Trial),data=df)}
  if(randomeffects == 3) { model.tmp=lmer(y~x + (1|GS) + (1|GS.Trial) + (1|Weather),data=df)}
  if(randomeffects == 4) { model.tmp=lmer(y~x + (1|GS) + (1|Weather),data=df)}
  
  #Get the estimate and std.error from the full model
  results1=summary(model.tmp)
  Estimate=results1$coefficients[2,1]
  Std.Error=results1$coefficients[2,2]
  
  # Full and reduced models for calculating the ANOVA p-value
  if(randomeffects == 1){ full.lmer = lmer(y ~ x + (1|GS) ,data = df, REML =  FALSE) }
  if(randomeffects == 2){ full.lmer = lmer(y ~ x + (1|GS) + (1|GS.Trial),data = df, REML =  FALSE) }
  if(randomeffects == 3){ full.lmer = lmer(y ~ x + (1|GS) + (1|GS.Trial) + (1|Weather),data = df, REML =  FALSE) }
  if(randomeffects == 4){ full.lmer = lmer(y ~ x + (1|GS) + (1|Weather),data = df, REML =  FALSE) }
  
  if(randomeffects == 1){reduced.lmer <- lmer(y ~ 1 + (1|GS),data = df, REML = FALSE)}
  if(randomeffects == 2){reduced.lmer <- lmer(y ~ 1 + (1|GS) + (1|GS.Trial),data = df, REML = FALSE)}
  if(randomeffects == 3){reduced.lmer <- lmer(y ~ 1 + (1|GS) + (1|GS.Trial) + (1|Weather),data = df, REML = FALSE)}
  if(randomeffects == 4){reduced.lmer <- lmer(y ~ 1 + (1|GS) + (1|Weather),data = df, REML = FALSE)}
  
  #get the pvalue from the ANOVA
  results2=anova(reduced.lmer, full.lmer)
  pvalue=results2$`Pr(>Chisq)`[2]
  
  # Combining the data into a table and set the sigcode based on the p-value
  data.frame(Response=Response, "Main Effect"=Predictor,Estimate=Estimate,Std.Error=Std.Error, pvalue=round(pvalue,3)) |> 
    mutate(SigCode=if_else(pvalue<0.0005, "***",
                           if_else(pvalue<0.001, "**",
                                   if_else(pvalue<=0.006, "*", 
                                           if_else(pvalue<0.01,".","n.s.")))))
  
}