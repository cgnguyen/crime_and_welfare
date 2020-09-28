####Setup####
  library(needs)
  needs(essurvey)  
  needs(tidyverse)  #Tidyverse helps us with many helper functions 
  needs(car)        #I still clean my data using car
  needs(plotrix)    #For standard error function
  needs(ggthemes)   #To make ggplot look nicer
  needs(haven)      #Extract labels etc
  needs(texreg)  
  needs(broom)
  needs(MASS)      #To run ordered logit models
  needs(sjPlot)
  needs(arm)
  
####Download Individual Level ESS Data####
     set_email("cgnguyen@gmail.com") #Add your own email here

    #Check for ESS4
    ifelse(file.exists("./data/ESS4/ESS4e04_5.dta"),print("File Exists"), 
           download_rounds(
             rounds = 4,
             output_dir = "data",
             format = 'stata'))
    
    ESS<-read_dta("./data/ESS4/ESS4e04_5.dta")
    
    
    
    
    
####Data Cleaning####
    ####*Crime and Punitivness####
    #Higher values - more punitivness 
      ESS$punish<-(ESS$hrshsnt-5)*-1
      ESS$punish_fac<-as_factor(zap_missing(ESS$hrshsnt), ordered=T)
    
    ####*Welfare Attitudes#### 
      #Initally reverse coded, recode so higher values = higher welfare punitivness 
      #Many manage to obtain benefits/services not entitled to
      ESS$wel_ben<-(ESS$bennent-5)*-1
      
      # Most unemployed people do not really try to find a job
      ESS$wel_job<-(ESS$uentrjb-5)*-1
      
      #Indes of punitnvess -> Check for factor solution? Need to exclude NA for either cases
      ESS$wel_index<-ESS$wel_ben+ESS$wel_job
      
      #Welfare makes people lazy 
      ESS$wel_lazy<-(ESS$sblazy-5)*-1
      
    ####*Specific Welfare Policy preference####
      #Spending/taxation tradeoff
      ESS$welpol_general<-ESS$ditxssp
      ESS$welpol_general_1<-ESS$gincdif
      
      #Unemployed
      ESS$welpol_unemp<-ESS$gvslvue
      
      #Old people 
      ESS$welpol_old<-ESS$gvslvol
      
      #Sick people
      ESS$welpol_health<-ESS$gvhlthc
      
      #Childcare
      ESS$welpol_child<-ESS$gvcldcr
      
      #Migratnts -note different wording , put in more than they deserve, not sure it can be compared 
      ESS$welpol_migrants<-(ESS$imrccon-10)*-1
      
      head(ESS[,c("imrccon","welpol_migrants")])
      

    ####*sociodemographics####
      ESS$gender<-as_factor(zap_missing(ESS$gndr))
      ESS$age<-ESS$agea
      ESS$activity<-as_factor(zap_missing(ESS$mnactic))
      
      ESS$education<-as_factor(zap_missing(ESS$edulvla), ordered = F) 
      ESS$education <- fct_recode(ESS$education,
        NULL = "Not possible to harmonise into 5-level ISCED",
        NULL = "Other"
      )
      
      ESS$income<-as_factor(zap_missing(ESS$hinctnta), ordered = F)
      ESS$crime<-as_factor(zap_missing(ESS$crmvct))
      ESS$crime<-relevel(ESS$crime, ref="No")
      ESS$income_feeling<-as_factor(zap_missing(ESS$hincfel), ordered=F)
      ESS$cntry<-as_factor(ESS$cntry)
      
      ## Recoding ESS$income into ESS$income_rec
      ESS$income_simple <- fct_recode(ESS$income,
          "Poor" = "J - 1st decile",
          "Poor" = "R - 2nd decile",
          "Poor" = "C - 3rd decile",
          "Middle" = "M - 4th decile",
          "Middle" = "F - 5th decile",
          "Middle" = "S - 6th decile",
          "Middle" = "K - 7th decile",
          "Rich" = "P - 8th decile",
          "Rich" = "D - 9th decile",
          "Rich" = "H - 10th decile"
        )
      ESS$income_simple<-relevel(ESS$income_simple, ref="Middle")        
      
      
    ####*Other controls-Economic Vulnerability####
      #lkuemp: How likely unemployed and looking for work next 12 months - note lots of "non working"
      ESS$vul_emp<-as_factor(zap_missing(ESS$lkuemp))
      # How likely less time paid work than like because care for family next 12 months)
      ESS$vul_money<-as_factor(zap_missing(ESS$lknemny))
      #How likely less time paid work than like because care for family next 12 months)
      ESS$vul_paid<-as_factor(zap_missing(ESS$lklpwcf))
      ESS$vul_care<-as_factor(zap_missing(ESS$lklpwcf))
      
    ####*other controls: Generalized Trust####
      ESS$trust<- rowMeans(ESS[,c("ppltrst","pplfair","pplhlp")], na.rm=T)
                      
                      
 
      
    ####*political variables
      ESS$lr<-ESS$lrscale

      
#       
#       ESS$vul_emp<-ESS$lkuemp
# 
#       -	V
# -	Variable lknemny: How likely not enough money for household necessities next 12 months
# -	(Variable lklpwcf: How likely less time paid work than like because care for family next 12 months)
# -	Variable lknhlcn: How likely not receive health care needed if become ill next 12 months
# -	+ fear of crime, walk at night, crime victimization waere hier sicher angebracht 

    ####*crime experineces and victimization
    ESS$crime<-as_factor(zap_missing(ESS$crmvct))
    ESS$crime<-relevel(ESS$crime, ref="No")
    
    ESS$crime_fear<-as_factor(zap_missing(ESS$crvctwr))     
       
####Add national control variables####
    #GDP capita
    GDP<-read.csv("./../data/country_data/")
    
    
    #inequality/gini
    
    #unemployment benefit generosity 
    
    #crime rate/ homicide rates

####Welfare attitudes and punitvness link models####
   
    mod_1<-lm(welpol_general~punish+
                        gender+age+income_simple+education+activity+
                        lrscale+  #political variables
                        trust+    #social variables
                        trstprl+trstlgl+trstplc+
                        crime+crime_fear+
                        cntry, data=ESS)
    
    summary(mod_1)
       
       
    ####*Check mediation model? crime punitivness to welfare punitivness to welfare attitdes####
    library(mediation)
    
    
    mod_1_y<-lm(welpol_general~punish+wel_lazy+
                        gender+age+income_simple+education+activity+
                        lrscale+  #political variables
                        trust+    #social variables
                        trstprl+trstlgl+trstplc+
                        crime+crime_fear+
                        cntry, data=ESS)
    
    
    mod_1_m<-lm(wel_lazy~punish+
                        gender+age+income_simple+education+activity+
                        lrscale+  #political variables
                        trust+    #social variables
                        trstprl+trstlgl+trstplc+
                        crime+crime_fear+
                        cntry, data=ESS)
    summary(mod_1_m)
       
       
    mod_mediate<-mediate(model.m = mod_1_m, model.y = mod_1_y,
                         sims=1000, dropobs = T, treat="punish",mediator = "wel_lazy")
    summary(mod_mediate)
    
     
####Depreciated- stuff that didn't work#### 
      
####New Approach - compare the effect of punitivness on other welfare attitudes by order of deservingness####
    ESS_subset<-ESS %>%
         dplyr::select(starts_with("welpol"),punish,
                       age,income_simple,education,activity,gender,
                        lrscale,
                        # trust,
                        # trstprl,trstlgl,trstplc,
                        crime,
                        cntry)%>%
         na.omit()
         
       
       
       
    mod_1_welpol_general<-lm(welpol_general~punish+
                        gender+age+income_simple+education+activity+
                        lrscale+  #political variables
                        # trust+    #social variables
                        # trstprl+trstlgl+trstplc+
                        crime+
                        cntry, data=ESS_subset)
    
    
       
    mod_1_welpol_unemp<-lm(welpol_unemp~punish+
                        gender+age+income_simple+education+activity+
                        lrscale+  #political variables
                        # trust+    #social variables
                        # trstprl+trstlgl+trstplc+
                        crime+
                        cntry, data=ESS_subset)
    
    mod_1_welpol_old<-lm(welpol_old~punish+
                        gender+age+income_simple+education+activity+
                        lrscale+  #political variables
                        # trust+    #social variables
                        # trstprl+trstlgl+trstplc+
                        crime+
                        cntry, data=ESS_subset)
       
    mod_1_welpol_child<-lm(welpol_child~punish+
                        gender+age+income_simple+education+activity+
                        lrscale+  #political variables
                        # trust+    #social variables
                        # trstprl+trstlgl+trstplc+
                        crime+
                        cntry, data=ESS_subset)    
   
    mod_1_welpol_migrants<-lm(welpol_migrants~punish+
                        gender+age+income_simple+education+activity+
                        lrscale+  #political variables
                        # trust+    #social variables
                        # trstprl+trstlgl+trstplc+
                        crime+
                        cntry, data=ESS_subset)    
      
    mod_1_welpol_health<-lm(welpol_health~punish+
                        gender+age+income_simple+education+activity+
                        lrscale+  #political variables
                        # trust+    #social variables
                        # trstprl+trstlgl+trstplc+
                        crime+
                        cntry, data=ESS_subset)           
      #   
      # screenreg(list(standardize(mod_1_welpol_general),
      #                standardize(mod_1_welpol_migrants),
      #                standardize(mod_1_welpol_unemp),
      #                standardize(mod_1_welpol_child),
      #                standardize(mod_1_welpol_old)),
      #           custom.model.names = c("General","Migrants","Unemployed","Childcare","Old People"))
                
             #   
      htmlreg(list(mod_1_welpol_general,
                     mod_1_welpol_unemp,
                     mod_1_welpol_child,
                     mod_1_welpol_old,
                     mod_1_welpol_health),
                custom.model.names = c("General","Unemployed","Childcare","Old People", "Sick People"),
                omit.coef = "cntry",
              file="models.html")

                        
  
    
       
      summary(lm(welpol_general~welpol_unemp+welpol_child+welpol_old+welpol_health, data=ESS))
       
       
       
       
       
       
####Basic Correlations - Punitivness and Welfare Attitudes- Fixed Effect Models####
      mod_1<-lm(punish~wel_index+cntry, data=ESS)
      summary(mod_1)
      
      #Clear Correlation between welfare and crime punitinvess in simple correlational model - add sociodemographics and see if it drops out?
      mod_2<-lm(punish~wel_index+gender+age+income+education+activity+cntry, data=ESS)
      
      summary(mod_2)
      
      #Add some other correlates of interest - particularly crime victimization +
      mod_3<-lm(punish~gincdif
      summary(mod_3)
      
      
      ####*Addition, how do these things relate to welfare attitudes 
      mod_4<-lm(gincdif~sblazy+gender+age+income_simple+education+activity+
                  lrscale+  #political variables
                  crime+
                  cntry, data=ESS)
      
      
####Interaction Models <- how does the relationship change across different variables####
      mod_3_gender<-lm(punish~wel_index*gender+gender+age+income_simple+education+activity+
                  lrscale+  #political variables
                  trust+    #social variables
                  trstprl+trstlgl+trstplc+
                  crime+
                  cntry, data=ESS)

      
      mod_3_age<-lm(punish~wel_index*age+gender+age+income_simple+education+activity+
                  lrscale+  #political variables
                  trust+    #social variables
                  trstprl+trstlgl+trstplc+
                  crime+
                  cntry, data=ESS)

      summary(mod_3_age)
      
      mod_3_income_simple<-lm(punish~wel_index*income_simple+gender+age+income_simple+education+activity+
                  lrscale+  #political variables
                  trust+    #social variables
                  trstprl+trstlgl+trstplc+
                  crime+
                  cntry, data=ESS)

      
      summary(mod_3_income_simple)
      
      mod_3_education<-lm(punish~wel_index*education+gender+age+income_simple+education+activity+
                  lrscale+  #political variables
                  trust+    #social variables
                  trstprl+trstlgl+trstplc+
                  crime+
                  cntry, data=ESS)

      summary(mod_3_education)
      
      
      mod_3_activity<-lm(punish~wel_index*activity+gender+age+income_simple+education+activity+
                  lrscale+  #political variables
                  trust+    #social variables
                  trstprl+trstlgl+trstplc+
                  crime+
                  cntry, data=ESS)
      summary(mod_3_activity)
      
      
      mod_3_lrscale<-lm(punish~wel_index*lrscale+gender+age+income_simple+education+activity+
                  lrscale+  #political variables
                  trust+    #social variables
                  trstprl+trstlgl+trstplc+
                  crime+
                  cntry, data=ESS)
      summary(mod_3_lrscale)
      
          
      mod_3_trust<-lm(punish~wel_index*trust+gender+age+income_simple+education+activity+
                  lrscale+  #political variables
                  trust+    #social variables
                  trstprl+trstlgl+trstplc+
                  crime+
                  cntry, data=ESS)
      summary(mod_3_lrscale)
      
                
      mod_3_trstprl<-lm(punish~wel_index*trstprl+gender+age+income_simple+education+activity+
                  lrscale+  #political variables
                  trust+    #social variables
                  trstprl+trstlgl+trstplc+
                  crime+
                  cntry, data=ESS)
      summary(mod_3_trstprl)
      
      mod_3_trstplc<-lm(punish~wel_index*trstplc+gender+age+income_simple+education+activity+
                  lrscale+  #political variables
                  trust+    #social variables
                  trstprl+trstlgl+trstplc+
                  crime+
                  cntry, data=ESS)
      summary(mod_3_trstplc)

      
      mod_3_trstlgl<-lm(punish~wel_index*trstlgl+gender+age+income_simple+education+activity+
                  lrscale+  #political variables
                  trust+    #social variables
                  trstprl+trstlgl+trstplc+
                  crime+
                  cntry, data=ESS)
      summary(mod_3_trstlgl)

      mod_3_crime<-lm(punish~wel_index*crime+gender+age+income_simple+education+activity+
                  lrscale+  #political variables
                  trust+    #social variables
                  trstprl+trstlgl+trstplc+
                  crime+
                  cntry, data=ESS)
      summary(mod_3_crime)

      
      
            
      
####Representing Outcomes####
    ####*General setup variables####
    #Confidence intervals 
    z=1.68
    xmin=0.05
    xmax=0.2
      
      
    ####*Generate Figures for each Interaction####
    temp_gender<-marginextract(mod_3_gender, "wel_index","gender")%>%
          ggplot()+
                aes(x=temp, y=coef,ymin=coef-(z*se),ymax=coef+(z*se))+
                geom_errorbar(width=0, linetype="dashed")+
                geom_point(size=3 )+
                geom_hline(yintercept = 0.138,linetype="dashed")+
                coord_flip()+
                theme_bw()+
                xlab("Gender")+
                ylim(c(xmin,xmax))+
                ylab("")+ 
                scale_x_discrete(label=c("Female","Male"))+
                theme(axis.text.x = element_blank())

      
      temp_age<-marginextract(mod_3_age, "wel_index","age")%>%
              mutate(temp=as_factor(temp)) %>%
             ggplot()+
                aes(x=temp, y=coef,ymin=coef-(z*se),ymax=coef+(z*se))+
                geom_errorbar(width=0, linetype="dashed")+
                geom_point(size=3 )+
                geom_hline(yintercept = 0.138,linetype="dashed")+
                coord_flip()+
                theme_bw()+
                xlab("Age")+
                ylim(c(xmin,xmax))+
                ylab("")+
                scale_x_discrete(label=c("Age 33","Age 61"))+
                theme(axis.text.x = element_blank())

      temp_income_simple<-marginextract(mod_3_income_simple, "wel_index","income_simple") %>%
        mutate(temp =factor(temp,levels = c("wel_index:income_simplePoor", "wel_index:income_simpleMiddle", "wel_index:income_simpleRich")))%>%
          ggplot()+
                aes(x=temp, y=coef,ymin=coef-(z*se),ymax=coef+(z*se))+
                geom_errorbar(width=0, linetype="dashed")+
                geom_point(size=3 )+
                geom_hline(yintercept = 0.138,linetype="dashed")+
                coord_flip()+
                theme_bw()+
                xlab("Income")+
                ylim(c(xmin,xmax))+
                ylab("")+
                scale_x_discrete(label=c("Rich","Middle","Poor"))+
                theme(axis.text.x = element_blank())
        
        
      temp_education<-marginextract(mod_3_education, "wel_index","education")%>%
                mutate(temp =factor(temp,
                                    levels = c("wel_index:educationTertiary education completed (ISCED 5-6)", 
                                               "wel_index:educationPost-secondary non-tertiary education completed (ISCED 4)", 
                                               "wel_index:educationUpper secondary education completed (ISCED 3)",
                                               "wel_index:educationLower secondary education completed (ISCED 2)",
                                               "wel_index:educationLess than lower secondary education (ISCED 0-1)")))%>%
          ggplot()+
                aes(x=temp, y=coef,ymin=coef-(z*se),ymax=coef+(z*se))+
                geom_errorbar(width=0, linetype="dashed")+
                geom_point(size=3 )+
                geom_hline(yintercept = 0.138,linetype="dashed")+
                coord_flip()+
                xlab("Education-ISCED")+
                ylim(c(xmin,xmax))+
                ylab("") +
                scale_x_discrete(label=c("V/VI","IV","III","II","I"))+
                theme_bw()+
                theme(axis.text.x = element_blank())
      
      
      temp_activity<-marginextract(mod_3_activity, "wel_index","activity") %>%
          dplyr::filter(temp!=c("wel_index:activityCommunity or military service")) %>%
              ggplot()+
                aes(x=temp, y=coef,ymin=coef-(z*se),ymax=coef+(z*se))+
                geom_errorbar(width=0, linetype="dashed")+
                geom_point(size=3 )+
                geom_hline(yintercept = 0.138,linetype="dashed")+
                coord_flip()+
                xlab("Main Activity")+
                ylim(c(xmin,xmax))+
                scale_x_discrete(label=c("Education","Housework","Other", "Paid Work","Sick or Disabled","Retired","Unemployed","Inactive"))+
                theme_bw()+
                ylab("") 
      

      ####*Generate figures for attitudes etc####
          temp_lrscale<-marginextract(mod_3_lrscale, "wel_index","lrscale")%>%
              mutate(temp=as_factor(temp)) %>%
             ggplot()+
                aes(x=temp, y=coef,ymin=coef-(z*se),ymax=coef+(z*se))+
                geom_errorbar(width=0, linetype="dashed")+
                geom_point(size=3 )+
                geom_hline(yintercept = 0.138,linetype="dashed")+
                coord_flip()+
                theme_bw()+
                xlab("Left Right Selfplacement")+
                ylim(c(xmin,xmax))+
                ylab("")+
                scale_x_discrete(label=c("25% (4) ","75% (7)"))+
                theme(axis.text.x = element_blank())
      
        temp_trust<-marginextract(mod_3_trust, "wel_index","trust")%>%
              mutate(temp=as_factor(temp)) %>%
             ggplot()+
                aes(x=temp, y=coef,ymin=coef-(z*se),ymax=coef+(z*se))+
                geom_errorbar(width=0, linetype="dashed")+
                geom_point(size=3 )+
                geom_hline(yintercept = 0.138,linetype="dashed")+
                coord_flip()+
                theme_bw()+
                xlab("Generalized Trust")+
                ylim(c(xmin,xmax))+
                ylab("")+
                scale_x_discrete(label=c("25% (4) ","75% (7)"))+
                theme(axis.text.x = element_blank())
      
      
       temp_trstprl<-marginextract(mod_3_trstprl, "wel_index","trstprl")%>%
              mutate(temp=as_factor(temp)) %>%
             ggplot()+
                aes(x=temp, y=coef,ymin=coef-(z*se),ymax=coef+(z*se))+
                geom_errorbar(width=0, linetype="dashed")+
                geom_point(size=3 )+
                geom_hline(yintercept = 0.138,linetype="dashed")+
                coord_flip()+
                theme_bw()+
                xlab("Trust: Parliament")+
                ylim(c(xmin,xmax))+
                ylab("")+
                scale_x_discrete(label=c("25% (3) ","75% (6)"))+
                theme(axis.text.x = element_blank())
      
      
        temp_trstplc<-marginextract(mod_3_trstplc, "wel_index","trstplc")%>%
              mutate(temp=as_factor(temp)) %>%
              ggplot()+
                aes(x=temp, y=coef,ymin=coef-(z*se),ymax=coef+(z*se))+
                geom_errorbar(width=0, linetype="dashed")+
                geom_point(size=3 )+
                geom_hline(yintercept = 0.138,linetype="dashed")+
                coord_flip()+
                theme_bw()+
                xlab("Trust: Police")+
                ylim(c(xmin,xmax))+
                ylab("")+
                scale_x_discrete(label=c("25% (4) ","75% (8)"))
      
             
          temp_trstlgl<-marginextract(mod_3_trstlgl, "wel_index","trstlgl")%>%
              mutate(temp=as_factor(temp)) %>%
              ggplot()+
                aes(x=temp, y=coef,ymin=coef-(z*se),ymax=coef+(z*se))+
                geom_errorbar(width=0, linetype="dashed")+
                geom_point(size=3 )+
                geom_hline(yintercept = 0.138,linetype="dashed")+
                coord_flip()+
                theme_bw()+
                xlab("Trust: Legal System")+
                ylim(c(xmin,xmax))+
                ylab("")+
                scale_x_discrete(label=c("4","7"))+
                theme(axis.text.x = element_blank())

                   
          temp_crime<-marginextract(mod_3_crime, "wel_index","crime")%>%
              mutate(temp=as_factor(temp)) %>%
              ggplot()+
                aes(x=temp, y=coef,ymin=coef-(z*se),ymax=coef+(z*se))+
                geom_errorbar(width=0, linetype="dashed")+
                geom_point(size=3 )+
                geom_hline(yintercept = 0.138,linetype="dashed")+
                coord_flip()+
                theme_bw()+
                xlab("Crime Victim")+
                ylim(c(xmin,xmax))+
                ylab("")+
                scale_x_discrete(label=c("Yes","No"))+
                theme(axis.text.x = element_blank())
       
       

      ####Generate Full Output Figures####
      library(grid)
      library(gridExtra)
             
      #Get main effect
      temp_main<-tidy(mod_3)%>%
        filter(term=="wel_index")%>%
        rename(temp=term,coef=estimate,se=std.error)%>%
        dplyr::select(temp,coef,se) %>%
        ggplot()+
                aes(x=temp, y=coef,ymin=coef-(z*se),ymax=coef+(z*se))+
                geom_errorbar(width=0, linetype="dashed")+
                geom_point(size=3 )+
                geom_hline(yintercept = 0.138,linetype="dashed")+
                coord_flip()+
                xlab("Main Effect")+
                ylim(c(xmin,xmax))+
                theme_bw()+
                ylab("")+
                theme(axis.text.x = element_blank())

      ####*Sociodemographics####
      gA=ggplot_gtable(ggplot_build(temp_main))
      gB1=ggplot_gtable(ggplot_build(temp_gender))
      gB2=ggplot_gtable(ggplot_build(temp_age))
      gB3=ggplot_gtable(ggplot_build(temp_education))
      gB4=ggplot_gtable(ggplot_build(temp_income_simple))
      gB5=ggplot_gtable(ggplot_build(temp_activity))
      
      
      
      maxWidth = grid::unit.pmax(gA$widths, gB1$widths,gB2$widths,gB3$widths,
                                gB4$widths,gB5$widths) 

      gA$widths <- as.list(maxWidth)
      gB1$widths <- as.list(maxWidth)
      gB2$widths <- as.list(maxWidth)
      gB3$widths <- as.list(maxWidth)
      gB4$widths <- as.list(maxWidth)
      gB5$widths <- as.list(maxWidth)
      
      
      
            
      grid.newpage()
      figure.full<-grid.arrange(
        arrangeGrob(gA,
                    gB1,
                    gB2,
                    gB3,
                    gB4,
                    gB5,
                    nrow=6))
      
     
      figure.full
      #Save as PDF (a4 measures with some room for text)
      
      ggsave(filename="results1.pdf", 
       plot = figure.full, 
       device = cairo_pdf, 
       width = 8, 
       height = 11, 
       units = "in")  
      
      
      ####*Attitudes####
      gA=ggplot_gtable(ggplot_build(temp_main))
      gC1=ggplot_gtable(ggplot_build(temp_lrscale))
      gC2=ggplot_gtable(ggplot_build(temp_trust))
      gC3=ggplot_gtable(ggplot_build(temp_trstprl))
      gC4=ggplot_gtable(ggplot_build(temp_trstlgl))
      gC5=ggplot_gtable(ggplot_build(temp_trstplc))
      
      
      maxWidth = grid::unit.pmax(gA$widths, gC1$widths,gC2$widths,gC3$widths,
                                gC4$widths,gC5$widths) 

      gA$widths <- as.list(maxWidth)
      gC1$widths <- as.list(maxWidth)
      gC2$widths <- as.list(maxWidth)
      gC3$widths <- as.list(maxWidth)
      gC4$widths <- as.list(maxWidth)
      gC5$widths <- as.list(maxWidth)
      
      
      grid.newpage()
      figure.full<-grid.arrange(
        arrangeGrob(gA,
                    gC1,
                    gC2,
                    gC3,
                    gC4,
                    gC5,
                    nrow=6))
      
      
          # 
          # heights=c(.25,.2,.25,.25))
         
      ggsave(filename="results2.pdf", 
       plot = figure.full, 
       device = cairo_pdf, 
       width = 8, 
       height = 11, 
       units = "in")  
      
      
      temp_lrscale<-marginextract(mod_3_gender, "wel_index","lrscale")
      
      
      