# clean workspace
rm(list=ls())
# set decimals to digits instead of scientific
options(scipen=999)
# set work directory
setwd(dir = "~/Documents/R Directory/Non_Representative_Survey_Methods_Workshop/")
library(data.table)
# # # # # # # # 
# # # # # # # # 
# # # # # # # # 
# # # # # # # # 
# Load survey data 
survey = read.csv("sample_survey.csv")

names(survey) = c("p_T_nextGE",#"Certainty.to.vote.in.general.election.10.is.certain" 
                  "nextGE_V",#"GEVote"                                             
                  "T_pastGE",#"DidyouvoteGE2015"                                    
                  "V_pastGE",#"Past.vote"                                          
                  "brexit_V",#"Eurefvote"                                           
                  "p_T_brexit",#"Certainty.to.vote.in.EU.referendum.10.is.certain"   
                  "gender",#"Gender"                                              
                  "age",#"Age"                                                
                  "region",#"Region"                                              
                  "ethnicity",#"Ethnicity"                                          
                  "edu",#"Education"                                           
                  "tenure",#"Housing"                                            
                  "mosaic2014",#"mosaic_2014_groups"                                  
                  "postcode",#"postcode"                                           
                  "pcon",#"parliamentary_constituency"                          
                  "la"#"local_authority" 
                  )

# don't need mosaic, LA or post-code - we are estimating at the constituency level 
survey= survey[,-which(names(survey)=="mosaic2014")]
survey= survey[,-which(names(survey)=="la")]
survey= survey[,-which(names(survey)=="postcode")]

# clean variable levels 
survey$p_T_nextGE = as.numeric(as.character(unlist(survey$p_T_nextGE)))/10
levels(survey$nextGE_V) = c("conservative",#"Conservative"                         
                               NA,#"Dont.know"                            
                               'green',#"Green.Party"                         
                               "labour",#"Labour"                               
                               "libdem",#"Liberal.Democrat"                     
                               "novote",#"None.-.wont.vote"                    
                               "other",#"Other.party"                          
                               "pc",#"Plaid.Cymru.-.the.Welsh.Nationalists" 
                               "snp",#"Scottish.National.Party.SNP"         
                               "ukip",#"UKIP"                                 
                               "novote")#"Would.not.vote")
levels(survey$T_pastGE) = c(NA,0,0,NA,1)
survey$T_pastGE = as.numeric(as.characxter(unlist(survey$T_pastGE)))
levels(survey$V_pastGE) =c(NA,#0
                              NA,#blank
                              "conservative",#"Conservative"                         
                              NA,#"Dont.know"   
                              NA,# "Dont.remember"
                              'green',#"Green.Party"                         
                              "labour",#"Labour"                               
                              "libdem",#"Liberal.Democrat"                     
                              "other",#"Other.party"                          
                              "pc",#"Plaid.Cymru.-.the.Welsh.Nationalists" 
                              "snp",#"Scottish.National.Party.SNP"         
                              "ukip")#"UKIP")

levels(survey$brexit_V) = c(NA,#"Dont.know"      
                               "leave",#"Leave"          
                               "remain",#"remain"         
                               "novote")#"Would.not.vote")
survey$p_T_brexit = as.numeric(as.character(unlist(survey$p_T_brexit)))
levels(survey$gender) = c("2. Female",#"Female"             
                          "1. Male",#"Male"
                          NA)

survey$age = as.numeric(as.character(unlist(survey$age)))
survey$age = ifelse(survey$age>100,NA,survey$age)
survey$age = cut(survey$age,
                 breaks = c(-1,17,24,34,49,64,max(survey$age,na.rm=TRUE)),
                 labels = c("0. 0-17","1. 18-24","2. 25-34","3. 35-49","4. 50-64","5. 65+"))

levels(survey$region) = c(#NA,#"0"                        
                          "4. East Midlands",#"East.Midlands"            
                          "6. East of England",#"East.of.England"          
                          "7. London",#"London"                   
                          "1. North East",#"North.East"              
                          "2. North West",#"North.West"               
                          NA,#"Prefer.not.to.say"        
                          "11. Scotland",#"Scotland"                 
                          "8. South East",#"South.East"               
                          "9. South West",#"South.West"              
                          NA,#"Unknown"
                          "10. Wales",#"Wales"                    
                          "5. West Midlands",#"West.Midlands"            
                          "3. Yorkshire and the Humber"#"Yorkshire.and.The.Humber"
                          )


levels(survey$ethnicity) = c("3. Asian",#"Asian./.Asian.British:.Bangladeshi"          
                             #"3. Asian",#"Asian./.Asian.British:.Chinese"              
                             "3. Asian",#"Asian./.Asian.British:.Indian"              
                             "3. Asian",#"Asian./.Asian.British:.Other.Asian"          
                             "3. Asian",#"Asian./.Asian.British:.Pakistani"            
                             "3. Asian",#"Asian/Asian.British"                        
                             "2. Black",#"Black./.African./.Caribbean./.Black.British" 
                             "2. Black",#"Black/Black.British"                         
                             "1. White",#"Gypsy./.Traveller./.Irish.Traveller"        
                             "4. Other",#"Mixed"                                      
                             "4. Other",#"Mixed./.Multiple.Ethnic.Groups"              
                             "4. Other",#"Other.ethnic.group"                         
                             "4. Other",#"Other.Ethnic.Group"                          
                             NA,#"Prefer.not.to.say"                           
                             "1. White")#"White")

levels(survey$edu) = c(NA,#"0",                        
                      "6. Other",#"Apprenticeship",# not quite accurate but will do     
                      NA,#"Dont.know",                
                      "2. Level 1",#"Level.1",                  
                      "3. Level 2",#"Level.1.or.2",            
                      "3. Level 2",#"Level.2",                  
                      "4. Level 3",#"Level.3",                  
                      "5. Level 4",#"Level.4",                  
                      "1. No Qualifications",#"No.formal.qualifications", 
                      "1. No Qualifications",#"No.qualifications",       
                      "6. Other")#"Other")

levels(survey$tenure) = c(NA,#""                                                                             
                          "2. Owns with a mortgage or loan",#"Being.bought.on.a.mortgage"                                                   
                          "5. Lives here rent-free",#"Living.rent.free"                                                             
                          NA,#"Other/dont.know"                                                              
                          NA,#"Other/Dont.know"                                                              
                          "1. Owns outright",#"Owned.outright.by.household"                                                  
                          "2. Owns with a mortgage or loan",#"Owned.with.a.mortgage.or.loan"                                                
                          NA,#"Prefer.not.to.say"                                                            
                          "4. Rents (with or without housing benefit)",#"Privately.rented.from.someone.other.than.a.private.landlord.or.letting.agency"
                          "4. Rents (with or without housing benefit)",#"Rented.from.a.private.landlord.or.letting.agency"                             
                          "4. Rents (with or without housing benefit)",#"Rented.from.council/local.authority"                                          
                          "4. Rents (with or without housing benefit)",#"Rented.from.Housing.Association"                                              
                          "4. Rents (with or without housing benefit)",#"Rented.from.Local.Authority"                                                  
                          "4. Rents (with or without housing benefit)",#"Rented.from.private.landlord"                                                 
                          "3. Part-owns and part-rents (shared ownership)",#"Shared.ownership.part.owned.and.part.rented"                                  
                          "4. Rents (with or without housing benefit)"#"Social.housing.rented.other.than.from.council/local.authority"
                          )

# show random sample of 30 observations from survey
library(xtable)
print(xtable(survey[sample(1:dim(survey)[1],size = 30),]), include.rownames=FALSE)

# # # # # # # # 
# # # # # # # # 
# # # # # # # # 
# load Referendum results at the constiruency level 
# Referendum results at constituency level 
# https://medium.com/@chrishanretty/final-estimates-of-the-leave-vote-or-areal-interpolation-and-the-uks-referendum-on-eu-membership-5490b6cab878
C_res_EU = read.csv("Final estimates of the Leave vote share in the EU referendum.csv")
C_res_EU_turnout = read.csv("Hanretty Turnout estimates for Westminster constituencies in the EU referendum - turnout.csv")
C_res_EU = 
  merge(C_res_EU[,c("PCON11CD","Constituency.name","Figure.to.use")],
        C_res_EU_turnout,
        by.x = c("PCON11CD","Constituency.name"),
        by.y = c("PCON11CD","Seat"),
        all = TRUE)
# change spelling here - weird accent messed up the read
levels(C_res_EU$Constituency.name)[which(levels(C_res_EU$Constituency.name)=="Ynys\xcaMon")] = "Ynys Mon"

names(C_res_EU) = c("pcon", #"PCON11CD"          
                    "pcon_name", #"Constituency.name" 
                    "pct_brexit_V", #"Figure.to.use"     
                    "N_T_brexit", #"turnout"           
                    "N_pcon15", #"Electorate15"      
                    "region",#"Region"           
                    "pct_brexit_T")#"Turnout" )

# # # # # # # # 
# # # # # # # # 
# # # # # # # # 
# # # # # # # # 
# raw constituency level estimates v. results for turnout and vote choice
# create unified turnout/vote variable
survey$brexit_VT = ifelse(survey$p_T_brexit<10,"novote",as.character(unlist(survey$brexit_V)))
# the survey also estimates northern ireland, whilst we don't have pcon results for that; we can ignore.
survey = survey[-which(substr(survey$pcon,1,1)=="N"|substr(survey$pcon,1,1)=="M"),]
survey$pcon = as.factor(as.character(unlist(survey$pcon)))
# estimate area-level turnout
pct_brexit_T_pred_raw = 1-(table(survey$pcon,survey$brexit_VT)[,"novote"]/rowSums(table(survey$pcon,survey$brexit_VT)))
pct_brexit_T_pred_raw = pct_brexit_T_pred_raw[-which(is.na(pct_brexit_T_pred_raw ))]
# estimate area-level leave pct
pct_brexit_leave_pred_raw = table(survey$pcon,survey$brexit_VT)[,"leave"]/
  rowSums(table(survey$pcon,survey$brexit_VT)[,-which(colnames(table(survey$pcon,survey$brexit_VT))=="novote")])
pct_brexit_leave_pred_raw = pct_brexit_leave_pred_raw[-which(is.na(pct_brexit_leave_pred_raw ))]
# create mean absolute error function
mae = function(y,x){mean(abs(y-x),na.rm=TRUE)}

# some pcons are not present in the survey 
missing_in_survey = which(is.na(match(C_res_EU$pcon,names(pct_brexit_T_pred_raw))))
missing_in_survey_V = which(is.na(match(C_res_EU$pcon,names(pct_brexit_leave_pred_raw))))

# plot results for comparison with observed turnout and percentage leave
pdf(file = 'raw_sample_means_preformance.pdf',width = 10,height = 5)
par(mfrow = c(1,2))
# raw estimates of turnout results 
plot(
pct_brexit_T_pred_raw[match(names(pct_brexit_T_pred_raw),C_res_EU$pcon[-missing_in_survey])],
C_res_EU$pct_brexit_T[-missing_in_survey],
bty = "n",xlab = "predicted",ylab = 'observed',main = 'PC-level turnout\nraw sample means',
ylim = c(min(c(C_res_EU$pct_brexit_T,pct_brexit_T_pred_raw),na.rm=TRUE),1),
xlim = c(min(c(C_res_EU$pct_brexit_T,pct_brexit_T_pred_raw),na.rm=TRUE),1)
)
abline(0,1)
legend('topleft',legend = c(
  paste("MAE (%):",
        round(mae(x = 100*pct_brexit_T_pred_raw[match(names(pct_brexit_T_pred_raw),C_res_EU$pcon[-missing_in_survey])],
                  y = 100*C_res_EU$pct_brexit_T[-missing_in_survey]),2)),
  paste("corr:",
        round(cor(x = pct_brexit_T_pred_raw[match(names(pct_brexit_T_pred_raw),C_res_EU$pcon[-missing_in_survey])],
                  y = C_res_EU$pct_brexit_T[-missing_in_survey],use = 'complete.obs'),2))),
  bty = "n"
)
loess_curve_model = 
loess(formula = pct_brexit_T[-missing_in_survey] ~ 
        pct_brexit_T_pred_raw[match(names(pct_brexit_T_pred_raw),pcon[-missing_in_survey])],
      data = C_res_EU)
j <- order(pct_brexit_T_pred_raw[match(names(pct_brexit_T_pred_raw),C_res_EU$pcon[-missing_in_survey])])
lines(x = pct_brexit_T_pred_raw[match(names(pct_brexit_T_pred_raw),C_res_EU$pcon[-missing_in_survey])][j],
      y = loess_curve_model$fitted[j],
      col = 'darkgreen',lty = 1, lwd =2 )
# raw estimates of leave percentage
plot(
  pct_brexit_leave_pred_raw[match(names(pct_brexit_leave_pred_raw),C_res_EU$pcon[-missing_in_survey])],
  C_res_EU$pct_brexit_V[-missing_in_survey_V],
  bty = "n",xlab = "predicted",ylab = 'observed',main = 'PC-level leave percentage\nraw sample means',
  ylim = c(min(c(C_res_EU$pct_brexit_V,pct_brexit_leave_pred_raw),na.rm=TRUE),1),xlim = c(min(c(C_res_EU$pct_brexit_V,pct_brexit_leave_pred_raw),na.rm=TRUE),1)
)
abline(0,1)
legend('topleft',legend = c(
  paste("MAE (%):",
        round(mae(x = 100*pct_brexit_leave_pred_raw[match(names(pct_brexit_leave_pred_raw),C_res_EU$pcon[-missing_in_survey])],
                  y = 100*C_res_EU$pct_brexit_V[-missing_in_survey_V]),2)),
  paste("corr:",
        round(cor(x = pct_brexit_leave_pred_raw[match(names(pct_brexit_leave_pred_raw),C_res_EU$pcon[-missing_in_survey])],
                  y = C_res_EU$pct_brexit_V[-missing_in_survey_V],use = 'complete.obs'),2))),
  bty = "n"
)
loess_curve_model = 
  loess(formula = pct_brexit_V[-missing_in_survey_V] ~ 
          pct_brexit_leave_pred_raw[match(names(pct_brexit_leave_pred_raw),pcon[-missing_in_survey])],
        data = C_res_EU)
j <- order(pct_brexit_leave_pred_raw[match(names(pct_brexit_leave_pred_raw),C_res_EU$pcon[-missing_in_survey])])
lines(x = pct_brexit_leave_pred_raw[match(names(pct_brexit_leave_pred_raw),C_res_EU$pcon[-missing_in_survey])][j],
      y = loess_curve_model$fitted[j],
      col = 'darkgreen',lty = 1, lwd =2 )
dev.off()

# # # # # # # # 
# # # # # # # # 
# # # # # # # # 
# # # # # # # # 
# these are not very good - let's break the voting population into 'cells' (categories)

# examples pre-tabulated cross-tabs table 
PC_age_edu_crosstabs = read.csv("PC_Age_Edu.csv")
PC_age_edu_crosstabs = PC_age_edu_crosstabs[,-which(names(PC_age_edu_crosstabs)=="date")]
library(reshape2)
PC_age_edu_crosstabs = reshape2::melt(PC_age_edu_crosstabs,id.vars = c("geography","geography.code"))
PC_age_edu_crosstabs = PC_age_edu_crosstabs[-grep("All",PC_age_edu_crosstabs$variable),]
PC_age_edu_crosstabs$variable = gsub("\\.","",gsub("\\.qualifications\\.","",
                                     gsub("\\.to\\.","-",
                                     gsub("measures\\.\\.Value","",
                                          gsub("Highest\\.Level\\.of\\.Qualification","_",
                                               gsub("Age","",PC_age_edu_crosstabs$variable))))))
PC_age_edu_crosstabs$variable = as.character(unlist(PC_age_edu_crosstabs$variable))
PC_age_edu_crosstabs$age = unlist(lapply(PC_age_edu_crosstabs$variable,function(x){strsplit(x,split="\\_")[[1]][1]}))
PC_age_edu_crosstabs$edu = unlist(lapply(PC_age_edu_crosstabs$variable,function(x){strsplit(x,split="\\_")[[1]][2]}))
PC_age_edu_crosstabs = PC_age_edu_crosstabs[,-which(names(PC_age_edu_crosstabs)=="variable")]
PC_age_edu_crosstabs = PC_age_edu_crosstabs[rev(order(PC_age_edu_crosstabs$value)),]
names(PC_age_edu_crosstabs) = c("pcon_name","pcon","N","age","edu")

print(xtable(PC_age_edu_crosstabs[c(1:5,(dim(PC_age_edu_crosstabs)[1]-5):dim(PC_age_edu_crosstabs)[1]),]), include.rownames=FALSE)
# # # # # # # # 
# # # # # # # # 
# # # # # # # # 
# # # # # # # # 
# Load more complete stratification frame derived from individual level census sub-sample
# https://census.ukdataservice.ac.uk/use-data/guides/microdata.aspx

# # # # # # # # 
# # # # # # # # 
# # # # # # # # 
# # # # # # # # 
# Fixed Effects Model (england and wales only): 
# first, ensure startification frame and survey use the same categories; 

PC_age_edu_crosstabs$pcon = as.factor(PC_age_edu_crosstabs$pcon)

# first ensure age matches
PC_age_edu_crosstabs$age = as.factor(PC_age_edu_crosstabs$age)
survey$age = as.factor(as.character(unlist(survey$age)))
levels(survey$age) = c("16-24",#"1. 18-24" 
                       "25-34",#"2. 25-34" 
                       "35-49",#"3. 35-49" 
                       "50-64",#"4. 50-64" 
                       "65andover")#"5. 65+" 

# then set up education in the same way
PC_age_edu_crosstabs$edu = as.factor(PC_age_edu_crosstabs$edu)
survey$edu = as.factor(as.character(unlist(survey$edu)))
levels(PC_age_edu_crosstabs$edu) = c("6. Other",#"Apprenticeship" 
                                     "2. Level 1",#"Level1"         
                                     "3. Level 2",#"Level2"         
                                     "4. Level 3",#"Level3"         
                                     "5. Level 4",#"Level4andabove" 
                                     "1. No Qualifications",#"No"             
                                     "6. Other")#"Other"  
# we ate up a category - have to re-aggregate
PC_age_edu_crosstabs = aggregate(data.frame(N = PC_age_edu_crosstabs$N),
                                 by = list(pcon_name = PC_age_edu_crosstabs$pcon_name,
                                           pcon = as.factor(PC_age_edu_crosstabs$pcon),
                                           age = as.factor(PC_age_edu_crosstabs$age),
                                           edu = as.factor(PC_age_edu_crosstabs$edu)),
                                 FUN = sum)

# merge constituencies with lookup to get region 
lookup1 = 
read.csv("Ward_to_Westminster_Parliamentary_Constituency_to_Local_Authority_District_December_2016_Lookup_in_the_United_Kingdom.csv")
lookup2 = 
read.csv("Local_Authority_District_to_Region_December_2016_Lookup_in_England.csv")
lookup = merge(lookup1,lookup2,by=c("LAD16CD"),all=TRUE)
lookup$RGN16NM = as.character(unlist(lookup$RGN16NM))
lookup$RGN16NM[which(is.na(lookup$RGN16NM))] = "Wales"
lookup = lookup[complete.cases(lookup[,c("PCON16CD","RGN16NM")]),]

PC_age_edu_crosstabs = 
merge(PC_age_edu_crosstabs,unique(lookup[,c("PCON16CD","RGN16NM")]),by.x = "pcon",by.y = "PCON16CD")
PC_age_edu_crosstabs$RGN16NM= as.factor(PC_age_edu_crosstabs$RGN16NM)
# we only care about england and wales in this example
survey = survey[-which(is.na(match(survey$pcon,PC_age_edu_crosstabs$pcon))),]
survey$pcon = as.factor(as.character(unlist(survey$pcon)))

survey = merge(survey,unique(lookup[,c("PCON16CD","RGN16NM")]),by.x = "pcon",by.y = "PCON16CD")
survey$RGN16NM = as.factor(survey$RGN16NM)

# get single response question
survey$brexit_V = as.character(unlist(survey$brexit_V))
survey$brexit_V = as.factor(ifelse(survey$p_T_brexit<10,'novote',survey$brexit_V))
# for now, complete-cases from survey only
survey_complete = survey[complete.cases(survey[,c("brexit_V","age","edu","pcon")]),]

# get pcon_id to match strat.frame
survey_complete$pcon_id = 
match(survey_complete$pcon,
      levels(PC_age_edu_crosstabs$pcon)
      )
# get edu_id to match strat.frame
survey_complete$edu_id = 
  match(survey_complete$edu,
        levels(PC_age_edu_crosstabs$edu)
  )
# get age_id to match strat.frame
survey_complete$age_id = 
  match(survey_complete$age,
        levels(PC_age_edu_crosstabs$age)
  )

# get region_id to match strat.frame
survey_complete$region_id = 
  match(survey_complete$RGN16NM,
        levels(PC_age_edu_crosstabs$RGN16NM)
  )

# fit fixed effects model with JAGS 
data_list = list(age_id = survey_complete$age_id,
                 N.age = max(survey_complete$age_id),
                 edu_id = survey_complete$edu_id,
                 N.edu = max(survey_complete$edu_id),
                 region_id = survey_complete$region_id,
                 N.region = max(survey_complete$region_id),
                 pcon_id = survey_complete$pcon_id,
                 N.pcon = max(survey_complete$pcon_id),
                 brexit = model.matrix(~survey_complete$brexit_V-1),
                 N = dim(survey_complete)[1],
                 N.j = nlevels(survey_complete$brexit_V)
                 )


library(R2jags)
model_code_polls= '
model{

for(i in 1:N){ for(j in 1:N.j){

    pi[i,j] <- omega[i,j]/sum(omega[i,1:N.j])
brexit[i,j] ~ dpois(omega[i,j])
omega[i,j] <- exp(mu[i,j])
   mu[i,j] <- lambda[i] + 
              beta[j] + 
              alpha[age_id[i],j] +
              eta[edu_id[i],j] +
              rho[region_id[i],j] +
              gamma[pcon_id[i],j]
} }

beta[1] <- 0 
for(j in 2:N.j){
beta[j] ~ dnorm(0,0.1)
}

for(i in 1:N){
lambda[i] ~ dnorm(0,0.01)
}

for(a in 1:N.age ){
alpha[a,1] <- 0
for(j in 2:N.j){
alpha[a,j] ~ dnorm(0,0.1)
} }

for(e in 1:N.edu ){
eta[e,1] <- 0
for(j in 2:N.j){
eta[e,j]~ dnorm(0,0.1)
} }

for(r in 1:N.region ){
rho[r,1] <- 0
for(j in 2:N.j){
rho[r,j]~ dnorm(0,0.1)
} }

for(c in 1:N.pcon){
gamma[c,1] <- 0
for(j in 2:N.j){
gamma[c,j]~ dnorm(0,0.1)

} }

}'

#model_code_polls= '
#model{
#
#for(i in 1:N){ 
#brexit[i] ~ dcat(pi[i,1:N.j])
#for(j in 1:N.j){
#pi[i,j] <- omega[i,j]/sum(omega[i,1:N.j])
#omega[i,j] <- exp(mu[i,j])
#   mu[i,j] <- beta[j] + 
#              alpha[age_id[i],j] +
#              eta[edu_id[i],j] +
#              rho[region_id[i],j] +
#              gamma[pcon_id[i],j]
#} }
#
#beta[1] <- 0 
#for(j in 2:N.j){
#beta[j] ~ dnorm(0,0.1)
#}
#
#for(a in 1:N.age ){
#alpha[a,1] <- 0
#for(j in 2:N.j){
#alpha[a,j] ~ dnorm(0,0.1)
#} }
#
#for(e in 1:N.edu ){
#eta[e,1] <- 0
#for(j in 2:N.j){
#eta[e,j]~ dnorm(0,0.1)
#} }
#for(c in 1:N.pcon){
#

#for(r in 1:N.region ){
#  rho[r,1] <- 0
#  for(j in 2:N.j){
#    rho[r,j]~ dnorm(0,0.1)
#  } }

#gamma[c,1] <- 0
#for(j in 2:N.j){
#gamma[c,j]~ dnorm(0,0.1)
#} }
#
#}'
tmpf=tempfile()
tmps=file(tmpf,"w")
cat(model_code_polls,file=tmps)
close(tmps)


# Choose the parameters to watch
model_parameters =  c('pi',"beta","alpha","eta","gamma","rho","lambda")

# Run the model - can be slow
model_run = jags.parallel(data = data_list,
                 parameters.to.save = model_parameters,
                 model.file=tmpf,
                 n.cluster = 2,
                 n.chains = 2,
                 n.burnin =10000,
                 n.iter = 15000,
                 n.thin = 5) # Amount of thinning

# Check convergence 
plot(model_run$BUGSoutput$summary[,"Rhat"],
     xlab = "estimated parameters index",
     ylab = 'Rhat')
abline(h = 1.1,col = 'darkgreen')

# Predictions 
library(foreach)

# get mean estimates (no uncertainty)
raw_pred_mean = 
  foreach(j  = 1:data_list$N.j,.combine = 'cbind') %do%
  as.data.table(
        model_run$BUGSoutput$mean$beta[j] +
        model_run$BUGSoutput$mean$alpha[as.integer(PC_age_edu_crosstabs$age),j] +
        model_run$BUGSoutput$mean$eta[as.integer(PC_age_edu_crosstabs$edu),j] +
        model_run$BUGSoutput$mean$rho[as.integer(PC_age_edu_crosstabs$RGN16NM),j] +
        model_run$BUGSoutput$mean$gamma[as.integer(PC_age_edu_crosstabs$pcon),j]
    )
p_pred_mean = t(apply(raw_pred_mean,1,function(x){exp(x)/(sum(exp(x)))}))

# We will not use the uncertainty for now - let's look at predictive performance of point estimates 
# aggregate means over constituencies 
area_pred_counts = 
aggregate(p_pred_mean[,]* PC_age_edu_crosstabs$N,
          by = list(pcon_name = PC_age_edu_crosstabs$pcon_name,
                    pcon = PC_age_edu_crosstabs$pcon),
          FUN = function(x){sum(x,na.rm=TRUE)})

area_N = 
  aggregate(PC_age_edu_crosstabs$N,
            by = list(pcon_name = PC_age_edu_crosstabs$pcon_name,
                      pcon = PC_age_edu_crosstabs$pcon),
            FUN = function(x){sum(x,na.rm=TRUE)})

area_T_preds = 1-area_pred_counts[,grep("V",names(area_pred_counts))][,which(levels(survey_complete$brexit_V)=="novote")]/area_N$x
area_T_counts = area_T_preds*area_N$x

area_V_preds = area_pred_counts[,grep("V",names(area_pred_counts))][,which(levels(survey_complete$brexit_V)!="novote")]/area_T_counts 
area_V_preds_leave = area_V_preds[,which(levels(survey_complete$brexit_V)=="leave")]

# Plot results 
# plot results for comparison with observed turnout and percentage leave

# we don't have NI and Scotland in thise strat. frame
missing_in_strat_frame = which(is.na(match(C_res_EU$pcon,levels(PC_age_edu_crosstabs$pcon))))

pdf(file = 'fixed_effects_means_preformance.pdf',width = 10,height = 5)
par(mfrow = c(1,2))
# raw estimates of turnout results 
plot(
  area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
  C_res_EU$pct_brexit_T[-missing_in_strat_frame],
  bty = "n",xlab = "predicted",ylab = 'observed',main = 'PC-level turnout\nfixed effects',
  ylim = c(min(c(C_res_EU$pct_brexit_T,area_T_preds),na.rm=TRUE),1),
  xlim = c(min(c(C_res_EU$pct_brexit_T,area_T_preds),na.rm=TRUE),1)
)
abline(0,1)
legend('topleft',legend = c(
  paste("MAE (%):",
        round(mae(x = 100*area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
                  y = 100*C_res_EU$pct_brexit_T[-missing_in_strat_frame]),2)),
  paste("corr:",
        round(cor(x = area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
                  y = C_res_EU$pct_brexit_T[-missing_in_strat_frame],use = 'complete.obs'),2))),
  bty = "n"
)
loess_curve_model = 
  loess(formula = pct_brexit_T[-missing_in_strat_frame] ~ 
          area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),pcon[-missing_in_strat_frame])],
        data = C_res_EU)
j <- order(area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])])
lines(x = area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])][j],
      y = loess_curve_model$fitted[j],
      col = 'darkgreen',lty = 1, lwd =2 )
# raw estimates of leave percentage
plot(
  area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
  C_res_EU$pct_brexit_V[-missing_in_strat_frame],
  bty = "n",xlab = "predicted",ylab = 'observed',main = 'PC-level turnout\nfixed effects',
  ylim = c(min(c(C_res_EU$pct_brexit_V,area_V_preds_leave),na.rm=TRUE),1),
  xlim = c(min(c(C_res_EU$pct_brexit_V,area_V_preds_leave),na.rm=TRUE),1)
)
abline(0,1)
legend('topleft',legend = c(
  paste("MAE (%):",
        round(mae(x = 100*area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
                  y = 100*C_res_EU$pct_brexit_V[-missing_in_strat_frame]),2)),
  paste("corr:",
        round(cor(x = area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
                  y = C_res_EU$pct_brexit_V[-missing_in_strat_frame],use = 'complete.obs'),2))),
  bty = "n"
)
loess_curve_model = 
  loess(formula = pct_brexit_V[-missing_in_strat_frame] ~ 
          area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),pcon[-missing_in_strat_frame])],
        data = C_res_EU)
j <- order(area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])])
lines(x = area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])][j],
      y = loess_curve_model$fitted[j],
      col = 'darkgreen',lty = 1, lwd =2 )
dev.off()


# # # # # # # # 
# # # # # # # # 
# # # # # # # # 
# # # # # # # # 
# Multilevel  Model (england and wales only): 

model_code_polls= '
model{

for(i in 1:N){ for(j in 1:N.j){

    pi[i,j] <- omega[i,j]/sum(omega[i,1:N.j])
brexit[i,j] ~ dpois(omega[i,j])
omega[i,j] <- exp(mu[i,j])
   mu[i,j] <- lambda[i] + 
              beta_star[j] + 
              alpha_star[age_id[i],j] +
              eta_star[edu_id[i],j] +
              rho_star[region_id[i],j] +
              gamma_star[pcon_id[i],j]
} }

beta_star[1] <- 0 
for(j in 2:N.j){
beta_star[j] ~ dnorm(0,0.01) # no need for shrinkage here - less than 3 groops
}

for(i in 1:N){
lambda[i] ~ dnorm(0,0.01) # no shrinkage here - we need this for ensuring likelihood of poisson is same as multinomial
}

for(a in 1:N.age ){
alpha[a,1] <- 0
for(j in 2:N.j){
alpha[a,j] ~ dnorm(0,tau[1])
}
for(j in 1:N.j){
alpha_star[a,j] <- alpha[a,j]*aux[1]
} }

for(e in 1:N.edu ){
eta[e,1] <- 0
for(j in 2:N.j){
eta[e,j]~ dnorm(0,tau[2])
} 
for(j in 1:N.j){
eta_star[e,j] <- eta[e,j]*aux[2]
}
}

for(r in 1:N.region ){
rho[r,1] <- 0
for(j in 2:N.j){
rho[r,j]~ dnorm(0,tau[3])
} 
for(j in 1:N.j){
rho_star[r,j] <- rho[r,j]*aux[3]
}
}

for(c in 1:N.pcon){
gamma[c,1] <- 0
for(j in 2:N.j){
gamma[c,j] ~ dnorm(0,tau[4])
} 
for(j in 1:N.j){
gamma_star[c,j] <- gamma[c,j]*aux[4]
}
}

for(l in 1:4){
tau[l] <- pow(sigma[l],-2)
sigma[l] ~ dunif(0,10)
}

for(aux_id in 1:4){
aux[aux_id] ~ dnorm(0,0.01)
}

}'

tmpf=tempfile()
tmps=file(tmpf,"w")
cat(model_code_polls,file=tmps)
close(tmps)


# Choose the parameters to watch
model_parameters =  c('pi',"beta_star","alpha_star","eta_star","rho_star","gamma_star","lambda","sigma")

# Run the model - can be slow
model_run = jags.parallel(data = data_list,
                          parameters.to.save = model_parameters,
                          model.file=tmpf,
                          n.cluster = 2,
                          n.chains = 2,
                          n.burnin =10000,
                          n.iter = 15000,
                          n.thin = 5) # Amount of thinning


# Check convergence 
plot(model_run$BUGSoutput$summary[,"Rhat"],
     xlab = "estimated parameters index",
     ylab = 'Rhat')
abline(h = 1.1,col = 'darkgreen')

# Predictions 
library(foreach)
# get mean estimates (no uncertainty)
raw_pred_mean = 
  foreach(j  = 1:data_list$N.j,.combine = 'cbind') %do%
  as.data.table(
    model_run$BUGSoutput$mean$beta_star[j] +
      model_run$BUGSoutput$mean$alpha_star[as.integer(PC_age_edu_crosstabs$age),j] +
      model_run$BUGSoutput$mean$eta_star[as.integer(PC_age_edu_crosstabs$edu),j] +
      model_run$BUGSoutput$mean$rho_star[as.integer(PC_age_edu_crosstabs$RGN16NM),j] +
      model_run$BUGSoutput$mean$gamma_star[as.integer(PC_age_edu_crosstabs$pcon),j]
  )
p_pred_mean = t(apply(raw_pred_mean,1,function(x){exp(x)/(sum(exp(x)))}))

# We will not use the uncertainty for now - let's look at predictive performance of point estimates 
# aggregate means over constituencies 
area_pred_counts = 
  aggregate(p_pred_mean[,]* PC_age_edu_crosstabs$N,
            by = list(pcon_name = PC_age_edu_crosstabs$pcon_name,
                      pcon = PC_age_edu_crosstabs$pcon),
            FUN = function(x){sum(x,na.rm=TRUE)})

area_N = 
  aggregate(PC_age_edu_crosstabs$N,
            by = list(pcon_name = PC_age_edu_crosstabs$pcon_name,
                      pcon = PC_age_edu_crosstabs$pcon),
            FUN = function(x){sum(x,na.rm=TRUE)})

area_T_preds = 1-area_pred_counts[,grep("V",names(area_pred_counts))][,which(levels(survey_complete$brexit_V)=="novote")]/area_N$x
area_T_counts = area_T_preds*area_N$x

area_V_preds = area_pred_counts[,grep("V",names(area_pred_counts))][,which(levels(survey_complete$brexit_V)!="novote")]/area_T_counts 
area_V_preds_leave = area_V_preds[,which(levels(survey_complete$brexit_V)=="leave")]

# Plot results 
# plot results for comparison with observed turnout and percentage leave

# we don't have NI and Scotland in thise strat. frame
missing_in_strat_frame = which(is.na(match(C_res_EU$pcon,levels(PC_age_edu_crosstabs$pcon))))

pdf(file = 'multilevel_means_preformance.pdf',width = 10,height = 5)
par(mfrow = c(1,2))
# raw estimates of turnout results 
plot(
  area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
  C_res_EU$pct_brexit_T[-missing_in_strat_frame],
  bty = "n",xlab = "predicted",ylab = 'observed',main = 'PC-level turnout\nmultilevel model',
  ylim = c(min(c(C_res_EU$pct_brexit_T,area_T_preds),na.rm=TRUE),1),
  xlim = c(min(c(C_res_EU$pct_brexit_T,area_T_preds),na.rm=TRUE),1)
)
abline(0,1)
legend('topleft',legend = c(
  paste("MAE (%):",
        round(mae(x = 100*area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
                  y = 100*C_res_EU$pct_brexit_T[-missing_in_strat_frame]),2)),
  paste("corr:",
        round(cor(x = area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
                  y = C_res_EU$pct_brexit_T[-missing_in_strat_frame],use = 'complete.obs'),2))),
  bty = "n"
)
loess_curve_model = 
  loess(formula = pct_brexit_T[-missing_in_strat_frame] ~ 
          area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),pcon[-missing_in_strat_frame])],
        data = C_res_EU)
j <- order(area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])])
lines(x = area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])][j],
      y = loess_curve_model$fitted[j],
      col = 'darkgreen',lty = 1, lwd =2 )
# raw estimates of leave percentage
plot(
  area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
  C_res_EU$pct_brexit_V[-missing_in_strat_frame],
  bty = "n",xlab = "predicted",ylab = 'observed',main = 'PC-level turnout\nmultilevel model',
  ylim = c(min(c(C_res_EU$pct_brexit_V,area_V_preds_leave),na.rm=TRUE),1),
  xlim = c(min(c(C_res_EU$pct_brexit_V,area_V_preds_leave),na.rm=TRUE),1)
)
abline(0,1)
legend('topleft',legend = c(
  paste("MAE (%):",
        round(mae(x = 100*area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
                  y = 100*C_res_EU$pct_brexit_V[-missing_in_strat_frame]),2)),
  paste("corr:",
        round(cor(x = area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
                  y = C_res_EU$pct_brexit_V[-missing_in_strat_frame],use = 'complete.obs'),2))),
  bty = "n"
)
loess_curve_model = 
  loess(formula = pct_brexit_V[-missing_in_strat_frame] ~ 
          area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),pcon[-missing_in_strat_frame])],
        data = C_res_EU)
j <- order(area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])])
lines(x = area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])][j],
      y = loess_curve_model$fitted[j],
      col = 'darkgreen',lty = 1, lwd =2 )
dev.off()

# impact of variance parmeter on coefficient estimates 
pdf(file = 'shrinkage_variance_parameter-mulilevel.pdf',width = 10,height = 5)
par(mfrow=c(2,5))
for(j in 2:(data_list$N.j)){
for(a in 1:data_list$N.age){
plot(x = model_run$BUGSoutput$sims.list$sigma[,1],
     y = model_run$BUGSoutput$sims.list$alpha_star[,a,j],
     col = 'darkgrey',
     xlab = 'sigma_alpha',ylab="alpha", 
     main = paste(levels(survey_complete$brexit_V)[j],levels(PC_age_edu_crosstabs$age)[a],sep = ", "),
     ylim = c(min(as.matrix(model_run$BUGSoutput$sims.list$alpha_star)),max(as.matrix(model_run$BUGSoutput$sims.list$alpha_star))),
     xlim = c(0,max(as.matrix(model_run$BUGSoutput$sims.list$sigma[,1]))))
  loess_curve_model = 
    loess(formula = model_run$BUGSoutput$sims.list$alpha_star[,a,j] ~ 
            model_run$BUGSoutput$sims.list$sigma[,1])
  x <- order( model_run$BUGSoutput$sims.list$sigma[,1])
  lines(x =  model_run$BUGSoutput$sims.list$sigma[,1][x],
        y = loess_curve_model$fitted[x],
        col = 'darkgreen',lty = 1, lwd =2)
  abline(h = 0,lty = 2)
}}
dev.off()


# # # # # # # # 
# # # # # # # # 
# # # # # # # # 
# # # # # # # # 
# Multilevel  Model (england and wales only): Areal predictor included 

pcon_vars = read.csv("BES-2015-General-Election-results-file-v2.21.csv")

pcon_vars = pcon_vars[,c("ONSConstID",
                         "UKIP15",
                         "c11PopulationDensity",
                         "c11PassportEU",
                         "c11Christian",
                         "c11Muslim",
                         "c11IndustryAgriculture",
                         "c11Degree",
                         "c11Retired",
                         "c11EthnicityAsian",
                         "c11EthnicityBlack",
                         "c11Employed",
                         "c11NSSECLongtermUnemployed",
                         "c11Deprived4"
                         )]

pcon_vars = data.frame(ONSConstID = pcon_vars$ONSConstID,apply(pcon_vars[,-1],2,function(x){scale(ifelse(is.na(x),mean(x,na.rm = TRUE),x))})) # fill in mean for NA
pcon_vars_strat = pcon_vars[match(levels(PC_age_edu_crosstabs$pcon),pcon_vars$ONSConstID),]

pcon_vars_survey = pcon_vars[match(survey_complete$pcon,pcon_vars$ONSConstID),]

data_list$X = pcon_vars_survey[,-1]
data_list$N.x = dim(pcon_vars_survey)[2]-1
data_list$V = diag(dim(pcon_vars_survey)[2]-1)/100
data_list$M = rep(0,dim(pcon_vars_survey)[2]-1)

model_code_polls= '
model{

for(i in 1:N){ for(j in 1:N.j){

    pi[i,j] <- omega[i,j]/sum(omega[i,1:N.j])
brexit[i,j] ~ dpois(omega[i,j])
omega[i,j] <- exp(mu[i,j])
   mu[i,j] <- lambda[i] + 
              beta_star[j] + 
              alpha_star[age_id[i],j] +
              eta_star[edu_id[i],j] +
              rho_star[region_id[i],j] +
              gamma_star[pcon_id[i],j] + 
              inprod(phi[1:N.x,j],X[i,1:N.x])
} }

for(x in 1:N.x){
phi[x,1]<-0
}
for(j in 2:N.j){
phi[1:N.x,j] ~ dmnorm(M[1:N.x],V[1:N.x,1:N.x])
}

beta_star[1] <- 0 
for(j in 2:N.j){
beta_star[j] ~ dnorm(0,0.01)
}


for(i in 1:N){
lambda[i] ~ dnorm(0,0.01)
}


for(a in 1:N.age ){
alpha[a,1] <- 0
for(j in 2:N.j){
alpha[a,j] ~ dnorm(0,tau[1])
}
for(j in 1:N.j){
alpha_star[a,j] <- alpha[a,j]*aux[1]
} }

for(e in 1:N.edu ){
eta[e,1] <- 0
for(j in 2:N.j){
eta[e,j]~ dnorm(0,tau[2])
} 
for(j in 1:N.j){
eta_star[e,j] <- eta[e,j]*aux[2]
}
}

for(r in 1:N.region ){
rho[r,1] <- 0
for(j in 2:N.j){
rho[r,j]~ dnorm(0,tau[3])
} 
for(j in 1:N.j){
rho_star[r,j] <- rho[r,j]*aux[3]
}
}

for(c in 1:N.pcon){
gamma[c,1] <- 0
for(j in 2:N.j){
gamma[c,j] ~ dnorm(0,tau[4])
} 
for(j in 1:N.j){
gamma_star[c,j] <- gamma[c,j]*aux[4]
}
}

for(l in 1:4){
tau[l] <- pow(sigma[l],-2)
sigma[l] ~ dunif(0,10)
}

for(aux_id in 1:4){
aux[aux_id] ~ dnorm(0,0.01)
}

}'

tmpf=tempfile()
tmps=file(tmpf,"w")
cat(model_code_polls,file=tmps)
close(tmps)


# Choose the parameters to watch
model_parameters =  c('pi',"beta_star","alpha_star","eta_star","rho_star","gamma_star","lambda","phi","sigma")

# Run the model - can be slow
model_run = jags.parallel(data = data_list,
                          parameters.to.save = model_parameters,
                          model.file=tmpf,
                          n.cluster = 2,
                          n.chains = 2,
                          n.burnin =10000,
                          n.iter = 15000,
                          n.thin = 5) # Amount of thinning

# Check convergence 
plot(model_run$BUGSoutput$summary[,"Rhat"],
     xlab = "estimated parameters index",
     ylab = 'Rhat')
abline(h = 1.1,col = 'darkgreen')

# Predictions 
library(foreach)
# get mean estimates (no uncertainty)
raw_pred_mean = 
  foreach(j  = 1:data_list$N.j,.combine = 'cbind') %do%
  as.data.table(
    model_run$BUGSoutput$mean$beta_star[j] +
      model_run$BUGSoutput$mean$alpha_star[as.integer(PC_age_edu_crosstabs$age),j] +
      model_run$BUGSoutput$mean$eta_star[as.integer(PC_age_edu_crosstabs$edu),j] +
      model_run$BUGSoutput$mean$rho_star[as.integer(PC_age_edu_crosstabs$RGN16NM),j] + 
      model_run$BUGSoutput$mean$gamma_star[as.integer(PC_age_edu_crosstabs$pcon),j] + 
      as.matrix(pcon_vars_strat[as.integer(PC_age_edu_crosstabs$pcon),-1]) %*% 
      as.numeric(model_run$BUGSoutput$mean$phi[,j] )
  ) 

p_pred_mean = t(apply(raw_pred_mean,1,function(x){exp(x)/(sum(exp(x)))}))

# get simulations of results (uncertainty)
#raw_pred_sims = 
#foreach(j  = 1:data_list$N.j,.combine = 'cbind') %do%
#as.data.table(
#sapply(X = 1:model_run$BUGSoutput$n.sims,FUN = function(x){
#    model_run$BUGSoutput$sims.list$beta[x,j] +
#    model_run$BUGSoutput$sims.list$alpha[x,as.integer(PC_age_edu_crosstabs$age),j] +
#    model_run$BUGSoutput$sims.list$eta[x,as.integer(PC_age_edu_crosstabs$edu),j] +
#    model_run$BUGSoutput$sims.list$gamma[x,as.integer(PC_age_edu_crosstabs$pcon),j]
#  }))
#p_pred_sims = foreach(s = 1:model_run$BUGSoutput$n.sims) %do%
#  as.data.table(
#  t(apply(raw_pred_sims[,which(!is.na(match(names(raw_pred_sims),paste("V",s,sep="")))),with = FALSE],
#        1,function(x){exp(x)/(sum(exp(x)))})))

# We will not use the uncertainty for now - let's look at predictive performance of point estimates 
# aggregate means over constituencies 
area_pred_counts = 
  aggregate(p_pred_mean[,]* PC_age_edu_crosstabs$N,
            by = list(pcon_name = PC_age_edu_crosstabs$pcon_name,
                      pcon = PC_age_edu_crosstabs$pcon),
            FUN = function(x){sum(x,na.rm=TRUE)})

area_N = 
  aggregate(PC_age_edu_crosstabs$N,
            by = list(pcon_name = PC_age_edu_crosstabs$pcon_name,
                      pcon = PC_age_edu_crosstabs$pcon),
            FUN = function(x){sum(x,na.rm=TRUE)})

area_T_preds = 1-area_pred_counts[,grep("V",names(area_pred_counts))][,which(levels(survey_complete$brexit_V)=="novote")]/area_N$x
area_T_counts = area_T_preds*area_N$x

area_V_preds = area_pred_counts[,grep("V",names(area_pred_counts))][,which(levels(survey_complete$brexit_V)!="novote")]/area_T_counts 
area_V_preds_leave = area_V_preds[,which(levels(survey_complete$brexit_V)=="leave")]

# Plot results 
# plot results for comparison with observed turnout and percentage leave

# we don't have NI and Scotland in thise strat. frame
missing_in_strat_frame = which(is.na(match(C_res_EU$pcon,levels(PC_age_edu_crosstabs$pcon))))

pdf(file = 'multilevel_and_area_pred_preformance.pdf',width = 10,height = 5)
par(mfrow = c(1,2))
# raw estimates of turnout results 
plot(
  area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
  C_res_EU$pct_brexit_T[-missing_in_strat_frame],
  bty = "n",xlab = "predicted",ylab = 'observed',main = 'PC-level turnout\nmultilevel model + area predictor',
  ylim = c(min(c(C_res_EU$pct_brexit_T,area_T_preds),na.rm=TRUE),1),
  xlim = c(min(c(C_res_EU$pct_brexit_T,area_T_preds),na.rm=TRUE),1)
)
abline(0,1)
legend('topleft',legend = c(
  paste("MAE (%):",
        round(mae(x = 100*area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
                  y = 100*C_res_EU$pct_brexit_T[-missing_in_strat_frame]),2)),
  paste("corr:",
        round(cor(x = area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
                  y = C_res_EU$pct_brexit_T[-missing_in_strat_frame],use = 'complete.obs'),2))),
  bty = "n"
)
loess_curve_model = 
  loess(formula = pct_brexit_T[-missing_in_strat_frame] ~ 
          area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),pcon[-missing_in_strat_frame])],
        data = C_res_EU)
j <- order(area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])])
lines(x = area_T_preds[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])][j],
      y = loess_curve_model$fitted[j],
      col = 'darkgreen',lty = 1, lwd =2 )
# raw estimates of leave percentage
plot(
  area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
  C_res_EU$pct_brexit_V[-missing_in_strat_frame],
  bty = "n",xlab = "predicted",ylab = 'observed',main = 'PC-level turnout\nmultilevel model + area predictor',
  ylim = c(min(c(C_res_EU$pct_brexit_V,area_V_preds_leave),na.rm=TRUE),1),
  xlim = c(min(c(C_res_EU$pct_brexit_V,area_V_preds_leave),na.rm=TRUE),1)
)
abline(0,1)
legend('topleft',legend = c(
  paste("MAE (%):",
        round(mae(x = 100*area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
                  y = 100*C_res_EU$pct_brexit_V[-missing_in_strat_frame]),2)),
  paste("corr:",
        round(cor(x = area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])],
                  y = C_res_EU$pct_brexit_V[-missing_in_strat_frame],use = 'complete.obs'),2))),
  bty = "n"
)
loess_curve_model = 
  loess(formula = pct_brexit_V[-missing_in_strat_frame] ~ 
          area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),pcon[-missing_in_strat_frame])],
        data = C_res_EU)
j <- order(area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])])
lines(x = area_V_preds_leave[match(levels(PC_age_edu_crosstabs$pcon),C_res_EU$pcon[-missing_in_strat_frame])][j],
      y = loess_curve_model$fitted[j],
      col = 'darkgreen',lty = 1, lwd =2 )
dev.off()

