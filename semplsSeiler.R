library(semPLS)

CBdata <- read.csv("/home/jannic/Schreibtisch/PB/Data/2014_08_30-CB_Alle_R.csv")

#create measuring model
latentvar <- c("Anlagevorschlag","Anlagevorschlag","Anlagevorschlag" 
               ,"Produktangebot","Produktangebot"
               ,"Raeumlichkeiten","Raeumlichkeiten","Raeumlichkeiten" 
               ,"Erreichbarkeit","Erreichbarkeit" 
               ,"PreisLeistung","PreisLeistung"
               ,"Kundenzufriedenheit","Kundenzufriedenheit","Kundenzufriedenheit" 
               ,"Kundenloyalitaet","Kundenloyalitaet","Kundenloyalitaet" 
               ,"Reporting","Reporting"
               ,"Performance","Performance"  
               ,"Berater","Berater","Berater","Berater","Berater","Berater") 


indicators <- c("SQ069","SQ075","SQ074"
               ,"SQ008","SQ078"
               ,"SQ020","SQ021","SQ022"
               ,"SQ015","SQ016"
               ,"SQ062","SQ076"
               ,"SQ061","SQ044","SQ110"
               ,"SQ001","SQ104","SQ105"
               ,"SQ082","SQ081"
               ,"SQ079","SQ080"
               ,"SQ004","SQ005","SQ011","SQ059","SQ067","SQ046")   

seiler_outer_model <- matrix(c(latentvar,indicators),length(latentvar))
colnames(seiler_outer_model) <- c("source","target")

#create structural model
exogenous <- c("Anlagevorschlag","Anlagevorschlag"
       ,"Produktangebot","Produktangebot"
       ,"Raeumlichkeiten"
       ,"Erreichbarkeit"
       ,"PreisLeistung"
       ,"Kundenzufriedenheit"
       ,"Reporting"
       ,"Performance","Performance","Performance"
       ,"Berater","Berater")  

endogenous <- c("PreisLeistung","Kundenzufriedenheit"
       ,"PreisLeistung","Kundenzufriedenheit"
       ,"Kundenzufriedenheit"
       ,"Kundenzufriedenheit"
       ,"Kundenzufriedenheit"
       ,"Kundenloyalitaet"
       ,"Kundenzufriedenheit"
       ,"PreisLeistung","Kundenloyalitaet","Kundenzufriedenheit"
       ,"Kundenzufriedenheit","Kundenloyalitaet")   

seiler_inner_model <- matrix(c(exogenous,endogenous),length(exogenous))
colnames(seiler_inner_model) <- c("source","target")

#create plsm object
model <- plsm(data = CBdata, strucmod = seiler_inner_model, measuremod = seiler_outer_model)

#calculate model
seiler_model <- sempls(model = model,data=CBdata,wscheme='centroid')

#show model
seiler_model

#load the Rgraphviz Package
library(Rgraphviz)

#visualize model with dot 
#graphviz required
pathDiagram(new_seiler_model, file = "seiler-structure-new", edge.labels = "both",
            output.type = "graphics", digits = 3, rSquared = rSquared(new_seiler_model))

#plot diagram
plot(agread("/home/jannic/Dokumente/R/pbsem/seiler-structure.dot", layoutType="dot"), main="Path model")



#pfadkoeffizienten nicht signifikant, anlagevorschlag zu viele missing values
model <- removeLVs(model,c("Raeumlichkeiten","Erreichbarkeit" ,"Anlagevorschlag","Reporting"))

#mein berater erklärt mir dinge so dass ich sie nachvollziehen kann SQ004
model <- removeMVs(model,"SQ004")

#nicht signifikante pfade löschen
new_model <- removePath(model, from="Produktangebot",to="Kundenzufriedenheit")


new_seiler_model <- sempls(new_model = model,data=CBdata,wscheme='centroid')
new_seiler_model
#obs 1 has 1 NA at SQ110 but is completely not taken into account



#obs 261 hat fast nur missing values also löschen
new_indicatorset <- CBdata[-261,new_model$manifest]



library(psych)
library(GPArotation)

#target indicators used in the model
indicatorset <- CBdata[c("SQ069","SQ075","SQ074"
                         ,"SQ008","SQ078"
                         ,"SQ020","SQ021","SQ022"
                         ,"SQ015","SQ016"
                         ,"SQ062","SQ076"
                         ,"SQ061","SQ044","SQ110"
                         ,"SQ001","SQ104","SQ105"
                         ,"SQ082","SQ081"
                         ,"SQ079","SQ080"
                         ,"SQ004","SQ005","SQ011","SQ059","SQ067","SQ046")   ]

#do parallel analysis
fa.parallel(new_indicatorset,fa="fa")

#do factor analysis
PB_fac <- fa(new_indicatorset, nfactors=4, rotate="varimax")

#show results
print(PB_fac$loadings, cut=0.4)

#show initial Model
seiler_model

#show diagram
fa.diagram(PB_fac, main ="Factor Analysis")

















