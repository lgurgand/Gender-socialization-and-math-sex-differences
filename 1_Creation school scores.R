#setwd("~/Dropbox/ELFE/Database 2 (10 nov)")
#setwd("~/GitHub/Stage-M2/Elfe")
database <- read.csv("~/database", header=T, sep = ",")

library(dplyr)
library('semPlot')
library('lavaan')
library(tidyselect)
school <- database %>% select(c("id_Dem706_482_HP", starts_with(c("A04X", "A06X")) ))

num4 <- school[c(which(colnames(school) =="id_Dem706_482_HP"), 
                 which(colnames(school) == "A04X_BALLE4") :which(colnames(school) == "A04X_CANARD1")) ] #remove A04X_NOUNOURS and nounours 15
# + remove sapin 4 ??

lit4 <- school[c(which(colnames(school) =="id_Dem706_482_HP"),
                 which(colnames(school) == "A04X_LETTRG") :which(colnames(school) == "A04X_CHOU") )]
num6 <- school[c(which(colnames(school) =="id_Dem706_482_HP"),
                 which(colnames(school) == "A06X_CALCUL10") :which(colnames(school) == "A06X_COMPAR8")) ]
lit6 <- school[c(which(colnames(school) =="id_Dem706_482_HP"),
                 which(colnames(school) == "A06X_VIC") :which(colnames(school) == "A06X_CADEAU")) ]


# on recode chaque question en Bonne/mauvaise reponse (TRUE/1 or FALSE/0)

num4 <- num4 %>%  mutate(A04X_CANARD1=(A04X_CANARD1==1))
num4 <- num4 %>%  mutate(A04X_CANARD2=(A04X_CANARD2==1))
num4 <- num4 %>%  mutate(A04X_CANARD3=(A04X_CANARD3==1))
num4 <- num4 %>%  mutate(A04X_CANARD4=(A04X_CANARD4==1))
num4 <- num4 %>%  mutate(A04X_CANARD5=(A04X_CANARD5==1))

num4 <- num4 %>% select(-A04X_SAPIN6) # ON ENLEVE SAPIN6 QUI EST UN FILLER ET N AURAIT PAS DU ETRE INTEGRE DS LES DONNES(JPF)

#num4 <- num4 %>%  mutate(A04X_NOUNOURS15=(A04X_NOUNOURS15=="15"))
num6 <- num6 %>%  mutate(A06X_CALCUL10=(A06X_CALCUL10==10))
num6 <- num6 %>%  mutate(A06X_CALCUL3=(A06X_CALCUL3==3))
num6 <- num6 %>%  mutate(A06X_CALCUL4A=(A06X_CALCUL4A==4))
num6 <- num6 %>%  mutate(A06X_CALCUL1=(A06X_CALCUL1==1))
num6 <- num6 %>%  mutate(A06X_CALCUL4B=(A06X_CALCUL4B==4))
num6 <- num6 %>%  mutate(A06X_CALCUL18=(A06X_CALCUL18==18))
num6 <- num6 %>%  mutate(A06X_CALCUL6=(A06X_CALCUL6==6))
num6 <- num6 %>%  mutate(A06X_CALCUL52=(A06X_CALCUL52==52))
num6 <- num6 %>%  mutate(A06X_PROBLEM8=(A06X_PROBLEM8==8))
num6 <- num6 %>%  mutate(A06X_PROBLEM3A=(A06X_PROBLEM3A==3))
num6 <- num6 %>%  mutate(A06X_PROBLEM6=(A06X_PROBLEM6==6))
num6 <- num6 %>%  mutate(A06X_PROBLEM21=(A06X_PROBLEM21==21))
num6 <- num6 %>%  mutate(A06X_PROBLEM25=(A06X_PROBLEM25==25))
num6 <- num6 %>%  mutate(A06X_PROBLEM3B=(A06X_PROBLEM3B==3))
num6 <- num6 %>%  mutate(A06X_SUITE7=(A06X_SUITE7==1))
num6 <- num6 %>%  mutate(A06X_SUITE40=(A06X_SUITE40==1))
num6 <- num6 %>%  mutate(A06X_SUITE25=(A06X_SUITE25==1))
num6 <- num6 %>%  mutate(A06X_SUITE70=(A06X_SUITE70==1))
num6 <- num6 %>%  mutate(A06X_SUITE6=(A06X_SUITE6==1))
num6 <- num6 %>%  mutate(A06X_SUITE54=(A06X_SUITE54==1))
num6 <- num6 %>%  mutate(A06X_EURO35=(A06X_EURO35==35))
num6 <- num6 %>%  mutate(A06X_EURO36=(A06X_EURO36==36))
num6 <- num6 %>%  mutate(A06X_EURO62=(A06X_EURO62==62))
num6 <- num6 %>%  mutate(A06X_EURO47=(A06X_EURO47==47))





num6 <- num6 %>%  mutate(A06X_COMPAR1=(A06X_COMPAR1==1))
num6 <- num6 %>%  mutate(A06X_COMPAR2=(A06X_COMPAR2==1))
num6 <- num6 %>%  mutate(A06X_COMPAR3=(A06X_COMPAR3==1))
num6 <- num6 %>%  mutate(A06X_COMPAR4=(A06X_COMPAR4==1))
num6 <- num6 %>%  mutate(A06X_COMPAR5=(A06X_COMPAR5==1))
num6 <- num6 %>%  mutate(A06X_COMPAR6=(A06X_COMPAR6==1))
num6 <- num6 %>%  mutate(A06X_COMPAR7=(A06X_COMPAR7==1))
num6 <- num6 %>%  mutate(A06X_COMPAR8=(A06X_COMPAR8==1))

lit6 <- lit6 %>%  mutate(A06X_VIC=(A06X_VIC==1))
lit6 <- lit6 %>%  mutate(A06X_BIVU=(A06X_BIVU==1))
lit6 <- lit6 %>%  mutate(A06X_FRO=(A06X_FRO==1))
lit6 <- lit6 %>%  mutate(A06X_DAF=(A06X_DAF==1))
lit6 <- lit6 %>%  mutate(A06X_UTRO=(A06X_UTRO==1))
lit6 <- lit6 %>%  mutate(A06X_CLU=(A06X_CLU==1))
lit6 <- lit6 %>%  mutate(A06X_PLAR=(A06X_PLAR==1))
lit6 <- lit6 %>%  mutate(A06X_CHI=(A06X_CHI==1))
lit6 <- lit6 %>%  mutate(A06X_ACIR=(A06X_ACIR==1))
lit6 <- lit6 %>%  mutate(A06X_MOU=(A06X_MOU==1))
lit6 <- lit6 %>%  mutate(A06X_PEINTURE=(A06X_PEINTURE==1))
lit6 <- lit6 %>%  mutate(A06X_MANGER=(A06X_MANGER==1))
lit6 <- lit6 %>%  mutate(A06X_ZOUZOU=(A06X_ZOUZOU==1))
lit6 <- lit6 %>%  mutate(A06X_LAVER=(A06X_LAVER==1))
lit6 <- lit6 %>%  mutate(A06X_TERRIER=(A06X_TERRIER==1))
lit6 <- lit6 %>%  mutate(A06X_DESSINER=(A06X_DESSINER==1))
lit6 <- lit6 %>%  mutate(A06X_MALADE=(A06X_MALADE==1))
lit6 <- lit6 %>%  mutate(A06X_MAMAN=(A06X_MAMAN==1))
lit6 <- lit6 %>%  mutate(A06X_HERBE=(A06X_HERBE==1))
lit6 <- lit6 %>%  mutate(A06X_FRAICHE=(A06X_FRAICHE==1))
lit6 <- lit6 %>%  mutate(A06X_PAPILLON=(A06X_PAPILLON==1))
lit6 <- lit6 %>%  mutate(A06X_MAISON=(A06X_MAISON==1))
lit6 <- lit6 %>%  mutate(A06X_NOIR=(A06X_NOIR==1))
lit6 <- lit6 %>%  mutate(A06X_CHIEN=(A06X_CHIEN==1))
lit6 <- lit6 %>%  mutate(A06X_VOLER=(A06X_VOLER==1))
lit6 <- lit6 %>%  mutate(A06X_MURET=(A06X_MURET==1))
lit6 <- lit6 %>%  mutate(A06X_QUENTIN=(A06X_QUENTIN==1))
lit6 <- lit6 %>%  mutate(A06X_ENNEMI=(A06X_ENNEMI==1))
lit6 <- lit6 %>%  mutate(A06X_BALLON=(A06X_BALLON==1))
lit6 <- lit6 %>%  mutate(A06X_JARDIN=(A06X_JARDIN==1))
lit6 <- lit6 %>%  mutate(A06X_GARCON=(A06X_GARCON==1))

lit6 <- lit6 %>%  mutate(A06X_LIT=(A06X_LIT==3))
lit6 <- lit6 %>%  mutate(A06X_RAISIN=(A06X_RAISIN==4))
lit6 <- lit6 %>%  mutate(A06X_COCHON=(A06X_COCHON==4))
lit6 <- lit6 %>%  mutate(A06X_NID=(A06X_NID==2))
lit6 <- lit6 %>%  mutate(A06X_TABLE=(A06X_TABLE==5))
lit6 <- lit6 %>%  mutate(A06X_MOUTON=(A06X_MOUTON==2))
lit6 <- lit6 %>%  mutate(A06X_FROMAGE=(A06X_FROMAGE==2))
lit6 <- lit6 %>%  mutate(A06X_VOITURE=(A06X_VOITURE==3))
lit6 <- lit6 %>%  mutate(A06X_CIGOGNE=(A06X_CIGOGNE==1))
lit6 <- lit6 %>%  mutate(A06X_BOUQUET=(A06X_BOUQUET==2))
lit6 <- lit6 %>%  mutate(A06X_GUITARE=(A06X_GUITARE==1))
lit6 <- lit6 %>%  mutate(A06X_FLECHE=(A06X_FLECHE==3))
lit6 <- lit6 %>%  mutate(A06X_FONTAINE=(A06X_FONTAINE==2))
lit6 <- lit6 %>%  mutate(A06X_CADEAU=(A06X_CADEAU==4))

table(rowSums(is.na(num4)), useNA="always")
#number of data included that miss at least one value but still have at least 10 items
sum(rowSums(is.na(num4))<16 & rowSums(is.na(num4))>0)
#number of data that have at least 1 item but less than 10 = lost
sum(rowSums(is.na(num4))>17 & rowSums(!is.na(num4))>1)

table(rowSums(is.na(lit4)), useNA="always")
sum(rowSums(is.na(lit4))<25 & rowSums(is.na(lit4))>0)
sum(rowSums(is.na(lit4))>24 & rowSums(!is.na(lit4))>1)

table(rowSums(is.na(num6)), useNA="always")
sum(rowSums(is.na(num6))<22 & rowSums(is.na(num6))>0)
sum(rowSums(is.na(num6))>21 & rowSums(!is.na(num6))>1)

table(rowSums(is.na(lit6)), useNA="always")
sum(rowSums(is.na(lit6))<35 & rowSums(is.na(lit6))>0)
sum(rowSums(is.na(lit6))>34 & rowSums(!is.na(lit6))>1)

#correlation number na and score:

scorewithna <- rowMeans(num6[ , 2:ncol(num6)], na.rm = TRUE)
scorewithna <- as.data.frame(scorewithna)
scorewithna$id <- num6$id_Dem706_482_HP
colnames(scorewithna)<- c("score", "id")
scorewithna$score[which(is.nan(scorewithna$score))]<-NA
scorewithna$numberna <- rowSums(is.na(num6[ , 2:ncol(num6)]))

#correlation between number of NA and meanscore on values available:

cor.test(scorewithna$numberna, scorewithna$score, method="pearson")
# --> more missing values = lower score  --> in favor of hypothesis of missing values = wrong answers

# we recode the answers in numeracy in 1st grade: when at least one answer,
# every missing values are counted as a wrong response


for (i in 1:nrow(num6)){
  if (rowSums(!is.na(num6[i,]))>1){
    for (x in 2:ncol(num6)){
      if (is.na(num6[i,x])){
        num6[i,x]<-0
      }
    }
  }
}



#just to put every variable in the right format

for(i in 2:ncol(num4)){
  num4[,i]<- as.integer(as.logical(num4[,i]))
}


for(i in 2:ncol(lit4)){
  lit4[,i]<- as.integer(as.logical(lit4[,i]))
}
for(i in 2:ncol(num6)){
  num6[,i]<- as.integer(as.logical(num6[,i]))
}
for(i in 2:ncol(lit6)){    #bc idk how last are encoded
  lit6[,i]<- as.integer(as.logical(lit6[,i]))
}


#keep values only when at least 10 questions answered

num4nona <- num4[rowSums(is.na(num4[ , 2:ncol(num4)])) < 16, ]
num6nona <- num6[rowSums(is.na(num6[ , 2:ncol(num6)])) < 22, ]
lit4nona <- lit4[rowSums(is.na(lit4[ , 2:ncol(lit4)])) < 25, ]
lit6nona <- lit6[rowSums(is.na(lit6[ , 2:ncol(lit6)])) < 35, ]

# If you want to look at the correlation between each of the questions, run this:
#library(corrplot)
#corrplot(cor(num4nona[,2:ncol(num4nona)], use="pairwise.complete.obs"), method = 'number', number.cex = 0.35 , tl.cex = 0.4) 
#corrplot(cor(num6nona[,2:ncol(num6nona)], use="pairwise.complete.obs"), method = 'number', number.cex = 0.35 , tl.cex = 0.4) 
#corrplot(cor(lit4nona[,2:ncol(lit4nona)], use="pairwise.complete.obs"), method = 'number') 
#corrplot(cor(lit6nona[,2:ncol(lit6nona)], use="pairwise.complete.obs"), method = 'number') 

#################################################################################
#############SCORES PONDERES  ##############################################
# we calculate one score per category, then the final score is the average across  categories

############## Num 4 ##########################################################



scorenum4pond <- data.frame(matrix(ncol = 8, nrow = nrow(num4nona)))
colnames(scorenum4pond) <- c('id_Dem706_482_HP', 'tracer', 'resoudre', 'comparer', 'reco1', 'reco2', 'denombrer', "scorenum4")
scorenum4pond$id_Dem706_482_HP<-num4nona$id_Dem706_482_HP
scorenum4pond$tracer<-rowMeans(num4nona[ , 2:4], na.rm = TRUE)
scorenum4pond$resoudre<-rowMeans(num4nona[ , 5:10], na.rm = TRUE)
scorenum4pond$comparer<-rowMeans(num4nona[ , 11:14], na.rm = TRUE)
scorenum4pond$reco1<-rowMeans(num4nona[ , 15:17], na.rm = TRUE)  
scorenum4pond$reco2<-rowMeans(num4nona[ , 18:22], na.rm = TRUE)
scorenum4pond$denombrer<-rowMeans(num4nona[ , 23:27], na.rm = TRUE)
#final score:
scorenum4pond$scorenum4<-rowMeans(scorenum4pond[ , 2:ncol(scorenum4pond)], na.rm = TRUE)


############## Num 6 ##########################################################

scorenum6pond <- data.frame(matrix(ncol = 5, nrow = nrow(num6nona)))
colnames(scorenum6pond) <- c('id_Dem706_482_HP', 'calcul', 'suite', 'comparer', "scorenum6")
scorenum6pond$id_Dem706_482_HP<-num6nona$id_Dem706_482_HP
scorenum6pond$calcul<-rowMeans(num6nona[ , 2:19], na.rm = TRUE)
scorenum6pond$suite<-rowMeans(num6nona[ , 20:25], na.rm = TRUE)
scorenum6pond$comparer<-rowMeans(num6nona[ , 26:ncol(num6nona)], na.rm = TRUE)
#final score:
scorenum6pond$scorenum6<-rowMeans(scorenum6pond[ , 2:ncol(scorenum6pond)], na.rm = TRUE)


############## Lit 4 ##########################################################

scorelit4pond <- data.frame(matrix(ncol = 6, nrow = nrow(lit4nona)))
colnames(scorelit4pond) <- c('id_Dem706_482_HP', 'reco', 'son', 'supp', 'image', "scorelit4")
scorelit4pond$id_Dem706_482_HP<-lit4nona$id_Dem706_482_HP
scorelit4pond$reco<-rowMeans(lit4nona[ , 2:11], na.rm = TRUE)
scorelit4pond$son<-rowMeans(lit4nona[ , 12:20], na.rm = TRUE)
scorelit4pond$supp<-rowMeans(lit4nona[ , 21:26], na.rm = TRUE)
scorelit4pond$image<-rowMeans(lit4nona[ , 27:ncol(lit4nona)], na.rm = TRUE)
#final score:
scorelit4pond$scorelit4<-rowMeans(scorelit4pond[ , 2:ncol(scorelit4pond)], na.rm = TRUE)


############## Lit 6 ##########################################################

scorelit6pond <- data.frame(matrix(ncol = 5, nrow = nrow(lit6nona)))
colnames(scorelit6pond) <- c('id_Dem706_482_HP', 'phoneme', 'compr', 'lect', "scorelit6")
scorelit6pond$id_Dem706_482_HP<-lit6nona$id_Dem706_482_HP
scorelit6pond$phoneme<-rowMeans(lit6nona[ , 2:11], na.rm = TRUE)
scorelit6pond$compr<-rowMeans(lit6nona[ , 12:32], na.rm = TRUE)
scorelit6pond$lect<-rowMeans(lit6nona[ , 33:ncol(lit6nona)], na.rm = TRUE)
#final score:
scorelit6pond$scorelit6<-rowMeans(scorelit6pond[ , 2:ncol(scorelit6pond)], na.rm = TRUE)

plot(density(scale(scorelit4pond$score)), ylim=c(0,1), xlim=c(-4,4))
#lines(density(scale(allscoreslit6$lit6pond)))
#lines(density(allscoreslit6$lit6cfa))
#lines(density(res$coord[,"Dim.1"]))



############## Distribution  ###################################################
hist(scorenum4pond$scorenum4, breaks = seq(0, 1, 0.01))
hist(scorenum6pond$scorenum6, breaks = seq(0, 1, 0.01))
hist(scorelit4pond$scorelit4, breaks = seq(0, 1, 0.01))
hist(scorelit6pond$scorelit6, breaks = seq(0, 1, 0.01))



# scorelit4pond[,c(1,6)]
# scorelit6pond[,c(1,5)]
# scorenum4pond[,c(1,8)]
# scorenum6pond[,c(1,5)]


data2<- merge(scorelit4pond[,c(1,6)], scorelit6pond[,c(1,5)], by="id_Dem706_482_HP", all=TRUE)
data3<- merge(scorenum4pond[,c(1,8)], data2, by="id_Dem706_482_HP", all=TRUE)
data4<- merge(scorenum6pond[,c(1,5)], data3, by="id_Dem706_482_HP", all=TRUE)

# students with only 0 are considered missing value
data4$scorenum6[which(data4$scorenum6==0)] <- NA
data4$scorelit6[which(data4$scorelit6==0)] <- NA

hist(data4$scorenum6, breaks = seq(0, 1, 0.01))

################## Retirer scores des enfants pas dans la bonne classe (MS ou CP)

classe=cbind(id_Dem706_482_HP=database$id_Dem706_482_HP, classeMS=database$A04X_SCOL, classeCP=database$A06X_SCOL)
test <- merge(data4, classe, by = "id_Dem706_482_HP", all.x=TRUE)
table(test$classeMS, useNA="always")
table(test$classeCP, useNA="always")

nrow(na.omit(test["scorenum4"]))
nrow(na.omit(test["scorenum6"]))
nrow(na.omit(test["scorelit4"]))
nrow(na.omit(test["scorelit6"]))


test$scorenum4[which(test$classeMS !=2)]<-NA
test$scorelit4[which(test$classeMS !=2)]<-NA
test$scorenum6[which(test$classeCP !=2)]<-NA
test$scorelit6[which(test$classeCP !=2)]<-NA

nrow(na.omit(test["scorenum4"]))
nrow(na.omit(test["scorenum6"]))
nrow(na.omit(test["scorelit4"]))
nrow(na.omit(test["scorelit6"]))

#write.csv(test, file="scoreschool")



