library(car)
library (explor)  
library(FactoMineR)
library(factoextra)
library(corrplot)
#install.packages("ggcorrplot")
library(ggcorrplot)

# Trafic de voyageurs et de marchandises depuis 1841
PROJECT <- read.csv2("C:/Users/sylva/Downloads/trafic-de-voyageurs-et-marchandises-depuis-1841.csv")

# Ici R avait reconnu les variables de notre base de donnée en tant que variables qualitatives,
# Ils fallaient donc modifier ses variables en variables quantitatives pour ainsi réaliser une Analyse des Composantes Primaires
PROJECT$Voyageurs<-as.numeric(PROJECT$Voyageurs)
PROJECT$Voyageurs.km<-as.numeric(PROJECT$Voyageurs.km)
PROJECT$Tonnes<-as.numeric(PROJECT$Tonnes)
PROJECT$Tonnes.km<-as.numeric(PROJECT$Tonnes.km)

dim(PROJECT)

str(PROJECT)
# on a 179 observations et on obtient 5 variables (avec les variables numériques qui sont nos 4 variables quantitatives)

head(PROJECT)
# Les données pour le test de corrélation

# REALISATION DE LA MATRICE DE CORRELATION avec seulement les variables quantitatives :
corr <-cor(PROJECT[,2:5]) 
cor(PROJECT[,2:5]) 
# ou bien cor(PROJECT[,-c(1,6)])

round(cor(PROJECT[,-c(1,6)]),2)# arrondi à 2 chiffres après la virgule
#--------------------------------------------------------------------------
cor_plot <- ggcorrplot(corr,
                       hc.order = TRUE,
                       type = "lower",
                       lab = TRUE,
                       lab_size = 4,
                       colors = c("red","white","green"),
                       title = "Matrice de corrélation",
                       ggtheme = theme_bw)
cor_plot
#--------------------------------------------------------------------------                      

# REALISATION DE L'ACP
res.ACPROJECT<- PCA(PROJECT,scale.unit=TRUE,quali.sup=1)

#La somme des valeurs propres
res.ACPROJECT$eig
sum(res.ACPROJECT$eig[,1])


#Coordonnées, COS² et contribution des individus actifs
ACPROJECTind<- res.ACPROJECT$ind

#ACPROJECTind$coord -> coordonnées
#ACPROJECTind$contrib -> Contributions des axes
#ACPROJECTind$cos2 -> Cos2, qualité de représentation


# Coordonnées, COS² et contribution des variables actives
ACPROJECTvar<-res.ACPROJECT$var

#ACPROJECTvar$coord -> coordonnées
#ACPROJECTvar$contrib -> Contributions des axes
#ACPROJECTvar$cos2 -> Cos2, qualité de représentation


#Coordonnées, COS² des variables supplémentaires
ACPROJECTsup<-res.ACPROJECT$quali.sup
ACPROJECTsup

# Résumé des résultats de l'ACP
summary(res.ACPROJECT)

## Exploration visuelle de l'ACP
explor(res.ACPROJECT)

#-------------------------------------------------------------------------




