#----------------------------
# Autor: DOUGLAS VARGAS
#----------------------------
# Dados obtidos em: 
# https://www.kaggle.com/dansbecker/melbourne-housing-snapshot
# Fonte original dos dados:
# https://www.kaggle.com/anthonypino/melbourne-housing-market
#----------------------------

#Importanto bibliotecas
library('ggplot2')
library('Boruta')
library('caTools')
library('statmod')
library('hnp')
library('fmsb')

#Carregando dados:
setwd('/home/douglas/Documentos/oper/')
df = read.csv('melb_data.csv')

head(df)

#Contando NA para cada coluna:
sapply(df, function(i) sum(length(which(is.na(i)))))

### remove  colunas com muitos NA
drop_col = c('YearBuilt', 'BuildingArea', 'Postcode', 'Bedroom2')
df_clean = df[ , !(names(df) %in% drop_col)]

# Removendo os valores NA dos carros
df_clean = df_clean[complete.cases(df_clean), ]
dim(df_clean)

#Analisando variaveis categóricas
####
summary(df$Type)
plot(df$Type)

#Selecionando as variaveis numéricas
df_num = Filter(is.numeric, df_clean)

#Correlação entre as variaveis numéricas
cor(df_num)
plot(df_num)

#Significancia de variaveis conforme o Z-score
price_bor = Boruta(Price~., data = df_num)
print(price_bor)
plot(price_bor)
attStats(price_bor)

#Adicionando os tipos de casas
df_num['Type'] = df_clean$Type

#Contando colunas com valores Igual a Zero:
colSums(df_num != 0)

##########################
# VARIAVEIS SELECIONADAS
##########################
# Price: Price in dollars

# Rooms: Number of rooms
# Distance: Distance from CBD (central business district)
# Propertycount: Number of properties that exist in the suburb.
# Bathroom: Number of Bathrooms
# Car: Number of carspots
# Landsize: Land Size
# Lattitude
# Longtitude
# Type: h, t, u

####
## Analise Exploratória
#####

# Distribuição dos preço médio dos imóveis
par(mfrow=c(1,1))
qplot(df_num$Price,
      geom = c("density")) +
  xlab("Preço")+ ylab("Densidade")+
  ggtitle("Preço")+
  theme_minimal()

qplot(df_num$Price,
      geom = c("boxplot")) +
  xlab("Preço")+ ylab(" ")+
  ggtitle("Preço")+
  theme_minimal()

summary(df_num$Price)

#Distância do centro comercial
qplot(df_num$Distance, 
      geom = c("density"))+
  xlab("Distância") + ylab("Densidade")+
  ggtitle("Disntância do centro comercial")+
  theme_minimal()

qplot(df_num$Distance, 
      geom = c("boxplot"))+
  xlab("Distância") + ylab(" ")+
  ggtitle("Disntância do centro comercial")+
  theme_minimal()

#Número de Quartos
qplot(df_num$Rooms)+
  xlab("Quartos") + ylab("Frequência")+
  ggtitle("Número de quartos")+
  theme_minimal()

#Número de Banheiros
qplot(df_num$Bathroom)+
  xlab("Banheiros") + ylab("Frequência")+
  ggtitle("Número de banheiros")+
  theme_minimal()

#Número de vagas para Carros
qplot(df_num$Car)+
  xlab("Vagas") + ylab("Frequência")+
  ggtitle("Número de vagas para carros")+
  theme_minimal()

#Tipos de casas
qplot(df_num$Type)+
  xlab(" ") + ylab("Frequência")+
  ggtitle("Tipo de Casas")+
  theme_minimal()

#Número de Propriedades no Suburbio
qplot(df_num$Propertycount, geom="density")+
  xlab("Número de propriedades") + ylab("Densidade")+
  ggtitle("Número de propriedades no bairro")+
  theme_minimal()

qplot(df_num$Propertycount, geom="boxplot")+
  xlab("Número de propriedades") + ylab(" ")+
  ggtitle("Número de propriedades no bairro")+
  theme_minimal()

#--------------------
#Possíveis outliers:
#Tamanho do terreno
#--------------------
qplot(df_num$Landsize)+
  xlab("Tamanho do terreno") + ylab("Densidade")+
  xlim(0,5000)+
  ggtitle("Tamanho do terreno")+
  theme_minimal()

qplot(df_num$Landsize, geom="boxplot")+
  xlab("Tamanho do terreno") + ylab("Densidade")+
  ggtitle("Tamanho do terreno")+
  theme_minimal()

plot(df_num$Longtitude, df_num$Lattitude, type="p",
     xlab = "Longitude", ylab = "Latitude")


#-------------------------------------------------
#Regressão linear para verificar a correlação e 
#significancia das variaveis
#-------------------------------------------------

#Incluindo variavel dummy
df_num$Type = factor(df_num$Type)

modelo_linear = lm(Price~., data=df_num) 
coef(modelo_linear)
summary(modelo_linear)

#---------------------------------------------
# Selecionando e removendo possívies outliers
# Influência e alavancagem
#---------------------------------------------
par(mfrow=c(1,1))
plot(density(df_num$Price), col='black', lwd=2.2,
     xlab='Preço', ylab='Densidade', "Dados vs Previsão")
lines(density(predict(modelo_linear, df_num, type='response')), col='red', lwd=2.2)

plot(cooks.distance(modelo_linear))
which(influence.measures(modelo_linear)$is.inf[,'cook.d'] )

cooksd_m0 = cooks.distance(modelo_linear)
length(cooksd_m0[cooksd_m0 > mean(cooksd_m0) * 2])

#-----------------
# Split dos dados
#----------------

head(df_num)
tail(df_num)
dim(df_num)

#Gerando uma amostra aleatoria: 80% treino | 20% teste
sample = sample.int(n = nrow(df_num), 
                    size=floor(0.80*nrow(df_num)), 
                    replace = FALSE)

#DF Treino
train = df_num[sample, ]
#DF Teste
test = df_num[-sample, ]

#verificando o conjunto de treino
head(train)
dim(train)
#verificando o conjunto de teste
head(test)
dim(test)

dim(test)[1]+ dim(train)[1] == dim(df_num)[1]

#-------------------------
##  SELEÇÃO DE MODELOS
#-------------------------

#Familia gaussiana, link = identidade
glm_gaussian = glm(Price~., data=train, 
                   family = gaussian(link = "identity"))

#Familia gaussiana, link = log
glm_gaussian2 = glm(Price~., data=train, 
                   family = gaussian(link = "log"))

#Familia Gamma, link = 'log'
glm_gamma = glm(Price~., data= train,
                family= Gamma(link='log'))

#-------------------------------------
#Pontos de alta alavancagem/Outliers
#------------------------------------
#R2
#https://www.sagepub.com/sites/default/files/upm-binaries/21121_Chapter_15.pdf

tab = matrix(cbind(with(summary(glm_gaussian), 1 - deviance/null.deviance),
                   AIC(glm_gaussian),
                   BIC(glm_gaussian),
                   with(summary(glm_gaussian2), 1 - deviance/null.deviance),
                   AIC(glm_gaussian2),
                   BIC(glm_gaussian2),
                   with(summary(glm_gamma), 1 - deviance/null.deviance),
                   AIC(glm_gamma),
                   BIC(glm_gamma)),
             ncol=3, byrow=TRUE)
colnames(tab) = c("R2", "AIC", "BIC")
rownames(tab) = c("Gau., link = ide.", 
                 "Gau., link = log", 
                 "Gamma, link = log")
tab

#----------------------
#  Calculando Erros
#---------------------
tab2 = matrix(cbind(
  sqrt(mean( (train$Price - predict(glm_gaussian, train))^2 )),
  sqrt(mean( (test$Price - predict(glm_gaussian, test) )^2 )),
  sqrt(mean( (train$Price - predict(glm_gaussian2, train))^2 )),
  sqrt(mean( (test$Price - predict(glm_gaussian2, test) )^2 )),
  sqrt(mean( (train$Price - predict(glm_gamma, train))^2 )),
  sqrt(mean( (test$Price - predict(glm_gamma, test) )^2 ))),
  ncol=2, byrow=TRUE)  
colnames(tab2) = c("In Sample", "Out Sample")
rownames(tab2) = c("Gau., link = ide.", 
                  "Gau., link = log", 
                  "Gamma, link = log")
tab2


#----------------------------
# ANALISE DOS RESIDUOS
#----------------------------

#DESVIO RESIDUAL PADRONIZADO
plot(density(rstandard(glm_gaussian, type='deviance')), col='black',lwd=2.2, 
     main="Desio Residual Padronizado", xlab = " ", ylab="Densidade")
lines(density(rstandard(glm_gaussian2, type='deviance')), col='red',lwd=2.2)
lines(density(rstandard(glm_gamma, type='deviance')), col='blue',lwd=2.2)
legend("topright",legend=c("Gaussiana-Identidade", 
                           "Gaussiana-Log", 
                           "Gamma-Log"), col=c("black", "red", "blue"), 
       lty=1:1, cex=0.8)


plot(density(qresid(glm_gaussian)), col='black', lwd=2.2,
     main="Quantis residuais", xlab = " ", ylab="Densidade")
lines(density(qresid(glm_gaussian2)), col='red',lwd=2.2)
lines(density(qresid(glm_gamma)), col='blue',lwd=2.2)
legend("topright",legend=c("Gaussiana-Identidade", 
                           "Gaussiana-Log", 
                           "Gamma-Log"), col=c("black", "red", "blue"), 
       lty=1:1, cex=0.8)


par(mfrow=c(1,2))
#-----------------------------
#TREINO vs PREVISAO
#----------------------------
plot(density(train$Price), col='grey', lwd=2.2,
     xlab='Preço', ylab='Densidade', "Treino vs Previsão")
lines(density(predict(glm_gaussian, train, type='response')), col='black', lwd=2.2)
lines(density(predict(glm_gaussian2, train, type='response')), col='red', lwd=2.2)
lines(density(predict(glm_gamma, train, type='response')), col='blue', lwd=2.2)
legend("topright",legend=c("Dados de treino",
                           "Gaussiana-Identidade", 
                           "Gaussiana-Log", 
                           "Gamma-Log"), col=c("grey","black", "red", "blue"), 
       lty=1:1, cex=0.6)

#---------------------------
#TESTE vs PREVISAO
#--------------------------
plot(density(test$Price), col='grey', lwd=2.2,
     xlab='Preço', ylab='Densidade', "Teste vs Previsão")
lines(density(predict(glm_gaussian, test, type='response')), col='black', lwd=2.2)
lines(density(predict(glm_gaussian2, test, type='response')), col='red', lwd=2.2)
lines(density(predict(glm_gamma, test, type='response')), col='blue', lwd=2.2)
legend("topright",legend=c("Dados de treino",
                           "Gaussiana-Identidade", 
                           "Gaussiana-Log", 
                           "Gamma-Log"), col=c("grey","black", "red", "blue"), 
       lty=1:1, cex=0.6)

par(mfrow=c(1,3))

#-----------------------------
# INDEPENDENCIA DAS VARIAVEIS
#----------------------------
scatter.smooth(rstandard(glm_gaussian, type='deviance'), 
               col='grey', main='Desvios\n Gaussian-Identidade')
scatter.smooth(rstandard(glm_gaussian2, type='deviance'), 
               col='grey', main='Desvios\n Gaussian-Log')
scatter.smooth(rstandard(glm_gamma, type='deviance'), 
               col='grey', main='Desvios\n Gamma-Log')
#-------------------------

par(mfrow=c(1,3))
scatter.smooth(predict(glm_gaussian, train, type='response'),
               rstandard(glm_gaussian, type='deviance'), 
               col='grey', main='Desvios\n Gaussian-Identidade')
scatter.smooth(predict(glm_gaussian2, train, type='response'),
               rstandard(glm_gaussian2, type='deviance'), 
               col='grey', main='Desvios\n Gaussian-Log')
scatter.smooth(predict(glm_gamma, train, type='response'),
               rstandard(glm_gamma, type='deviance'), 
               col='grey', main='Desvios\n Gamma-Log')


#---------------
# QQPLOT
#--------------

par(mfrow=c(1,3))
qqnorm(qresid(glm_gaussian)) 
qqline(qresid(glm_gaussian))

qqnorm(qresid(glm_gaussian2)) 
qqline(qresid(glm_gaussian2))

qqnorm(qresid(glm_gamma)) 
qqline(qresid(glm_gamma))

#--------------------
## DISTÂNCIA DE  COOK
#--------------------

par(mfrow=c(1,3))
plot(cooks.distance(glm_gaussian))
plot(cooks.distance(glm_gaussian2))
plot(cooks.distance(glm_gamma))

# Podemos ainda especificar os pontos específicos com alta 
# alavancagem usando uma referência de 2 vezes a média da 
# distância de Cook:

cooksd_m1 = cooks.distance(glm_gaussian)
cooksd_m2 = cooks.distance(glm_gaussian2)
cooksd_m3 = cooks.distance(glm_gamma)

length(cooksd_m0[cooksd_m1 > mean(cooksd_m1) * 2])
length(cooksd_m1[cooksd_m2 > mean(cooksd_m2) * 2])
length(cooksd_m2[cooksd_m3 > mean(cooksd_m3) * 2])


which(influence.measures(glm_gaussian)$is.inf[,'cook.d'] )
which(influence.measures(glm_gaussian2)$is.inf[,'cook.d'] )
which(influence.measures(glm_gamma)$is.inf[,'cook.d'] )

#--------------------------------------
#         MODELO SELECIONADO:
#             GAMMA - LOG
#-------------------------------------

#-------------------------------------------------------------------
# Note:
# Vamor reavalir o modelo para obter um melhor ajuste e entender
# como as variáveis influenciam no preços dos imóveis
# O processo a seguir pode ser repetido mais de uma vez. 
#--------------------------------------------------------------------

glm_gamma = glm(Price~., data= df_num,
                family= Gamma(link='log'))


tab3 = matrix(cbind(with(summary(glm_gamma), 1 - deviance/null.deviance),
                    NagelkerkeR2(glm_gamma)[2]),
              ncol = 2, byrow = TRUE)
colnames(tab3) = c("R2", "NagelkerkeR2")
tab3


#--------------------
#     PREVISAO
#-------------------
par(mfrow=c(1,1))
plot(density(df_num$Price), col='grey', lwd=2.2,
     xlab='Preço', ylab='Densidade', "Previsão\n Gamma-Log")
lines(density(predict(glm_gamma, df_num, type='response')), col='blue', lwd=2.2)
legend("topright",legend=c("Dados",
                           "Gamma-Log"), col=c("grey","blue"), 
       lty=1:1, cex=0.6)

#------------------------
### ANALISE DOS RESIDUOS
#-----------------------

par(mfrow=c(1,1))
#DESVIO RESIDUAL PADRONIZADO
plot(density(rstandard(glm_gamma, type='deviance')), col='black',lwd=2.2, 
     main=" ",
     xlab = " ", ylab="Densidade")
#QUANTIS RESIDUAL PADRONIZADO
lines(density(qresid(glm_gamma)), col='red', lwd=2.2,
     main=" ", 
     xlab = " ", ylab="Densidade")
legend("topright",legend=c("Desvios Padronizado",
                           "Quantis Residuais Padronizados"), col=c("black","red"), 
       lty=1:1, cex=0.6)

rd = residuals(glm_gamma, type="deviance")
fi = summary(glm_gamma)$dispersion
h = hatvalues(glm_gamma)
r = rd * sqrt(fi/(1-h))

plot(r, pch="+", xlab = " ", ylab="Residuos") 
abline(h = c(-2,0,2), lty=3)

#------------
# QQPLOT
#-----------
qqnorm(qresid(glm_gamma)) 
qqline(qresid(glm_gamma))

#--------------
### Evelope Simulado
#--------------
hnp(glm_gamma, resid.type = "deviance", halfnormal = FALSE)

#----------------------------
## DISTÂNCIA DE  COOK
#----------------------------
plot(cooks.distance(glm_gamma))

# Podemos ainda especificar os pontos específicos com alta 
# alavancagem usando uma referência de 2 vezes a média da 
# distância de Cook:
cooksd = cooks.distance(glm_gamma)
length(cooksd[cooksd > mean(cooksd) * 2])

########
### Alavancagem
########
plot(hatvalues(glm_gamma))

#################################
# Remoção e revaliação do modelo
#################################
# 
# Repetir o processo e analisar os resultados:
# Determinando os pontos de influencia
# para remoção e reajuste do modelo
###############################

influence.measures(glm_gamma)

#Distância de cook
cd = which(influence.measures(glm_gamma)$is.inf[,'cook.d'] )
length(cd)

#Hat
hat = which(influence.measures(glm_gamma)$is.inf[,'hat'] )
length(hat)

#dfb.Lbgt
dfb1 = which(influence.measures(glm_gamma)$is.inf[,'dfb.Lngt'] )
length(dfb1)

#dfb.Prpr
dfb2 = which(influence.measures(glm_gamma)$is.inf[,'dfb.Prpr'] )
length(dfb2)

#dffit
dfit = which(influence.measures(glm_gamma)$is.inf[,'dffit'] )
length(dfit)

#cov.r
cr = which(influence.measures(glm_gamma)$is.inf[,'cov.r'] )
length(cr)

#lista de colunas a remover
delrows = append(cd, hat)
delrows = append(delrows, dfb1)
delrows = append(delrows, dfb2)
delrows = append(delrows, dfit)
delrows = append(delrows, cr)

#Proporção do novo conjunto de dados para verificar 
# a influencia das variáveis
(dim(df_num)[1] - length(sort(unique(delrows), decreasing = TRUE)))/dim(df)[1]  

#Selecionado os colunas unicas da maior para o menor:
for(i in sort(unique(delrows), decreasing = TRUE) ){
  df_num = df_num[-c(i),]
  #print(i)
  }
dim(df_num)


#Exportando conjunto de dados final 
#write.csv2(x=df_num, file='df_ModeloFinal.csv', row.names = F)

#-----------------------


#-----------------------
#   MODELO FINAL
#-----------------------

glm_gamma = glm(Price~., data= df_num,
                family= Gamma(link='log'))


tab4 = matrix(cbind(with(summary(glm_gamma), 1 - deviance/null.deviance),
                    NagelkerkeR2(glm_gamma)[2]),
              ncol = 2, byrow = TRUE)
colnames(tab4) = c("R2", "NagelkerkeR2")
tab4


#------------------------
#### PREVISAO
#------------------------
plot(density(df_num$Price), col='grey', lwd=2.2,
     xlab='Preço', ylab='Densidade', "Previsão\n Gamma-Log")
lines(density(predict(glm_gamma, df_num, type='response')), col='blue', lwd=2.2)
legend("topright",legend=c("Dados",
                           "Gamma-Log"), col=c("grey","blue"), 
       lty=1:1, cex=0.6)

#-------------------------
### ANALISE DOS RESIDUOS
#-------------------------

par(mfrow=c(1,1))
#DESVIO RESIDUAL PADRONIZADO
plot(density(rstandard(glm_gamma, type='deviance')), col='black',lwd=2.2, 
     main=" ",
     xlab = " ", ylab="Densidade")
#QUANTIS RESIDUAL PADRONIZADO
lines(density(qresid(glm_gamma)), col='red', lwd=2.2,
     main=" ", 
     xlab = " ", ylab="Densidade")
legend("topright",legend=c("Desvios Padronizado",
                           "Quantis Residuais Padronizados"), col=c("black","red"), 
       lty=1:1, cex=0.6)

rd = residuals(glm_gamma, type="deviance")
fi = summary(glm_gamma)$dispersion
h = hatvalues(glm_gamma)
r = rd * sqrt(fi/(1-h))

plot(r, pch="+", xlab = " ", ylab="Residuos") 
abline(h = c(-2,0,2), lty=3, col='red')

#------------
# QQPLOT
#-------------
qqnorm(qresid(glm_gamma)) 
qqline(qresid(glm_gamma))

#----------------------
### Evelope Simulado
#---------------------
hnp(glm_gamma, resid.type = "deviance", halfnormal = FALSE)

summary(glm_gamma)
summary(df_num)