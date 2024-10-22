#########################
#Explorando e preparando os dados

#################################

#Removendo a primeira e a ultima colunas

dados_cancer<-Exemplo_1_Dados_cancer[c(-1,-33)]

#Tabela de frequencias para os rotulos

table(dados_cancer$diagnosis)

#Codificando a variavel como fatlor
dados_cancer$diagnosis<-factor(dados_cancer$diagnosis)

# Criando uma função que normaliza

normaliza<-function(x){return((x-min(x))/(max(x)-min(x)))
}

#Normalizando os dados numericos

dados_cancer_n<-as.data.frame(lapply(dados_cancer[2:31],normaliza))

# Criando datasets de treino e teste

dados_cancer_treino<-dados_cancer_n[1:469,]
dados_cancer_teste<-dados_cancer_n[470:568,]

#Separando rótulos

dados_cancer_treino_rotulos<-dados_cancer[1:469,1]$diagnosis
dados_cancer_teste_rotulos<-dados_cancer[470:568,1]$diagnosis

#######################################
#Aplicação do K-NN
install.packages("class")
install.packages("caret")


library(class)
library(caret)

predicoes<-knn(train=dados_cancer_treino,
               test=dados_cancer_teste,
               cl=dados_cancer_treino_rotulos,k=3)
confusionMatrix(dados_cancer_teste_rotulos,predicoes)

#########################################
# Escolhando o K
#########################################

acuracia<-c()

for(k in 1:30){
  
set.seed(1234)

predicoes<-knn(train=dados_cancer_treino,
               test=dados_cancer_teste,
               cl=dados_cancer_treino_rotulos,k=k)
matriz<-confusionMatrix(dados_cancer_teste_rotulos,predicoes)
acuracia<-c(acuracia,matriz[['overall']]['Accuracy'])

}

plot(1:30,1-acuracia,type='l')

predicoes<-knn(train=dados_cancer_treino,
               test=dados_cancer_teste,
               cl=dados_cancer_treino_rotulos,k=6)
confusionMatrix(dados_cancer_teste_rotulos,predicoes)





