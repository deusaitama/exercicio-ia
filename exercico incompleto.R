table(Exercício_1_Qualidade_do_leite$Grade)

#Codificando a variavel como fatlor
Exercício_1_Qualidade_do_leite$Grade<-factor(Exercício_1_Qualidade_do_leite$Grade)

# Criando uma função que normaliza

normaliza<-function(x){return((x-min(x))/(max(x)-min(x)))
}
#Normalizando os dados numericos

dados_leite_n<-as.data.frame(lapply(Exercício_1_Qualidade_do_leite[1:7],normaliza))

# Criando datasets de treino e teste

dados_leite_treino<-dados_leite_n[1:848,]
dados_leite_teste<-dados_leite_n[849:1059,]

#Separando rótulos

dados_leite_treino_rotulos<-Exercício_1_Qualidade_do_leite[1:848,]$Grade
dados_leite_teste_rotulos<-Exercício_1_Qualidade_do_leite[849:1059,]$Grade

acuracia<-c()

for(k in 1:30){
  
  set.seed(1234)
  
  predicoes<-knn(train=dados_leite_treino,
                 test=dados_leite_teste,
                 cl=dados_leite_treino_rotulos,k=k)
  matriz<-confusionMatrix(dados_leite_teste_rotulos,predicoes)
  acuracia<-c(acuracia,matriz[['overall']]['Accuracy'])
  
}
##n roda isso aqui