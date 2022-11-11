# Manipulação e visualização usando o R - Ufop 2022



#####################
# banco de dados
################

#Banco de dados com os alunos que participam de programas de iniciação científica da UFOP 2017 à 2021

dados = read.csv2(
  "http://dados.ufop.br/dataset/0de458ea-0c19-4b01-a712-cd7d37d4ad87/resource/266c947b-132c-4b49-aacd-956541043313/download/bolsistas_ic_v1_05112021.csv",
                  fileEncoding = "Latin1", check.names = F)
# disponivel em: http://dados.ufop.br/dataset/bolsistas-de-iniciacao-cientifica

# ou  
# dados = read.csv2("bolsistas_ic_v1_25022019.csv",fileEncoding = "Latin1")

str(dados)
dim(dados)
View(dados)


####################
#limpeza do banco
###################

# transformando strings em fatores
head(dados,2)
dados$curso_bolsista=factor(dados$curso_bolsista)
dados$programa=factor(dados$programa)
dados$ano=factor(dados$ano)
dados$setor=factor(dados$setor)
dados$fomento=factor(dados$fomento)
dados$tipo_bolsa=factor(dados$tipo_bolsa)


tabela1=table(dados$curso_bolsista)
View(tabela1)
# vemos que 96 alunos cursavam o ensino médio, há 4 células vazias(provavelmente do ensino médio) e algumas incoerências

# Alguns cursos apresentaram espaço " " no ivicio e isso fez com que um mesmo curso fosse dividido em mais de 1 categoria
#Para isso, existe a função str_trim(), que remove espaços em branco seguidos no início e no final do string

library(stringr)
dados$curso_bolsista =str_trim(dados$curso_bolsista)  # remove espaços em branco 

tabela1=table(dados$curso_bolsista)
View(tabela1)
# ainda há algumas incoerências

max(table(factor(dados$bolsista))) # temos até 5 linhas referentes a mesma pessoa
summary(dados)


###################
# Manipulaçao e visualização
###################

plot(factor(dados$ano))
plot(dados$fomento)
plot(dados$fomento[dados$tipo_bolsa=="VOLUNTARIO"])
plot(dados$fomento[dados$tipo_bolsa=="BOLSISTA"]) 
plot(factor(dados$tipo_bolsa))  # vemos que o tipo de bolsa pode ser "bolsista", "voluntário" e "Voluntario"
# Devemos padronizar as duas ultimas categorias pois elas possuem o mesmo significado.

dados$tipo_bolsa[which(dados$tipo_bolsa == "VOLUNTARIO")] <- "VOLUNTÁRIO"

plot(factor(dados$tipo_bolsa))

tabela1=table(factor(dados$curso_bolsista))
tabela1  # numero de alunos por curso

max(table(factor(dados$curso_bolsista)))

pie(table(factor(dados$curso_bolsista)))


# Outros gráficos

library(ggplot2)
# Comparação do número de bolsas ao longo dos anos
ggplot(dados,aes(ano))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  ggtitle("Comparação do número de bolsas \n ao longo dos anos") +
  xlab("Ano") + ylab("Frequência")

# Comparação do número de bolsas por fomento
ggplot(dados,aes(fomento))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  ggtitle("Comparação do número de bolsas \n por fomento") +
  xlab("Fomento") + ylab("Frequência")

# Comparação do número de bolsas pelo tipo da bolsa
ggplot(dados,aes(tipo_bolsa))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  ggtitle("Comparação do número de bolsas \n pelo tipo da bolsa") +
  xlab("Fomento") + ylab("Frequência")+
  scale_x_discrete(limits=c("BOLSISTA", "VOLUNTÁRIO"))

# Comparação do número de bolsas por curso
ggplot(dados,aes(curso_bolsista))+
  geom_bar(fill="steelblue")+
  theme_minimal()+
  ggtitle("Comparação do número de bolsas \n ao longo dos anos") +
  xlab("Curso") + ylab("Frequência")



write.csv(dados, "ictratada.csv") # salvando dados após pré processamento

# Esta base tratada será usada para a cosntrução do aplicativo Shiny

