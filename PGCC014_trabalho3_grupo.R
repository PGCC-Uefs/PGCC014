```{r}
install.packages("corrplot")
library(dplyr)
library(corrplot)
library(RColorBrewer)

codesmells_classes <- read.table("/home/beto/Documentos/PGCC Uefs/PGCC014/Atividade_3/datasets/codesmells_classes.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
codesmells_methods <- read.table("/home/beto/Documentos/PGCC Uefs/PGCC014/Atividade_3/datasets/codesmells_methods.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
context <- read.table("/home/beto/Documentos/PGCC Uefs/PGCC014/Atividade_3/datasets/context.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
metrics_classes <- read.table("/home/beto/Documentos/PGCC Uefs/PGCC014/Atividade_3/datasets/metrics_classes.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
metrics_methods <- read.table("/home/beto/Documentos/PGCC Uefs/PGCC014/Atividade_3/datasets/metrics_methods.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
repository <- read.table("/home/beto/Documentos/PGCC Uefs/PGCC014/Atividade_3/datasets/repository.csv",header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)


########################################################################################################################
########## PARTE 1 - MANIPULACAO DE DADOS - CRIACAO DA TABELA COM COLUNA INDICANDO SE CLASSE EH SMELL OU NAO
########################################################################################################################

## dataset de classes ##

# Cria coluna para indicar que a classe eh smell
classesIndicacaoSmell <- codesmells_classes
classesIndicacaoSmell$ehSmell <- "true"

# faz um left join entre as tabelas que identificam classes smells e as classes que tem smell
classesEhSmell <- merge(x=codesmells_classes,y=classesIndicacaoSmell,by=c("idrepository","classe","code_smell"),all.x=TRUE)

# troca campos que ficaram com valor nulo apos o left join
classesEhSmell$ehSmell[classesEhSmell$code_smell=="NULL"] <- "false"
classesEhSmell$code_smell <- NULL


## dataset de metodos ##

# Cria coluna para indicar que a classe eh smell
methodsIndicacaoSmell <- codesmells_methods
methodsIndicacaoSmell$ehSmell <- "true"

# faz um left join entre as tabelas que indicam  metodos smells e os metodos que tem smell
methodsEhSmell <- merge(x=codesmells_methods,y=methodsIndicacaoSmell,by=c("idrepository","classe","method","code_smell"),all.x=TRUE)

# troca campos que ficaram com valor nulo apos o left join
methodsEhSmell$ehSmell[methodsEhSmell$code_smell=="NULL"] <- "false"
methodsEhSmell$code_smell <- NULL


########################################################################################################################
########## SEPARACAO DOS CONTEXTOS POR GRUPO
########################################################################################################################

##nao entendi para que essa funcao, vai usar em algum lugar?
context_group <- context %>% group_by(contextLoc,contextNumContributors,contextNumCommits,contextTime) %>% summarize(n())

########################################################################################################################
########## PARTE 2 - VISUALIZACAO DO NUMERO DE CLASSES SMELLS POR PROJETO
########################################################################################################################

# Classes
dadosClasses <- classesEhSmell

colnames(dadosClasses)[2] <- "className"
colnames(dadosClasses)[3] <- "ehSmell"

dadosClasses$className <- NULL

dadosClasses_group <- dadosClasses %>% count(idrepository,ehSmell)

dadosClasses_group_true <- dadosClasses_group %>% filter(ehSmell == 'true')
dadosClasses_group_false <- dadosClasses_group %>% filter(ehSmell == 'false')

freqClasseSmells <- dadosClasses %>% count(ehSmell, sort = TRUE)
###  ehSmell     n
#1   false      19061
#2   true       853
###

#ordenando os dados de acordo com a quantidade de Classes Smells
dadosClasses_group_true <- dadosClasses_group_true[order(dadosClasses_group_true$n),] 
dadosClasses_group_true

#vizualizando a distribuicao de classes smells por projeto
boxplot(dadosClasses_group_true$n) 

# Valores dos quartis
summary(dadosClasses_group_true$n)
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.000   1.000   3.000   5.365   5.500  42.000   

#Valores de outliers
outlier_values <- boxplot.stats(dadosClasses_group_true$n)$out 
outlier_values
##[1] 13 13 13 13 14 15 15 18 18 18 19 19 20 20 23 24 26 28 28 33 42


#boxplot sem os outliers
boxplot(dadosClasses_group_true$n, outline=FALSE) 

########################################################################################################################
########## PARTE 2.1 - VERIFICANDO A CORRELACAO ENTRE NUMERO DE CLASSES SMELLS E DESENVOLVEDORES POR PROJETO
########################################################################################################################

dadosClasses_group_true <- inner_join(dadosClasses_group_true,context %>% select(idrepository,numContributors),by='idrepository')
dadosClasses_group_false <- inner_join(dadosClasses_group_false,context %>% select(idrepository,numContributors),by='idrepository')

plot(dadosClasses_group_true$n, dadosClasses_group_true$numContributors, xlab="Smells", ylab="Contributors", main="Classes (smell TRUE)", frame=FALSE, pch=19)
plot(dadosClasses_group_false$n, dadosClasses_group_false$numContributors, xlab="Smells", ylab="Contributors", main="Classes (smells FALSE)", frame=FALSE, pch=19)

## Calcula a correlacao numero de desenvolvedores X numero de classes smells

# Pearson
cor.test(dadosClasses_group_true$n, dadosClasses_group_true$numContributors, method=c("pearson"))
   ###Pearson's product-moment correlation

	#data:  dadosClasses_group_true$n and dadosClasses_group_true$numContributors
	#t = -0.64503, df = 157, p-value = 0.5198
	#alternative hypothesis: true correlation is not equal to 0
	#95 percent confidence interval:
	# -0.2054144  0.1050771
	#sample estimates:
	#        cor 
	#-0.05141092 
   ###
dadosCor$n <- dadosClasses_group_true$n
dadosCor$numContributors <- dadosClasses_group_true$numContributors
M <-cor(dadosCor)
corrplot(M, type="upper", order="hclust", title="Classes Smell vs Desenvolvedores",
         col=brewer.pal(n=8, name="RdYlBu"))


#Spearman
cor.test(dadosClasses_group_true$numContributors, dadosClasses_group_true$n, method=c("spearman"))

   ###Spearman's rank correlation rho

	#data:  dadosClasses_group_true$n and dadosClasses_group_true$numContributors
	#S = 507631, p-value = 0.002094
	#alternative hypothesis: true rho is not equal to 0
	#sample estimates:
     	#	rho 
	#0.242251
   ### 


# Kendal
cor.test(dadosClasses_group_true$n, dadosClasses_group_true$numContributors, method=c("kendal"))
    ###Kendall's rank correlation tau

	#data:  dadosClasses_group_true$n and dadosClasses_group_true$numContributors
	#z = 2.8406, p-value = 0.004504
	#alternative hypothesis: true tau is not equal to 0
	#sample estimates:
	#      tau 
	#0.1658214
    ### 

########################################################################################
########## PARTE 3 - VISUALIZACAO DO NUMERO DE METODOS SMELLS POR PROJETO
########################################################################################
# Methods
dadosMethods <- methodsEhSmell

colnames(dadosMethods)[2] <- "className"
colnames(dadosMethods)[3] <- "methodName"
colnames(dadosMethods)[4] <- "ehSmell"

dadosMethods$methodName <- NULL
dadosMethods$className <- NULL

dadosMethods_group <- dadosMethods %>% count(idrepository,ehSmell)

dadosMethods_group_true <- dadosMethods_group %>% filter(ehSmell == 'true')
dadosMethods_group_false <- dadosMethods_group %>% filter(ehSmell == 'false')

freqMethodSmells <- dadosMethods %>% count(ehSmell, sort = TRUE)
###  ehSmell      n
#1   false      136602
#2   true       5061
###

#ordenando os dados de acordo com a quantidade de metodos Smells
dadosMethods_group_true <- dadosMethods_group_true[order(dadosMethods_group_true$n),] 
dadosMethods_group_true

#vizualizando a distribuicao de metodos smells por projeto
boxplot(dadosMethods_group_true$n) 

# Valores dos quartis
summary(dadosMethods_group_true$n)
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     1       3       8      21      26     177

#Valores de outliers
outlier_values <- boxplot.stats(dadosMethods_group_true$n)$out 
outlier_values
##[1]  61  63  63  66  68  71  75  76  76  80  83  87  87  99 103 103 104 125 127
##[20] 136 147 156 157 177

#boxplot sem os outliers
boxplot(dadosMethods_group_true$n, outline=FALSE) 

########################################################################################################################
########## PARTE 3.1 - VERIFICANDO A CORRELACAO ENTRE NUMERO DE METODOS SMELLS E DESENVOLVEDORES POR PROJETO
########################################################################################################################

dadosMethods_group_true <- inner_join(dadosMethods_group_true,context %>% select(idrepository,numContributors),by='idrepository')
dadosMethods_group_false <- inner_join(dadosMethods_group_false,context %>% select(idrepository,numContributors),by='idrepository')

plot(dadosMethods_group_true$n, dadosMethods_group_true$numContributors, xlab="Smells", ylab="Contributors", main="Methods (smells TRUE)", frame=FALSE, pch=19)
plot(dadosMethods_group_false$n, dadosMethods_group_false$numContributors, xlab="Smells", ylab="Contributors", main="Methods (smells FALSE)", frame=FALSE, pch=19)

## Calcula a correlacao numero de desenvolvedores X numero de metodos smells

# Pearson
cor.test(dadosMethods_group_true$numContributors, dadosMethods_group_true$n, method=c("pearson"))
   ###Pearson's product-moment correlation

	#data:  dadosMethods_group_true$numContributors and dadosMethods_group_true$n
	#t = 1.9616, df = 239, p-value = 0.05096
	#alternative hypothesis: true correlation is not equal to 0
	#95 percent confidence interval:
	# -0.0004953855  0.2482957838
	#sample estimates:
	#        cor 
	#0.1258789 
   ###
dadosCor$n <- dadosMethods_group_true$n
dadosCor$numContributors <- dadosMethods_group_true$numContributors
M <-cor(dadosCor)
corrplot(M, type="upper", order="hclust", title="Métodos Smell vs Desenvolvedores",
         col=brewer.pal(n=8, name="RdYlBu"))

#Spearman
cor.test(dadosMethods_group_true$numContributors, dadosMethods_group_true$n, method=c("spearman"))

   ###Spearman's rank correlation rho

	#data:  dadosMethods_group_true$numContributors and dadosMethods_group_true$n
	#S = 1331189, p-value = 3.129e-12
	#alternative hypothesis: true rho is not equal to 0
	#sample estimates:
     	#	rho 
	#0.4293797
   ### 


# Kendal
cor.test(dadosMethods_group_true$numContributors, dadosMethods_group_true$n, method=c("kendal"))
    ###Kendall's rank correlation tau

	#data:  dadosMethods_group_true$numContributors and dadosMethods_group_true$n
	#z = 6.447, p-value = 1.141e-10
	#alternative hypothesis: true tau is not equal to 0
	#sample estimates:
	#      tau 
	#0.2966338 
    ### 


########################################################################################################################
########## PARTE 4 - VERIFICANDO A CORRELACAO ENTRE NUMERO DE METODOS SMELLS E NUMERO DE CLASSES SMELLS POR PROJETO
########################################################################################################################

# Classes vs Methods
metodo_classe <- left_join(dadosMethods_group_true,dadosClasses_group_true,by="idrepository")

left_join <- metodo_classe
left_join[is.na(left_join)] <- 0

colnames(left_join)[3] <- "methodsSmell"
colnames(left_join)[6] <- "classesSmell"

left_join$ehSmell.x <- NULL
left_join$ehSmell.y <- NULL
left_join$numContributors.x <- NULL
left_join$numContributors.y <- NULL
plot(left_join$methodsSmell, left_join$classesSmell, xlab="Method Smells", ylab="Classes Smells", main="Methods vs Classes", frame=FALSE, pch=19)

left_join <- left_join %>% filter(classesSmell > 0)
plot(left_join$methodsSmell, left_join$classesSmell, xlab="Method Smells", ylab="Classes Smells", main="Methods vs Classes para > 0", frame=FALSE, pch=19)


#vizualizando a distribuicao de metodos smells por classe e projeto
boxplot(left_join$methodsSmell)

# Valores dos quartis
summary(left_join$methodsSmell)
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   1.0     6.0    19.0    30.5    37.0   177.0

#Valores de outliers
outlier_values <- boxplot.stats(left_join$methodsSmell)$out 
outlier_values
##[1] 87  87  99 103 103 104 125 127 136 147 156 157 177

#boxplot sem os outliers
boxplot(left_join$methodsSmell, outline=FALSE)


## Calcula a correlacao numero de metodos com Smells X numero de classes com smells

# Pearson
cor.test(left_join$methodsSmell, left_join$classesSmell, method=c("pearson"))
   ###Pearson's product-moment correlation

	#data:  left_join$methodsSmell and left_join$classesSmell
	#t = 6.645, df = 143, p-value = 5.915e-10
	#alternative hypothesis: true correlation is not equal to 0
	#95 percent confidence interval:
	# 0.3504650 0.6011378
	#sample estimates:
	#        cor 
	#0.4857252 
   ###
dadosCor$n <- left_join$methodsSmell
dadosCor$numContributors <- left_join$classesSmell
colnames(dadosCor)[1] <- "methodsSmell"
colnames(dadosCor)[2] <- "classesSmell"
M <-cor(dadosCor)
corrplot(M, type="upper", order="hclust", title="Classes Smell vs Métodos Smell - Por projeto",
         col=brewer.pal(n=8, name="RdYlBu"))

# Spearman
cor.test(left_join$methodsSmell, left_join$classesSmell, method=c("spearman"))
   ###Spearman's rank correlation rho

	#data:  left_join$methodsSmell and left_join$classesSmell
	#S = 224444, p-value = 2.978e-13
	#alternative hypothesis: true rho is not equal to 0
	#sample estimates:
     	#	rho 
	#0.5582515
   ### 

# Kendal
cor.test(left_join$methodsSmell, left_join$classesSmell, method=c("kendal"))
    ###Kendall's rank correlation tau

	#data:  left_join$methodsSmell and left_join$classesSmell
	#z = 7.2985, p-value = 2.91e-13
	#alternative hypothesis: true tau is not equal to 0
	#sample estimates:
	#      tau 
	#0.4355623  
    ### 

#################################################################################################################################
########## PARTE 5 - VERIFICANDO A CORRELACAO ENTRE NUMERO DE CLASSES E METODOS SMELLS COM NUMERO DE LINHAS DE CODIGO POR PROJETO
#################################################################################################################################

left_join<-left_join(left_join,context,by="idrepository")
dados5 <- left_join

#removendo algumas colunas para deixar somente as de interesse para essa analise
dados5$numContributors <- NULL
dados5$numCommits <- NULL
dados5$time <- NULL
dados5$contextLoc <- NULL
dados5$contextNumContributors <- NULL
dados5$contextNumCommits <- NULL
dados5$contextTime <- NULL
dados5

#trabalhar os dados para visualizacao, mudando o nome das colunas
colnames(dados5)[1] <- "Project"
colnames(dados5)[2] <- "NumMetSmells"
colnames(dados5)[3] <- "NumClaSmells"
colnames(dados5)[4] <- "NumLinhas"
dados5

#########################################################################################################
########## PARTE 5.1 - CORRELACAO ENTRE NUMERO DE METODOS SMELLS E NUMERO DE LINHAS DE CODIGO POR PROJETO
#########################################################################################################

#ordenando os dados de acordo com a quantidade de linhas
dados5 <- dados5[order(dados5$NumLinhas),] 
dados5

plot(dados5$NumLinhas, dados5$NumMetSmells, xlab="NumLinhas", ylab="Method Smells", main="Methods smells vs loc numbers", frame=FALSE, pch=19)

#vizualizando a distribuicao de numero de linhas de codigo projeto
boxplot(dados5$NumLinhas)

# Valores dos quartis
summary(dados5$NumLinhas)
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   954   17910   23120   21305   29868   40304 

#Valores de outliers
outlier_values <- boxplot.stats(dados5$NumLinhas)$out 
outlier_values
##integer(0)

#boxplot sem os outliers
boxplot(dados5$NumLinhas, outline=FALSE)

## Calcula a correlacao numero de metodos com Smells X numero de linhas

# Pearson
cor.test(dados5$NumLinhas, dados5$NumMetSmells, method=c("pearson"))
   ###Pearson's product-moment correlation

	#data: dados5$NumLinhas and dados5$NumMetSmells
	#t = 5.3591, df = 143, p-value = 3.273e-07
	#alternative hypothesis: true correlation is not equal to 0
	#95 percent confidence interval:
	# 0.2635163 0.5362210
	#sample estimates:
	#        cor 
	#0.4089584 
   ###
dadosCor$methodsSmell <- dados5$NumMetSmells
dadosCor$classesSmell <- dados5$NumLinhas
colnames(dadosCor)[2] <- "NumLinhas"
M <-cor(dadosCor)
corrplot(M, type="upper", order="hclust", title="Métodos Smell vs Linhas",
         col=brewer.pal(n=8, name="RdYlBu"))


# Spearman
cor.test(dados5$NumLinhas, dados5$NumMetSmells, method=c("spearman"))
   ###Spearman's rank correlation rho

	#data:  dados5$NumLinhas and dados5$NumMetSmells
	#S = 251595, p-value = 9.508e-11
	#alternative hypothesis: true rho is not equal to 0
	#sample estimates:
     	#	rho 
	#0.5048115
   ### 

# Kendal
cor.test(dados5$NumLinhas, dados5$NumMetSmells, method=c("kendal"))
    ###Kendall's rank correlation tau

	#data:  dados5$NumLinhas and dados5$NumMetSmells
	#z = 6.2635, p-value = 3.764e-10
	#alternative hypothesis: true tau is not equal to 0
	#sample estimates:
	#      tau 
	#0.3542098  
    ### 

#########################################################################################################
########## PARTE 5.2 - CORRELACAO ENTRE NUMERO DE CLASSES SMELLS E NUMERO DE LINHAS DE CODIGO POR PROJETO
#########################################################################################################

plot(dados5$NumLinhas, dados5$NumClaSmells, xlab="NumLinhas", ylab="Classes Smells", main="Classes smells vs loc numbers", frame=FALSE, pch=19)

## Calcula a correlacao entre numero de linhas de codigo X numero de classes smells
# Pearson
cor.test(dados5$NumLinhas, dados5$NumClaSmells, method=c("pearson"))
   ###Pearson's product-moment correlation

	#data: dados5$NumLinhas and dados5$NumClaSmells
	#t = 5.4438, df = 143, p-value = 2.211e-07
	#alternative hypothesis: true correlation is not equal to 0
	#95 percent confidence interval:
	# 0.2695159 0.5408065
	#sample estimates:
	#        cor 
	#0.4143225 
   ###
dadosCor$methodsSmell <- dados5$NumClaSmells
dadosCor$NumLinhas <- dados5$NumLinhas
colnames(dadosCor)[2] <- "NumClaSmells"
M <-cor(dadosCor)
corrplot(M, type="upper", order="hclust", title="Classes Smell vs Linhas",
         col=brewer.pal(n=8, name="RdYlBu"))

# Spearman
cor.test(dados5$NumLinhas, dados5$NumClaSmells, method=c("spearman"))
   ###Spearman's rank correlation rho

	#data:  dados5$NumLinhas and dados5$NumClaSmells
	#S = 288597, p-value = 5.783e-08
	#alternative hypothesis: true rho is not equal to 0
	#sample estimates:
     	#	rho 
	#0.4319844
   ### 

# Kendal
cor.test(dados5$NumLinhas, dados5$NumClaSmells, method=c("kendal"))
    ###Kendall's rank correlation tau

	#data:  dados5$NumLinhas and dados5$NumClaSmells
	#z = 5.445, p-value = 5.179e-08
	#alternative hypothesis: true tau is not equal to 0
	#sample estimates:
	#      tau 
	#0.3219599  
    ### 

########################################################################################################################
########## PARTE 5.3 - VERIFICANDO A CORRELACAO ENTRE CLASSES E METODOS SMELLS JUNTOS E NUMERO DE LINHAS POR PROJETO
########################################################################################################################

# a variavel metodo_classe contem os smells de metodos e os smells de classe agrupados por projeto
#mudando os nomes da coluna para facilitar a analise
colnames(metodo_classe)[3] <- "methodsSmell"
colnames(metodo_classe)[6] <- "classesSmell"

#removendo colunas que nao serao usadas na analise
metodo_classe$ehSmell.x <- NULL
metodo_classe$ehSmell.y <- NULL
metodo_classe$numContributors.x <- NULL
metodo_classe$numContributors.y <- NULL

#substituindo os valores NULL apos a o merge e colocando 0 no lugar
metodo_classe <- replace(x = metodo_classe, list = is.na(metodo_classe), values = 0)

#somando os smells e colocando em uma nova coluna
metodo_classe$smells <- metodo_classe$methodsSmell + metodo_classe$classesSmell
metodo_classe

#juntando os smells com o contexto de numero de linhas
metodo_classe<-left_join(metodo_classe,context,by="idrepository")

#removendo algumas colunas para deixar somente as de interesse para essa analise
metodo_classe$numContributors <- NULL
metodo_classe$numCommits <- NULL
metodo_classe$time <- NULL
metodo_classe$contextLoc <- NULL
metodo_classe$contextNumContributors <- NULL
metodo_classe$contextNumCommits <- NULL
metodo_classe$contextTime <- NULL

#ordenando os dados de acordo com a quantidade de linhas
metodo_classe <- metodo_classe[order(metodo_classe$loc),] 
metodo_classe

plot(metodo_classe$loc, metodo_classe$smells, xlab="NumLinhas", ylab="Smells", main="Smells vs Loc numbers", frame=FALSE, pch=19)

## Calcula a correlacao entre numero de linhas de codigo X numero smells
# Pearson
cor.test(metodo_classe$loc, metodo_classe$smells, method=c("pearson"))
   ###Pearson's product-moment correlation

	#data:  metodo_classe$loc and metodo_classe$smells
	#t = 8.6972, df = 239, p-value = 5.569e-16
	#alternative hypothesis: true correlation is not equal to 0
	#95 percent confidence interval:
	# 0.3879850 0.5806991
	#sample estimates:
	#        cor 
	#0.4903122 
   ###
dadosCor$methodsSmell <- metodo_classe$loc
dadosCor$NumLinhas <- metodo_classe$smells
colnames(dadosCor)[1] <- "Loc"
colnames(dadosCor)[2] <- "Smells"
M <-cor(dadosCor)
corrplot(M, type="upper", order="hclust", title="Classes Smell e Métodos Smell vs Linhas",
         col=brewer.pal(n=8, name="RdYlBu"))



# Spearman
cor.test(metodo_classe$loc, metodo_classe$smells, method=c("spearman"))
   ###Spearman's rank correlation rho

	#data:  metodo_classe$loc and metodo_classe$smells
	#S = 902368, p-value < 2.2e-16
	#alternative hypothesis: true rho is not equal to 0
	#sample estimates:
     	#	rho 
	#0.6131958
   ### 

# Kendal
cor.test(metodo_classe$loc, metodo_classe$smells, method=c("kendal"))
    ###Kendall's rank correlation tau

	#data:  metodo_classe$loc and metodo_classe$smells
	#z = 10.003, p-value < 2.2e-16
	#alternative hypothesis: true tau is not equal to 0
	#sample estimates:
	#      tau 
	#0.4404655  
    ### 

########################################################################################################################
########## PARTE 5.4 - VERIFICANDO A CORRELACAO GERAL DO DATAFRAME CONTEXTO
########################################################################################################################

dadosCor <- context
dadosCor$idrepository <- NULL
dadosCor$contextLoc <- NULL
dadosCor$contextNumContributors <- NULL
dadosCor$contextNumCommits <- NULL
dadosCor$contextTime <- NULL
M <-cor(dadosCor)
corrplot(M, method="number", type="upper", order="hclust", title="Correlação geral",
         col=brewer.pal(n=8, name="RdYlBu"))

```