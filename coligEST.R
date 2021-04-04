
install.packages("stringr")
library(stringr)
library(MASS)

ufs<-read.csv("UFs.csv", header=FALSE, stringsAsFactors = FALSE)  #### Lê lista de siglas das UFs

deltaT_ij<-matrix(0,nrow=99, ncol=99)  ### Matriz Quadrada e Simétrica cujas linhas e colunas endereçam o partido e
                                          ### a existência ou não de coligação para a disputa da eleição de Governador com outros
                                        ########## que disputaram a eleição de Governador no Estado

for (x in 1:27) {       ###  Inicia repetição para leitura dos arquivos das pastas E'ano'/
  

  colig<-read.table(str_c("E1998/consulta_legendas_1998_", ufs$V1[x], ".txt"), sep=";", stringsAsFactors = TRUE)
  
  
  delta_ij<-matrix(0,nrow=99, ncol=99)  ### Matriz Quadrada e Simétrica cujas linhas e colunas endereçam o partido e
                                      ### a existência ou não de coligação para a disputa da eleição de Governador com outros
                                        ########## que disputaram a eleição de Governador no Estado

## Verificando quais partidos concorreram, coligados ou não, para Governador ##
  
  trans_sigla_num <- data.frame(colig$V13[which(colig$V10 == "GOVERNADOR")],colig$V12[which(colig$V10 == "GOVERNADOR")])
  names(trans_sigla_num)<-c("partido","numero")
  #trans_sigla_num$partido[which(trans_sigla_num$partido=="AVANTE") ]<-"PT do B"   ## correção para arquivo 2014
  #trans_sigla_num$partido[which(trans_sigla_num$partido=="PODE") ]<-"PTN"          ## correção para arquivo 2014
  
  if (x==1){
    sigla_num<-trans_sigla_num
  }
  else{
    sigla_num<-rbind(sigla_num,trans_sigla_num)
  }
    ### adiciona sigla e número ao data.frame sigla_num dos partidos que concorreram
  ## sigla_num será usado ao final como referência das siglas e seus números presentes naquela eleição no país todo!
  
  partidos<-as.character(colig$V13[which(colig$V10=="GOVERNADOR")])

  num.partidos<-as.character(colig$V12[which(colig$V10=="GOVERNADOR")])

  partidos<-as.factor(partidos)  ### Siglas dos partidos que concorreram para governador
  num.partidos<-as.factor(num.partidos) ### Número dos partidos que concorreram para governador

  partidos<-levels(partidos)      ### 'partidos' contém siglas dos partidos que concorreram, coligados ou não, para Governador
  num.partidos<-as.integer (levels(num.partidos))   ### 'num.partidos' contém número dos partidos que concorreram, 
                                                  ##          coligados ou não, para Governador

############################################################

  for (i in 1:length(num.partidos)) {
  
    coligacao <- as.character(colig$V16[which(colig$V10 == "GOVERNADOR" & colig$V12 == num.partidos[i])])
  
    coligacao <- levels(as.factor(coligacao))
  
      if (coligacao=="#NULO#")
    
        {

        for (j in 1:length(num.partidos)) {
    
            delta_ij[num.partidos[i],num.partidos[j]]<-0
        
            }
      
        }

          else   ######### Verifica com qual(is) partido(s) o partido 'num.partido(i)' possui coligação
      
            {
  
            partido.coligado <- as.character(colig$V12[which(colig$V10 == "GOVERNADOR" & colig$V16 == coligacao &! colig$V12 == num.partidos[i] )])
    
            partido.coligado <- as.integer(levels(as.factor(partido.coligado)))
    
            for (z in 1:length(partido.coligado)) {
    
              delta_ij[num.partidos[i],partido.coligado[z]]<-1  #### Identifica com '1' a posição da matriz
                                                                  #### que indica coligação entre o partido
                                                                #### num.partidos[i] e o partido partido.coligado[z]
    
              }
    
            }
  
        delta_ij[num.partidos[i],num.partidos[i]]<-1   ##### Identifica 1 na diagonal da matriz delta_ij
                                                     
  }

  deltaT_ij<-deltaT_ij+delta_ij

}


#######################################################################################################
################################ Tratamento Matriz 'deltaT_ij' ########################################################
##############################################################################################################

#################### Todos os Partidos ##############################################################

## Tratamento das siglas e números para todos os Partidos ###

colnames(sigla_num)<-c("partido","numero")
sigla_num<-data.frame(unique(sigla_num$partido), unique(sigla_num$numero))
colnames(sigla_num)<-c("partido","numero")
sigla_num<-sigla_num[order(sigla_num$numero),]

lincol<-vector(mode="integer",length=0)
part_ativos<-vector(mode="integer",length=0)

for (i in 1:99) {
  
  somando<-sum(deltaT_ij[i,]) ##### Verificando quais colunas-linhas/partidos de deltaT_ij estão inativos
  if (somando == 0) {
    lincol<-cbind(lincol,i)
  }
  else {
    part_ativos<-cbind(part_ativos,i)   ##### Vetor com número de partidos ativos alinhado com nova matriz deltaT_ij
  }
  
}

deltaT_ij<-deltaT_ij[-lincol,-lincol] ############ Subtraindo linhas e colunas de partidos inativos da matriz bruta deltaT_ij

deltaT_ij<-as.data.frame(deltaT_ij)
colnames(deltaT_ij)<-c(as.character(sigla_num$partido))
rownames(deltaT_ij)<-c(as.character(sigla_num$partido))

########### Partidos Relevantes #########################################################################

## Tratamento das siglas e números para Partidos Relevantes ###

sigla_relev<-vector(mode="integer",length=0)
part_relevantes<-c(11,12,13,14,15,22,23,25,40,45,65)
colnames(sigla_num)<-c("partido","numero")
sigla_num<-data.frame(unique(sigla_num$partido), unique(sigla_num$numero))
colnames(sigla_num)<-c("partido","numero")
sigla_num<-sigla_num[order(sigla_num$numero),]

for (i in 1:length(sigla_num$numero)) {
  aa<-0
  for (j in 1:11) {
    if (sigla_num[i,2]==part_relevantes[j]){
      aa<-1
    }
  }
  if (aa==0){
    sigla_relev<-cbind(sigla_relev,i)
  }
}

sigla_num<-sigla_num[-sigla_relev,]

lincol<-vector(mode="integer",length=0)

 for (i in 1:99) {
   aa<-0
   for (j in 1:11) {
    if (i==part_relevantes[j]){
       aa<-1
    }
   }
   if (aa==0){
     lincol<-cbind(lincol,i)
   }
 }

deltaT_ij<-deltaT_ij[-lincol,-lincol] ############ Subtraindo linhas e colunas de inativos/não relevantes da matriz bruta deltaT_ij

deltaT_ij<-as.data.frame(deltaT_ij)
colnames(deltaT_ij)<-c(as.character(sigla_num$partido))
rownames(deltaT_ij)<-c(as.character(sigla_num$partido))

#####################################################################################################
########## Tratamento Gráfico #######################################################################
#####################################################################################################

aa<-dist(deltaT_ij, method = "euclidean",diag=TRUE) # euclidean distances between the rows

fit1 <- cmdscale(aa,eig=TRUE, k=2) # k is the number of dim
fit1 # view results

# plot solution 
x <- fit1$points[,1]
y <- fit1$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric	MDS - 1998",	type="n")
text(x, y, labels = row.names(deltaT_ij), cex=.7)


fit2 <- isoMDS(aa, y = cmdscale(aa, 2), k=2, maxit = 50, tol = 1e-3, p=2) # k is the number of dim
fit2 # view results

# plot solution 
x <- fit2$points[,1]
y <- fit2$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Nonmetric MDS", type="n")
text(x, y, labels = row.names(deltaT_ij), cex=.7)
