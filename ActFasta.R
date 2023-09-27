setwd("C:/Users/santi/OneDrive/Documents/Programacion/RStudio")
library(seqinr)
library(Biostrings)
zika<-read.fasta("fastaZika.fna")
print(zika$id[1])
covid<-read.fasta("fastaCovid.fna")
dengue<-read.fasta("fastaDengue.fna")
covid2<-read.fasta("fastaCovid2.fasta")

mersCov<-read.fasta("mersCov.fasta")
print(attr(mersCov,"name"))
#Obteniendo tamanio genoma
length(zika[[1]])
length(covid[[1]])
length(dengue[[1]])
length(covid2[[1]])
length(mersCov[[1]])
#Obtenemos el porcentaje por nucleotido



#Obtenemos la cuenta de cada nucleotido

conteoZika<-count(zika[[1]],1)
conteoCovid<-count(covid[[1]],1)
conteoDengue<-count(dengue[[1]],1)
conteoCovid2<-count(covid2[[1]],1)
conteomersCov<-count(mersCov[[1]],1)
conteomersCov
#Contenido GC

cat("El contenido GC de la secuencia de ADN del zika es:",  GC(zika[[1]]) * 100, "%\n")
cat("El contenido GC de la secuencia de ADN en el covid es:",  GC(covid[[1]]) * 100, "%\n")
cat("El contenido GC de la secuencia de ADN del dengue es:",  GC(dengue[[1]]) * 100, "%\n")
cat("El contenido GC de la secuencia de ADN del covid 2 es:",  GC(covid2[[1]]) * 100, "%\n")
cat("El contenido GC de la secuencia de ADN del mersCov es:",  GC(mersCov[[1]]) * 100, "%\n")

#Secuencia complementaria
head(comp(zika[[1]]))
head(comp(covid[[1]]))
head(comp(dengue[[1]]))
head(comp(covid2[[1]]))
(comp(mersCov[[1]]))

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Biostrings")

#CONTAR Contenido GC

conteoGC<-function(seq){
  largo<-length(seq)
  a<-0
  for(i in 1:largo){
    if(seq[i]=="g" ||seq[i]=="G" ||seq[i]=="C" ||seq[i]=="c"){
    
    a=a+1
    
    }

  }
  return(round(a/largo,3))
}

cat("El contenido GC del zika es:",  conteoGC(zika[[1]]) * 100, "%\n")
cat("El contenido GC del covid es:",  conteoGC(covid[[1]]) * 100, "%\n")
cat("El contenido GC del dengue es:", conteoGC(dengue[[1]]) * 100, "%\n")
cat("El contenido GC del covid 2 es:",  conteoGC(covid2[[1]]) * 100, "%\n")


#Data frame de contenido
data_frame<-data.frame(Adenina= 1:5, citocina=1:5, guanina=1:5,timina= 1:5)
rownames(data_frame)<-c("zika","sarsCovid","dengue","wuhan1","mersCov")
data_frame[1,]<-conteoZika
data_frame[2,]<-conteoCovid
data_frame[3,]<-conteoDengue
data_frame[4,]<-conteoCovid2
data_frame[5,]<-conteomersCov
data_frame


calc_porcentaje<- function(sec){
  tam_secuencia<-length(sec)
  A<-0
  T<-0
  G<-0
  C<-0
  
  for(i in 1:tam_secuencia){
    if(sec[i]=="A"||sec[i]=="a"){
      A<-A+1  
    }
    else if(sec[i]=="T"||sec[i]=="t"){
      T<-T+1  
    }
    else if(sec[i]=="G"||sec[i]=="g"){
      G<-G+1  
    }
    else if(sec[i]=="C"||sec[i]=="c"){
      C<-C+1  
    }
  }
  nucleotidos<-c(1,2,3,4)
  nucleotidos[1]<-round(((A/tam_secuencia)*100),2)
  nucleotidos[2]<-round(((C/tam_secuencia)*100),2)
  nucleotidos[3]<-round(((G/tam_secuencia)*100),2)
  nucleotidos[4]<-round(((T/tam_secuencia)*100),2)
  cat("Porcentaje de cada base A:",((A/tam_secuencia)*100),"% T:",((T/tam_secuencia)*100),"% G:",((G/tam_secuencia)*100),"%  C:",((C/tam_secuencia)*100),"%")
  return(nucleotidos)
  cat("Porcentaje de cada base A:",((A/tam_secuencia)*100),"% T:",((T/tam_secuencia)*100),"% G:",((G/tam_secuencia)*100),"%  C:",((C/tam_secuencia)*100),"%")
  
}
porcentajeZika<-calc_porcentaje(zika[[1]])
porcentajeCovid<-calc_porcentaje(covid[[1]])
porcentajeDengue<-calc_porcentaje(dengue[[1]])
porcentajeCovid2<-calc_porcentaje(covid2[[1]])
porcentajemersCov<-calc_porcentaje(mersCov[[1]])
porcentaje

data_frame2<-data.frame(Adenina= 1:5, citocina=1:5, guanina=1:5,timina= 1:5)
rownames(data_frame2)<-c("zika","sarsCovid","dengue","wuhan1","mersCov")
data_frame2[1,]<-porcentajeZika
data_frame2[2,]<-porcentajeCovid
data_frame2[3,]<-porcentajeDengue
data_frame2[4,]<-porcentajeCovid2
data_frame2[5,]<-porcentajemersCov
data_frame2
