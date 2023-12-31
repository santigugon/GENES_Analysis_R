
setwd("C:/Users/santi/OneDrive/Documents/Programacion/RStudio/fastasEvidencia")
library(seqinr)
library(Biostrings)
library(viridis)
library(DECIPHER)
library(ade4)
library(seqinr)
library(adegenet)
library(ape)
library(ggtree)
library(ggplot2)
#DEclaramos las funciones que usaremos posteriormente
analisis_filogenetico <- function(idsAcceso) {
  # Leer secuencias usando read.GenBank
  viral_sequences <- read.GenBank(idsAcceso)
  
  viral_dnabin <- viral_sequences
  
  # Guardar las secuencias en un archivo fasta
  write.fasta(sequences = viral_dnabin, names = idsAcceso, file.out = "viral_sequences.fasta")
  print("Fasta escrito con exito")
  
  # Leer secuencias del archivo fasta
  viral_stringset <- readDNAStringSet("viral_sequences.fasta")
  print(viral_stringset)
  # Alinear secuencias
  oriented_sequences <- OrientNucleotides(viral_stringset[1])
  aligned_sequences <- AlignSeqs(oriented_sequences)
  print("Secuencias Alineadas")
  
  # Guardar secuencias alineadas en un archivo fasta
  writeXStringSet(aligned_sequences, "aligned_viral_sequences.fasta", format = "fasta")
  
  # Leer secuencias alineadas del archivo fasta
  alignment <- read.alignment("aligned_viral_sequences.fasta", format = "fasta")
  
  # Calcular matriz de distancia
  dist_matrix <- dist.alignment(alignment)
  print(dist_matrix)
  
  
  # Crear un �rbol filogen�tico a partir de la matriz de distancia
  viral_phylo <- nj(dist_matrix)
  print(viral_phylo)
  
  # Graficar el �rbol filogen�tico
  plot(viral_phylo, main = "Arbol filogen�tico de genomas virales")
}


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

crear_graficos_pie <- function(data_frame_porcentaje) {
  for (i in 1:nrow(data_frame_porcentaje)) {
    # Obtener porcentajes para cada base nitrogenada
    porcentajes <- unlist(data_frame_porcentaje[i,])
    #print(porcentajes)
    # Crear vector de etiquetas para cada base nitrogenada
    etiquetas <- c("Adenina", "Citosina", "Guanina", "Timina")
    etiquetas_con_porcentaje <- paste(etiquetas, porcentajes, "%", sep = " ")
    # Crear gr�fico de pastel
    pie(porcentajes, labels = etiquetas_con_porcentaje)#, main = rownames(data_frame_porcentaje)[i]
    title(main = paste("Composici�n de bases nucleot�dicas en", rownames(data_frame_porcentaje)[i]))
  }}

#A continuacion cargaremos las secuencias de nuestros virus, estaremos revisando 9 paises distintos. Todas son secuencias del sars covid en su etapa inicial en 2020 en los distintos paises ademas de una secuencia adicional
covid<-read.fasta("sarscovid.fna")
alemania<-read.fasta("alemania.fasta")
australia<-read.fasta("australia.fasta")
grecia<-read.fasta("grecia.fasta")
india<-read.fasta("india.fasta")
nepal<-read.fasta("nepal.fasta")
serbia<-read.fasta("serbia.fasta")
usa<-read.fasta("usa.fasta")
vietnam<-read.fasta("vietnam.fasta")
wuhan<-read.fasta("wuhan.fasta")
genoma2003<-read.fasta("genoma2003.fna")

print(attr(usa[[1]],"name"))
print(alemania)

fastas<-list(covid[[1]], alemania[[1]], australia[[1]], grecia[[1]], india[[1]], nepal[[1]], serbia[[1]], usa[[1]], vietnam[[1]], wuhan[[1]],genoma2003[[1]])
fastas2<-list(covid, alemania, australia, grecia, india, nepal, serbia, usa, vietnam, wuhan,genoma2003)
fasta_paises <- list("sarscovid", "alemania", "australia", "grecia", "india", "nepal", "serbia", "usa", "vietnam", "wuhan","genoma2003")

print(fastas[2])

print(attr(fastas[[2]],"Annot"))

analisis_fasta<-function(fastas){
  numFastas<-length(fastas);
  tamanios<-c(1:11)
  ids<-c(1:11)
  composicionPorc<-c(1:11)
  
  #Adenina= 1:10, citosina=1:10, guanina=1:10,timina= 1:10
  #Inicio DF de parte 7
  data_frame_longitud<-data.frame(Longitud_genoma= 1:11)
  rownames(data_frame_longitud)<-fasta_paises
  
   data_frame_composicion<-data.frame(adenina= 1:11, citocina=1:11, guanina=1:11,timina= 1:11)
   rownames(data_frame_composicion)<-fasta_paises
   
   data_frame_porcentaje<-data.frame(adenina= 1:11, citocina=1:11, guanina=1:11,timina= 1:11)
   rownames(data_frame_porcentaje)<-fasta_paises
 
   data_frame_gc<-data.frame(ContenidoGC= 1:11)
   rownames(data_frame_gc)<-fasta_paises
  data_frame_resumen<-data.frame(Virus= "Sars Covid",ID= 1:11,Longitud= 1:11 ,ContenidoGC= 1:11)
   rownames(data_frame_resumen)<-fasta_paises
   
  for(i in 1:numFastas){
    
  #Parte6-Calculando longitud e imprimiendo en consola
  tamanios[i]<-length(fastas[i][[1]])
  pais<-fasta_paises[[i]]

  
  tamanio<-tamanios[i]
  
  
 
  data_frame_longitud[i,]<-tamanios[i]
  
  #Parte 9
  composicion<-count(fastas[[i]],1)
  
  
  
  #Parte 10
  data_frame_composicion[i,] <-composicion

  

  
  #Parte 12
  contenidogc<-GC(fastas[[i]])
  cat("El sars Covid en ",pais,"tiene una longitud de", tamanio,"el contenido GC de la secuencia es:",contenidogc   * 100, "% y su composici�n es de ")
  print(composicion )
  data_frame_porcentaje[i,]<-calc_porcentaje(fastas[[i]])
  cat(" \n ")
  data_frame_gc[i,] <-contenidogc
  cat(" \n ")
  
  #Obtener nombre de cada virus
  
  ids[i]<-attr(fastas[[i]],"Annot")
  #Resumen
  data_frame_resumen[1,1]<-"Sars Covid Tor 2"

  data_frame_resumen[i,1]<-"SARS-Cov-2"
  data_frame_resumen[11,1]<-"Tor 2 Genoma 2003"
  data_frame_resumen[i,2]<-ids[i]
  data_frame_resumen[i,3]<-tamanios[i]
  data_frame_resumen[i,4]<-contenidogc
 
  cat(" \n ")
  cat(" \n ")
  }
 
  print(data_frame_longitud)
  cat(" \n ")
  print(data_frame_composicion)
  cat(" \n ")
  print(data_frame_gc)
  cat(" \n ")
  print(data_frame_resumen)
  df_longitud <- data_frame_longitud
  #print(composicionPorc)
  #print(data_frame_porcentaje)
  

  crear_graficos_pie(data_frame_porcentaje)
    
  
  
  
  
  
  # Graficar el DataFrame de longitud en barras
  barplot(df_longitud$Longitud_genoma, main = "Longitud de los genomas de SARS-CoV-2",
          xlab = "Pa�ses", ylab = "Longitud", names.arg = rownames(df_longitud))
  abline(h = mean(df_longitud$Longitud_genoma), col = "red", lwd = 2)
  df_gc <- data_frame_gc
  
  # Graficar el contenido GC en barras
  barplot(df_gc$ContenidoGC, main = "Contenido GC de los genomas de SARS-CoV-2",
          xlab = "Pa�ses", ylab = "Contenido GC", names.arg = rownames(df_gc))
  
  # Agregar l�nea de la media
  abline(h = mean(df_gc$ContenidoGC), col = "red", lwd = 2)
  
  idsAcceso <- c("AY274119", "MT318827", "MT450962", "MT459979", "MT416725", "MT072688", "MT459979", "MT412328", "MT192772", "NC_045512")
  analisis_filogenetico(idsAcceso)
  
  
    }




analisis_fasta(fastas)

