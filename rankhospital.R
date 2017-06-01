setwd("C:/Users/analaura/Desktop/Progra III/Calidad de Hospitales")
archivo<-read.csv("outcome-of-care-measures.csv")

rankhospital <- function(estado,resultado,num="mejor"){
    
    #Revisión de la validez del estado y resultado
    if (resultado=="ataque"){columna <- 11} else{
        if (resultado=="falla"){columna <- 17} else{
            if (resultado=="neumonia"){columna <- 23} else{ 
                stop("Resultado inválido :(")}
        }
    }
    lista <- archivo$State
    j<- sapply(lista,function(x){x!=estado})
    validacion <- all(j)
    
    
    
    if(validacion==FALSE){
        
        
        if(num=="mejor"){num<-1}
        
        #Extracción de la tabla omitiando "Not Available"
        newdata<-subset(archivo,archivo[,columna]!="Not Available")
        Mortality<-as.numeric(as.character(newdata[,columna]))
        
        
        #Una tabla que solo arroje el nombre del hospital, el estado y la tasa de mortalidad:
        tab<-data.frame(newdata$Hospital.Name,newdata$State,Mortality)
        
        #Tabla que muestra el ESTADO ELEGIDO, hospital y tasa de mortalidad
        tab1<-subset(tab,newdata$State==estado)
        
        if(num=="peor"){
            #Obtenemos la peor tasa de mortalidad
            x<-as.numeric(as.character(tab1[,3]))
            maximo<-max(na.omit(x))
            
            #Extraemos los estados que tienen la peor tasa de mortalidad
            h<-subset(tab1,tab1[,3]==maximo)
            
            #Ordenamos los estados alfabeticamente y contamos cuantos son para extraer el último 
            numdatos<-nrow(h)
            v<-sort(h[,1])
            fin<-as.character(v[[numdatos]])
            
            }else{
                if (nrow(data)<num) {
                    fin<-NA
                }else{
                
                #ordenamos las tasas de mortalidad de menor a mayor para extraer la tasa de la
                #posición que elegimos
                ordenados<-sort(as.numeric(as.vector(tab1[,3])))
                x<-ordenados[[num]] 
                
                #Extraemos los estados cuya tasa de mortalidad es igual a la que ocupa x posición
                #MANEJO DE EMPATES  
                edos<-subset(tab1, Mortality==x)
                edosOrd<-as.character(as.vector(sort(edos[,1])))
        
                tab1[,3]<-as.numeric(as.vector(tab1[,3]))
                
                #Extraemos los estados cuya tasa de mortalidad es menor a la elegida y los contamos
                x1<-subset(tab1, Mortality<x)
                nrow(x1)
                
                resta<-num-nrow(x1)
                fin<-edosOrd[[resta]]
            }
        }
        fin #wujuu
        
    }else{ stop("Estado inválido :(")}
}

rankhospital("TX","falla",4)
rankhospital("MD","ataque","peor")
rankhospital("MN","ataque",5000)
rankhospital("MD","ataque","mejor")
rankhospital("MD","ataque",5000)
rankhospital("AZ","atake",5)
rankhospital("ANA","ataque",3)
