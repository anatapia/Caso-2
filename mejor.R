setwd("C:/Users/analaura/Desktop/Progra III/Calidad de Hospitales")
archivo <- read.csv("outcome-of-care-measures.csv")

mejor <- function(estado,resultado){
    
    #Revisi�n de la validez del estado y resultado
    if (resultado=="ataque"){columna <- 11} else{
        if (resultado=="falla"){columna <- 17} else{
            if (resultado=="neumonia"){columna <- 23} else{ 
                stop("Resultado inv�lido")}
        }
    }
    
    lista <- archivo$State
    j<- sapply(lista,function(x){x!=estado})
    validacion <- all(j)
    
    
    
    if(validacion==FALSE){
        
        #Extracci�n de la tabla omitiando "Not Available"
        newdata<-subset(archivo,archivo[,columna]!="Not Available")
        Mortality<-as.numeric(as.character(newdata[,columna]))
        
        
        #Una tabla que solo arroje el nombre del hospital, el estado y la tasa de mortalidad:
        tab<-data.frame(newdata$Hospital.Name,newdata$State,Mortality)

        #Tabla que muestra el estado elegido, hospital y tasa de mortalidad
        tab1<-subset(tab,newdata$State==estado) 
        
        #Obtener la mejor (m�s baja) tasa de mortalidad
        h<-min(tab1[,3])
        #Estados que tienen la misma y m�s baja tasa de mortalidad ordenados alfabeticamente
        edos<-subset(tab1, Mortality==h)
        x<-sort(edos[,1])
        as.character(x[1]) #estado elegido, el m�s pro
        
    }else{ stop("Estado inv�lido :(")}
    
}
mejor("TX","ataque")
mejor("MD","ataque")
mejor("TX","falla")
mejor("MD","neumonia")
mejor("BB","ataque")
mejor("NY","atakue")
