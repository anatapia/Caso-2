setwd("C:/Users/analaura/Desktop/Progra III/Calidad de Hospitales")
archivo<-read.csv("outcome-of-care-measures.csv")

rankingcompleto<-function(resultado,num="mejor"){
    #Revisión de la validez del resultado
    if (resultado=="ataque"){
        columna <- 11
    }else if (resultado=="falla"){
        columna <- 17
    } else if (resultado=="neumonia"){
        columna <- 23
    } else{ 
        stop("Resultado inválido")
    }

    
    #Hacemos una lista con todos los estados, sin que se repitan
    est <- archivo$State
    estados <- levels(as.factor(est))
    
    #Dos vectores que vayan almacenando los resultados 
    vector<-c()
    vectorEDOS<-c()
    
    for (estado in estados){
        
        if(num=="mejor"){num<-1}
        
        newdata<-subset(archivo,archivo[,columna]!="Not Available")
        Mortality<-as.numeric(as.character(newdata[,columna]))
        
        #Una tabla que solo arroje el nombre del hospital que va en el 
        #ciclo, el estado y la tasa de mortalidad:
        tab<-data.frame(newdata$Hospital.Name,newdata$State,Mortality)
        
        tab1<-subset(tab,newdata$State==estado)
        
        if(num=="peor"){
            
            x<-as.numeric(as.character(tab1[,3]))
            maximo<-max(na.omit(x))
            
            h<-subset(tab1,tab1[,3]==maximo)
            numdatos<-nrow(h)
            
            v<-sort(h[,1])
            fin<-as.character(v[[numdatos]])
            
        }else{
            
            ordenados<-sort(as.numeric(as.vector(tab1[,3])))
            x<-ordenados[num] 
            
            edos<-subset(tab1, Mortality==x)
            edosOrd<-as.character(as.vector(sort(edos[,1])))
            
            tab1[,3]<-as.numeric(as.vector(tab1[,3]))
            x1<-subset(tab1, Mortality<x)
            nrow(x1)
            resta<-num-nrow(x1)
            fin<-edosOrd[resta]
            
        }
        fin
        vector<-c(vector,fin)
        vectorEDOS<-c(vectorEDOS,estado)
    }
    FINAL<-data.frame(vector,vectorEDOS)
    names(FINAL)<-c("hospital","state")
    FINAL
}
head(rankingcompleto("ataque",20),10)
tail(rankingcompleto("neumonia", "peor"), 3) 
tail(rankingcompleto("falla"), 10) 
