####################################################################################################
# FAILURE DETECTION
####################################################################################################
# function that returns TRUE if your attempted operation results in an error
# @x whichever object you pick that is a result of try(something(x))

fail <- function(x) inherits(x, "try-error")

####################################################################################################
# READ IN SPSS
####################################################################################################
# read in an SPSS file and return a list that has data, variable and value labels nicely prepared for analysis
# @filename = directory+name of the file you want to read in such as "Z:/kvoty/data_01.sav"

read_in_spss <- function(filename=NULL){
  
  if(is.null(filename)){
    stop("Select a path to the file!")
    return(NULL)
  }else{
    if(file.exists(filename)){
      
      #get list object with foreign::read.spss
      data_object <- read.spss(paste(filename,sep=""), use.value.labels = T, use.missings = T)
      data<-as.data.frame(data_object)
      #get data
      
      #get variable labels
      varlabels <- attr(data_object,"variable.labels")

      #get value labels
      vallabels <- attr(data_object,"label.table")   
      
      #data_list that returns everything at once
      datalist <-list(data_object=data_object,data=data,varlabels=varlabels,vallabels=vallabels)
      
      return(datalist)
      
    }else{ 
      stop("File not found or not able to load")
      return(NULL)
    }

  }

}

####################################################################################################
# Wrapper
####################################################################################################
# Odřádkuje dlouhý string
# @ width vhodný parametr po kolika znacích zalomit

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}



####################################################################################################
# Plot SM
####################################################################################################
# Grafovaci funkce
# Parametry
# @ Xdata 
# @ Typ - urcujici typ grafu 1-pruhovy, 2-pruhovy trideny, 3 kolac atd..
# @ Otazka - zobrazovana promenna zadava se bud jednoduse "sex" nebo "c("sex","edu")
# @ Trideni - tridici promenna
# @ Labels - zobrazuj popisky dat , default = TRUE
# @ Lablim - nezobrazuj popisky mensi nez , default = -Inf (zobrazuje vse)
# @ Decim - ukaz popisky zaokrouhlenne na pocet desetin , default = 0
# @ Barva - barevná skla

plot_sm<- function(xdata=dataf, typ, otazka,trideni,trideni2,id, labels = T, lablim = -Inf, decim = 0,barva = 0,varlabels){

if(!is.null(xdata)){
	
if(nrow(xdata)>0){  
  
  xdata$otaz<-xdata[,c(otazka)]
  
  	paleta = "Paired"
	if (barva ==0) {paleta = "Set3"} 
	if (barva ==1) {paleta = "RdBu"}
	if (barva ==2) {paleta = "Pastel1"}
	if (barva ==3) {paleta = "Accent"}
  
	if (typ ==1){
		grafdata <- aggregate(xdata$otaz ,by = list(xdata$otaz),FUN = length)
		colnames(grafdata)<-c("Var","Freq")
		grafdata$Freq <- grafdata$Freq * 100 / sum(grafdata$Freq)
		grafdata$Freqpos <- grafdata$Freq / 2
		grafdata$Lab <- round(x=grafdata$Freq, digits=decim)
		grafdata$Lab[grafdata$Freq<lablim] <- ""
		grafdata$Var2 <- sapply(grafdata$Var, function(x) wrapper(toString(x), width = 17))
    
		p <- ggplot(grafdata,aes(x=Var2), fill=Var2) + geom_bar(aes(y=Freq, fill = Var2), stat = "identity" ) 
		p <- p + theme(legend.position= "none") + xlab("")+ylab("")
		p <- p + theme(axis.text = element_text(size = rel(1.5)))
		p <- p + theme(panel.background = element_blank())
		p <- p + scale_fill_brewer( type = "div" , palette = paleta )
    		if (labels==T) {p <- p + geom_text(aes(y=Freqpos, label=Lab)) }
	}
  else if (typ ==2){
		xdata$trid<-xdata[,c(trideni)]
		grafdata <- aggregate(xdata$otaz ,by = list(xdata$trid, xdata$otaz),FUN = length)
		datasum <- aggregate(xdata$otaz,by = list(xdata$trid),FUN=length)
		grafdata <- merge(grafdata,datasum, by="Group.1")
		colnames(grafdata) <- c("Cro","Var","Freq","Sum")
		grafdata <- grafdata[order(grafdata$Cro,grafdata$Var),]
		grafdata$Freq <- grafdata$Freq *100/ grafdata$Sum 
		grafdata$Freqpos <- grafdata$Freq / 2
		grafdata$Lab <- round(x=grafdata$Freq, digits=decim)
		grafdata$Lab[grafdata$Freq<lablim] <- ""
		  
		p <- ggplot(grafdata, aes(Var, fill=Cro)) + geom_bar(aes(y = Freq),stat= "identity") + facet_grid(. ~ Cro) + coord_flip() 
		p <- p + theme(legend.position= "none")+ xlab("")+ylab("")
		p <- p + theme(strip.text = element_text(size = rel(1.5)),axis.text = element_text(size = rel(1.5)))
		p <- p + theme(panel.background = element_blank())
		p <- p + scale_fill_brewer( type = "div" , palette = paleta )
		if (labels==T) {p <- p + geom_text(aes(y=Freqpos, label=Lab)) }
	}
	else if (typ ==3){
		grafdata <- aggregate(xdata$otaz ,by = list(xdata$otaz),FUN = length)
		colnames(grafdata)<-c("Var","Freq")
		grafdata$Freq <- grafdata$Freq *100/ sum(grafdata$Freq )
		grafdata$Freqpos <- cumsum(grafdata$Freq) - 0.5 * grafdata$Freq
		grafdata$Lab <- round(grafdata$Freq, digits=decim)
		grafdata$Lab[grafdata$Freq<lablim] <- ""
		
		p <- ggplot(grafdata, aes(x =factor(1),y = Freq, fill = Var) ) +  geom_bar(stat="identity",width=1) + coord_polar(theta = "y") +  xlab("")+ ylab("")
		p <- p + theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),legend.title = element_blank()) 
		p <- p + theme(legend.text = element_text(size = rel(1.5)))
		p <- p + theme(panel.background = element_blank())
		p <- p + scale_fill_brewer( type = "div" , palette = paleta )
		if (labels==T) {p <- p + geom_text(aes(y=Freqpos,label=Lab) ) }
	}
  else if (typ ==4){
		xdata$trid<-xdata[,c(trideni)]
		grafdata <- aggregate(xdata$otaz ,by = list(xdata$trid, xdata$otaz),FUN = length)
		
		datasum <- aggregate(grafdata$x,by = list(grafdata$Group.1),FUN=sum) 
    grafdata <- merge(grafdata,datasum, by.x=c("Group.1"), by.y=c("Group.1"))
		
    colnames(grafdata) <- c("Cro","Var","Freq","Sum")
		grafdata <- grafdata[order(grafdata$Cro,grafdata$Var),]
		grafdata$Freq <- grafdata$Freq *100/ grafdata$Sum 
		grafdata$Freqpos <- (cumsum(grafdata$Freq) - 0.5 * grafdata$Freq) %% 100
		grafdata$Lab <- round(x=grafdata$Freq, digits=decim)
		grafdata$Lab[grafdata$Freq<lablim] <- ""
    
		p <- ggplot(grafdata, aes(x = factor(Cro), fill=factor(Var))) + geom_bar(aes(y = Freq),stat= "identity") + coord_flip() 
		p <- p + theme(axis.text = element_text(size = rel(1.5)))
		p <- p + theme(legend.title = element_blank()) + xlab("")+ylab("") 
		p <- p + theme(legend.text = element_text(size = rel(1.5)))		
		p <- p + theme(panel.background = element_blank())
		p <- p + scale_fill_brewer( type = "div" , palette = paleta )
	 	if (labels==T) {p <- p + geom_text(aes(y=Freqpos,label=Lab) ) }
	}
	else if (typ ==5){ 
    
		datamin <- xdata[,c(id,otazka)]
		datamelt <- melt(datamin,id=1)
		grafdata <- aggregate(datamelt[,1] , by =list(datamelt$variable,datamelt$value), FUN = length)
    
    datasum <- aggregate(grafdata$x, by= list(grafdata$Group.1),FUN = sum)
		grafdata <- merge(grafdata,datasum, by="Group.1")
		colnames(grafdata) <- c("Cro","Var","Freq","Sum")
		
		grafdata <- grafdata[order(grafdata$Cro,grafdata$Var),]
		
    grafdata$cs <- cumsum(grafdata$Freq)
    grafdata$Freq <- grafdata$Freq *100/ grafdata$Sum 
		grafdata$Freqpos <- (cumsum(grafdata$Freq) - 0.5 * grafdata$Freq) %%100
		grafdata$Lab <- round(x=grafdata$Freq, digits=decim)
		grafdata$Lab[grafdata$Freq<lablim] <- ""
  
    grafdata$Labs <- sapply(grafdata$Cro, function(x) wrapper(varlabels[toString(x)], width = 40))
    grafdata$Labs2 <- factor(grafdata$Labs, as.character(unique(grafdata$Labs)))
		grafdata$Labs2 <- factor(grafdata$Labs2, levels = rev(levels(grafdata$Labs2)))
	  
      
		p <- ggplot(grafdata, aes(x = Labs2, fill=Var)) + geom_bar(aes(y = Freq),stat= "identity") + coord_flip() 
    p <- p + theme(legend.title = element_blank()) + xlab("")+ylab("") 
		p <- p + theme(axis.text = element_text(size = rel(1.5)))
		p <- p + theme(panel.background = element_blank())
		p <- p + scale_fill_brewer( type = "div" , palette = paleta )
		p <- p + theme(legend.text = element_text(size = rel(1.5)))  
		if (labels==T) {p <- p + geom_text(aes(y=Freqpos,label=Lab) ) }
	}
  else if (typ ==6){
    
    xdata$trid<-xdata[,c(trideni)]
    grafdata <- aggregate(xdata$otaz[!is.na(xdata$otaz)] ,by = list(xdata$trid[!is.na(xdata$otaz)]),FUN = mean)
    
    colnames(grafdata)<-c("Cro","Freq")
    grafdata$Freq 
    grafdata$Freqpos <- grafdata$Freq / 2
    grafdata$Lab <- round(x=grafdata$Freq, digits=decim)
    grafdata$Lab[grafdata$Freq<lablim] <- ""
    
    p <- ggplot(grafdata,aes(x=Cro), fill="grey") + geom_bar(aes(y=Freq, fill = "grey"), stat = "identity" ) + coord_flip()
    p <- p + theme(legend.position= "none") + xlab("")+ylab("")
    p <- p + theme(axis.text = element_text(size = rel(1.5)))
    p <- p + theme(panel.background = element_blank())
    #p <- p + scale_fill_brewer( type = "div" , palette = paleta )
    if (labels==T) {p <- p + geom_text(aes(y=Freqpos, label=Lab)) }
  }
  else if (typ ==42){
	  xdata$trid<-xdata[,c(trideni)]
	  xdata$trid2<-xdata[,c(trideni2)]
    grafdata <- aggregate(xdata$otaz ,by = list(xdata$trid,xdata$trid2, xdata$otaz),FUN = length)
	  
	  datasum <- aggregate(grafdata$x,by = list(grafdata$Group.1,grafdata$Group.2),FUN=sum) 
    
  
	  grafdata <- merge(grafdata,datasum, by.x=c("Group.1","Group.2"), by.y=c("Group.1", "Group.2"))
	  
    
	  colnames(grafdata) <- c("Cro","Cro2","Var","Freq","Sum")
	  
    
    grafdata <- grafdata[order(grafdata$Cro,grafdata$Cro2,grafdata$Var),]
	  grafdata$Freq <- grafdata$Freq *100/ grafdata$Sum 
	  grafdata$Freqpos <- (cumsum(grafdata$Freq) - 0.5 * grafdata$Freq) %% 100
	  grafdata$Lab <- round(x=grafdata$Freq, digits=decim)
	  grafdata$Lab[grafdata$Freq<lablim] <- ""
	  
	  p <- ggplot(grafdata, aes(x = factor(Cro), fill=factor(Var))) + geom_bar(aes(y = Freq),stat= "identity") + coord_flip() 
	  p <- p + facet_grid(Cro2 ~ .)
    p <- p + theme(axis.text = element_text(size = rel(1)))
	  p <- p + theme(legend.title = element_blank()) + xlab("")+ylab("") 
	  p <- p + theme(legend.text = element_text(size = rel(1.25)))		
	  p <- p + theme(panel.background = element_blank())
	  p <- p + theme(strip.text = element_text(size = rel(1.25)))
	  p <- p + scale_fill_brewer( type = "div" , palette = paleta )
	  if (labels==T) {p <- p + geom_text(aes(y=Freqpos,label=Lab) ) }
	}
  else if (typ ==52){ 
    
    datamin <- xdata[,c(id,otazka, trideni)]
    datamelt <- melt(datamin,id=c(id,trideni))
    datamelt$val2 <- as.numeric(datamelt$value)
    datamelt <- datamelt[!is.na(datamelt$val2),]
    
     grafdata <- aggregate(datamelt$val2 , by =list(datamelt$variable,datamelt[,2]), FUN = mean)
    
    #datasum <- aggregate(grafdata$x, by= list(grafdata$Group.1),FUN = sum)
    #grafdata <- merge(grafdata,datasum, by="Group.1")
    
    colnames(grafdata) <- c("Var","Cro","Freq")
    
    grafdata <- grafdata[order(grafdata$Cro,grafdata$Var),]
    
    #grafdata$cs <- cumsum(grafdata$Freq)
    #grafdata$Freq <- grafdata$Freq *100/ grafdata$Sum 
    #grafdata$Freqpos <- (cumsum(grafdata$Freq) - 0.5 * grafdata$Freq) %%100
    grafdata$Lab <- round(x=grafdata$Freq, digits=decim)
    grafdata$Lab[grafdata$Freq<lablim] <- ""
    
    grafdata$Labs <- sapply(grafdata$Var, function(x) wrapper(varlabels[toString(x)], width = 40))
    
    grafdata$Labs2 <- factor(grafdata$Labs, as.character(unique(grafdata$Labs)))
    grafdata$Labs2 <- factor(grafdata$Labs2, levels = rev(levels(grafdata$Labs2)))

    p <- ggplot(grafdata, aes(x = Labs2,y = Freq, group=Cro))  + coord_flip() 
    p <- p + geom_point(aes(colour= factor(Cro), size = 25 ),stat= "identity") + geom_path(aes(colour=factor(Cro)))
    p <- p + guides(size=FALSE)
    p <- p + theme(legend.title = element_blank()) + xlab("")+ylab("") 
    p <- p + theme(axis.text = element_text(size = rel(1.5)))
    p <- p + theme(panel.background = element_blank())
    p <- p + scale_fill_brewer( type = "div" , palette = paleta )
    p <- p + theme(legend.text = element_text(size = rel(1.5)))  
    #if (labels==T) {p <- p + geom_text(aes(y=Freq,label=Lab) ) }
  }
  else if (typ ==62){
    
    xdata$trid<-xdata[,c(trideni)]
    xdata$trid2<-xdata[,c(trideni2)]
    grafdata <- aggregate(xdata$otaz[!is.na(xdata$otaz)] ,by = list(xdata$trid[!is.na(xdata$otaz)],xdata$trid2[!is.na(xdata$otaz)]),FUN = mean)
    
    colnames(grafdata)<-c("Cro","Cro2","Freq")
    grafdata$Freq 
    grafdata$Freqpos <- grafdata$Freq / 2
    grafdata$Lab <- round(x=grafdata$Freq, digits=decim)
    grafdata$Lab[grafdata$Freq<lablim] <- ""
    
    p <- ggplot(grafdata,aes(x=Cro), fill="grey") + geom_bar(aes(y=Freq, fill = "grey"), stat = "identity" ) + coord_flip()
    p <- p + facet_grid(Cro2 ~ .)
    p <- p + theme(legend.position= "none") + xlab("")+ylab("")
    p <- p + theme(axis.text = element_text(size = rel(1.5)))
    p <- p + theme(panel.background = element_blank())
    #p <- p + scale_fill_brewer( type = "div" , palette = paleta )
    if (labels==T) {p <- p + geom_text(aes(y=Freqpos, label=Lab)) }
  }
  
} else { p <- NULL }  
  
} else { p <- NULL }
  
p

}


####################################################################################################
# Tab SM
####################################################################################################
# Tabulkovací funkce
# Parametry
# @ Xdata 
# @ Typ - urcujici typ tabulky
# @ Otazka - zobrazovana promenna zadava se bud jednoduse "sex" nebo "c("sex","edu")
# @ Trideni - tridici promenna
# @ Decim - ukaz popisky zaokrouhlenne na pocet desetin , default = 0


tab_sm<- function(xdata=dataf, typ, otazka,trideni,trideni2,id, decim = 0,barva = 0,varlabels){
  
  if(!is.null(xdata)){
    
    if(nrow(xdata)>0){  
      
      xdata$otaz<-xdata[,c(otazka)]
  
      if (typ ==1 | typ==3){
        grafdata <- aggregate(xdata$otaz ,by = list(xdata$otaz),FUN = length)
        
        colnames(grafdata)<-c("Var","Freq")
        print(grafdata)
        
        grafdata$n <- grafdata$Freq
        grafdata$Freq <- grafdata$Freq * 100 / sum(grafdata$Freq)
        grafdata$Freqpos <- grafdata$Freq / 2
        grafdata$Lab <- round(x=grafdata$Freq, digits=decim)
        
        p <- grafdata[c("Var","Freq","n")]
        colnames(p)<-c("Varianta","Podíl", "n=")
        
      }
      else if (typ ==2 | typ==4){
        xdata$trid<-xdata[,c(trideni)]
        grafdata <- aggregate(xdata$otaz ,by = list(xdata$trid, xdata$otaz),FUN = length)
        datasum <- aggregate(xdata$otaz,by = list(xdata$trid),FUN=length)
        grafdata <- merge(grafdata,datasum, by="Group.1")
        colnames(grafdata) <- c("Cro","Var","Freq","Sum")
        grafdata <- grafdata[order(grafdata$Cro,grafdata$Var),]
        grafdata$n <- grafdata$Freq
        grafdata$Freq <- grafdata$Freq *100/ grafdata$Sum 
        grafdata$Lab <- round(x=grafdata$Freq, digits=decim)

        p <- grafdata[c("Cro","Var","Lab","n")]
        colnames(p)<-c("Třídění","Varianta","Podíl","n")
      }
      else if (typ ==5){ 
        datamin <- xdata[,c(id,otazka)]
        datamelt <- melt(datamin,id=1)
        grafdata <- aggregate(datamelt[,1] , by =list(datamelt$variable,datamelt$value), FUN = length)
        
        datasum <- aggregate(grafdata$x, by= list(grafdata$Group.1),FUN = sum)
        grafdata <- merge(grafdata,datasum, by="Group.1")
        colnames(grafdata) <- c("Cro","Var","Freq","Sum")
        
        grafdata <- grafdata[order(grafdata$Cro,grafdata$Var),]
        
        grafdata$cs <- cumsum(grafdata$Freq)
        grafdata$Freq <- grafdata$Freq *100/ grafdata$Sum 
        grafdata$Freqpos <- (cumsum(grafdata$Freq) - 0.5 * grafdata$Freq) %%100
        grafdata$Lab <- round(x=grafdata$Freq, digits=decim)
        
        grafdata$Labs <- sapply(grafdata$Cro, function(x) wrapper(varlabels[toString(x)], width = 40))
        
        p <- grafdata[c("Labs","Var","Lab")]
        colnames(p)<-c("Subotázka","Varianta","Podíl")
      }
      else if (typ ==6){
        
        xdata$trid<-xdata[,c(trideni)]
        grafdata <- aggregate(xdata$otaz[!is.na(xdata$otaz)] ,by = list(xdata$trid[!is.na(xdata$otaz)]),FUN = mean)
      

        
        colnames(grafdata)<-c("Cro","Freq")
        grafdata$Lab <- round(x=grafdata$Freq, digits=decim)
        
        p <- grafdata[c("Cro","Lab")]
        colnames(p) <- c("Třídění","Průměr")
        
      }
      else if (typ ==42){
        xdata$trid<-xdata[,c(trideni)]
        xdata$trid2<-xdata[,c(trideni2)]
        grafdata <- aggregate(xdata$otaz ,by = list(xdata$trid,xdata$trid2, xdata$otaz),FUN = length)
        
        datasum <- aggregate(grafdata$x,by = list(grafdata$Group.1,grafdata$Group.2),FUN=sum) 
        
        grafdata <- merge(grafdata,datasum, by.x=c("Group.1","Group.2"), by.y=c("Group.1", "Group.2"))
        
        
        colnames(grafdata) <- c("Cro","Cro2","Var","Freq","Sum")
        
        grafdata <- grafdata[order(grafdata$Cro,grafdata$Cro2,grafdata$Var),]
        grafdata$Freq <- grafdata$Freq *100/ grafdata$Sum 
        grafdata$Freqpos <- (cumsum(grafdata$Freq) - 0.5 * grafdata$Freq) %% 100
        grafdata$Lab <- round(x=grafdata$Freq, digits=decim)
        
        p <- grafdata[c("Cro","Cro2","Var","Freq")]
        colnames(p)<-c("Třídění 1","Třídění 2","Varianta","Podíl")
      }
      else if (typ ==52){ 
        
        datamin <- xdata[,c(id,otazka, trideni)]
        datamelt <- melt(datamin,id=c(id,trideni))
        datamelt$val2 <- as.numeric(datamelt$value)
        datamelt <- datamelt[!is.na(datamelt$val2),]
        
        grafdata <- aggregate(datamelt$val2 , by =list(datamelt$variable,datamelt[,2]), FUN = mean)
      
        colnames(grafdata) <- c("Var","Cro","Freq")
        
        grafdata <- grafdata[order(grafdata$Cro,grafdata$Var),]

        grafdata$Lab <- round(x=grafdata$Freq, digits=decim)
        
        grafdata$Labs <- sapply(grafdata$Var, function(x) wrapper(varlabels[toString(x)], width = 40))
        
        grafdata$Labs2 <- factor(grafdata$Labs, as.character(unique(grafdata$Labs)))
        grafdata$Labs2 <- factor(grafdata$Labs2, levels = rev(levels(grafdata$Labs2)))
        
        
        p<- grafdata[c("Labs2","Cro","Lab")]
        colnames(p) <- c("Proměnná","Třídění","Průměr")

        #if (labels==T) {p <- p + geom_text(aes(y=Freq,label=Lab) ) }
      }
      else if (typ ==62){
      
      xdata$trid<-xdata[,c(trideni)]
      xdata$trid2<-xdata[,c(trideni2)]
      grafdata <- aggregate(xdata$otaz[!is.na(xdata$otaz)] ,by = list(xdata$trid[!is.na(xdata$otaz)],xdata$trid2[!is.na(xdata$otaz)]),FUN = mean)
      
      colnames(grafdata)<-c("Cro","Cro2","Freq")

      grafdata$Freqpos <- grafdata$Freq / 2
      grafdata$Lab <- round(x=grafdata$Freq, digits=decim)
      
      p <- grafdata[c("Cro","Cro2","Freq")]
      colnames(p) <- c("Třídění 1","Třídění 2","Průměr")
            
      }
    
    } else { p <- NULL }  
    
  } else { p <- NULL }
  
  p
  
}

####################################################################################################
# Podklad
####################################################################################################
# Funkce ktera z csv nacte ktere promenne se budou zobrazovat (variables), ktere jsou sociodemografika (socio) a
# ktere budou pouzity jako tridici
# Parametry
# @filename cesta k souboru csv


podklad <- function(filename=NULL)
{

if(file.exists(filename)) {
  podklad <- read.csv(filename,sep = ";", header= T,stringsAsFactors=F)

} else {
  stop("Add Podklad.csv file to the directory") 
}

vgroup <- podklad[!is.na(podklad$GROUP),c("ID","NAME","LABEL","GROUP")]
vsocio  <- podklad[!is.na(podklad$SOCIO),c("ID","NAME","LABEL","SOCIO")]
vcross  <- podklad[!is.na(podklad$CROSS),c("ID","NAME","LABEL","CROSS")]

vars1 <- c(1:max(vgroup$GROUP))
vars2 <- c(1:max(vsocio$SOCIO))
vars3 <- c(1:max(vcross$CROSS))
vars4 <- c(1:nrow(vgroup))


variablesAll <- vgroup$NAME
names(variablesAll) <- vgroup$LABEL

variables <- as.list(lapply(vars1, function(x) vgroup$NAME[vgroup$GROUP==x][1]))
names(variables) <- lapply(vars1, function(x) vgroup$LABEL[vgroup$GROUP==x][1])

variablesG <- as.list(lapply(vars1, function(x) vgroup$NAME[vgroup$GROUP==x]))
names(variablesG) <- lapply(vars1, function(x) vgroup$LABEL[vgroup$GROUP==x][1])


socio <- as.list(lapply(vars2, function(x) vsocio$NAME[vsocio$SOCIO==x]))
names(socio) <- lapply(vars2, function(x) vsocio$LABEL[vsocio$SOCIO==x][1])

cross <- as.list(lapply(vars3, function(x) vcross$NAME[vcross$CROSS==x]))
names(cross) <- lapply(vars3, function(x) vcross$LABEL[vcross$CROSS==x][1])

vars<- list(variables=variables,socio=socio, cross=cross, variablesAll = variablesAll,variablesG=variablesG)

return(vars)
}

####################################################################################################
# Prom
####################################################################################################
# Vyrobi list ze struktury labelu
# @ vallabels - tabulka labelu
# @ prom - promenna


val2list<-function(prom){
  s <- as.list(laply(prom,function(x) {
    y<-as.character(x)
    names(y)<-names(x) 
    return(y)  
  }))
  return(s)
}

