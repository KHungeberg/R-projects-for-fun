#Approksimering af differentialkvotient ud fra tidligere data

#Vores approach vil være at antage vi har et datasæt af funktionsværdier og et tilhørende datasæt af x-værdier.
#Noter at dette kun virker på strengt voksende funktioner


#Vi genererer dataen således: 

n <- 10000 #Antal af punkter/data vi har

dataf <- rep(NA,n) #Tom vektor til funktionsværdierne 

datax <- rep(NA,n) #Tom vektor til x-værdierne 

for(i in 1:n){
  xsample <- runif(1,min=0,max = 3)
  
  datax[i] <- xsample #Datasæt af x-værdier
  dataf[i] <- log((xsample)) #Her er vores funktion (f(x)=x^3), men det kunne være hvilket som helst funktion, måske endag uden forskrift
}


Diffkvotient <- (min(log(3)-dataf)-(n*(min(log(3)-dataf)/(n+1))))/((min(3-datax)-n*(min(3-datax)/(n+1))))#Her tager vi differentialkvotienten i x=3 

Diffkvotient #Vores approksimering

1/3 #Den aktuelle differentialkvotient

