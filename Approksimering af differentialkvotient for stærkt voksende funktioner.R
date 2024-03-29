#Approksimering af differentialkvotient ud fra tidligere data

#Vores approach vil v�re at antage vi har et datas�t af funktionsv�rdier og et tilh�rende datas�t af x-v�rdier.
#Noter at dette kun virker p� strengt voksende funktioner


#Vi genererer dataen s�ledes: 

n <- 10000 #Antal af punkter/data vi har

dataf <- rep(NA,n) #Tom vektor til funktionsv�rdierne 

datax <- rep(NA,n) #Tom vektor til x-v�rdierne 

for(i in 1:n){
  xsample <- runif(1,min=0,max = 3)
  
  datax[i] <- xsample #Datas�t af x-v�rdier
  dataf[i] <- log((xsample)) #Her er vores funktion (f(x)=x^3), men det kunne v�re hvilket som helst funktion, m�ske endag uden forskrift
}


Diffkvotient <- (min(log(3)-dataf)-(n*(min(log(3)-dataf)/(n+1))))/((min(3-datax)-n*(min(3-datax)/(n+1))))#Her tager vi differentialkvotienten i x=3 

Diffkvotient #Vores approksimering

1/3 #Den aktuelle differentialkvotient

