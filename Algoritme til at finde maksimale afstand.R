#Algoritmisk metode til at finde maksimal længde: 

#Vi vil gerne have tilfældige retninger på vektoren i kuglerne. 
#Dette vil vi gøre ved at bestemme theta og phi uniform ift hhv (0;pi) og [0;2pi]

#Hvis en kugle i rummet (R^3) så husker vi at kuglens ligning er (x-a)^2+(y-b)^2+(z-c)^2=r^2 
#Hvor centrum af kuglen er i koordinatet (a,b,c). Parameterfremstillingen ville således være: 
#c(a,b,c)+r*c(sin(theta)*cos(phi),sin(phi)*sin(theta),cos(theta))

Kugle_1 <- matrix(NA,n,3)#Tom vektor hvor hver række er koordinaterne til et tilfældigt punkt på periferien af kugle1

Kugle_2 <- matrix(NA,n,3)#Samme som der står ovenfor, bare for kugle 2. 

n <- 100000 #Antallet af punkter vi kigger på. Teoretisk set burde vi nå den sande maksimale afstande for n -> uendelig

#Her laver jeg et loop som skaber de tilfældige punkter på kuglerne 
for(i in 1:n){

thetasample <- runif(1,min = 0,max = pi)
phisammple <- runif(1,min= 0,max=2*pi)

thetasample_2 <- runif(1,min = 0,max = pi)
phisammple_2 <- runif(1,min= 0,max=2*pi)

Kugle_1[i,] <- c(3,-2,2)-3*c(sin(thetasample)*cos(phisammple),sin(phisammple)*sin(thetasample),cos(thetasample))
#Kugle 1 har  -3*(sinus/cosinus vektor) fordi det giver den omvendte retning af kugle 2 cosinus/sinus vektor. 

Kugle_2[i,] <- c(13,8,12)+2*c(sin(thetasample_2)*cos(phisammple_2),sin(phisammple_2)*sin(thetasample_2),cos(thetasample_2))

}
Kuglediff <- Kugle_1-Kugle_2 #Tager differensen af alle de tilfældige punkter

afs <- rep(NA,n)#Tom vektor til længden mellem punkterne i kuglerne

#Loop som udregner normen af vektoren der opstår ved differensen af hvert punkt. 
for(i in 1:n){
  afs[i] <- sqrt(sum((Kuglediff[i,])^2))
}
max(afs) #Tager den maksimale afstand vi får ved at tage tilfældige punkter

sqrt(3)*10+5-max(afs) #Angiver hvor stor vores error 


