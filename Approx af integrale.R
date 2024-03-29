#Approksimering af bestemte integraler af st�rkt voksende funktioner.
#Note, nederst er det Riemann integralet der benyttets. Denne metode virker p� alle funktioner kontinuert mellem
#integralets gr�nser

#Vi starter med nogle tomme vektorer til dataen vi genererer og antallet af datapunkter:

n <- 10000 #Antal datapunkter

datax <- rep(NA,n) #Tom vektor til variabel-v�rdierne

dataf <- rep(NA,n) #Tom vektor til funktionsv�rdier

#Vi laver nu et for loop som genererer den data vi gerne vil have i form af at den uniformt tager v�rdier i integralets
#Nedre gr�nse og �vre gr�nse: 


for(i in 1:n){
  xsample <- runif(1,min=3,max=15)#0 er s�ledes vores nedre gr�nse og 5 er den �vre gr�nse
  datax[i] <- xsample #giver os vores x-v�rdier
  dataf[i] <- sqrt(xsample)#Giver os de tilsvarende funktionsv�rdier
}
#Vi vil nu gerne opskrive arealet af trekanterne op. 

dataA <- (max(datax)/2)*dataf #Arealet af alle trekanterne, men ikke sat op fra mindst til st�rst

#Nu skal vi s� udregne det areal som hver trekant har disjunkt fra alle de andre trekanter

#For at f� dette, tager vi arealet af hver trekant og tr�kker arealet af trekanten forans areal. F�rst m� vi dog ordne 
#Vores datas�t s� de er i nummerisk r�kkef�lge

nydatax <- sort(datax) #Sorteret variabel datas�t
nydataf <- sort(dataf) #Sorteret funktionsv�rdi datas�t
nydataA <- sort(dataA) #Trekanter sorteret fra mindste areal til st�rste BEH�VES IKKE OG GIVER IKKE HELT MENING IFT HVAD VI PR�VER AT F�

#Nu udregner vi arealet af den m�ngde i hver trekant som er disjunkt alle de trekanter som er foran den.

#Sk�rringspunktet af hver trekant og trekanten foran udregnes ved: 

Sk�r <- rep(NA,n)

for(i in 1:(n+1)){
  Sk�r[i] <- abs((nydataf[i+1]*(max(nydatax)-nydatax[i]))-((nydataf[i+1]-nydataf[i])*(max(nydatax)-nydatax[i])+(nydataf[i]*(max(nydatax)-nydatax[i]))))/(max(nydatax)-nydatax[i])
}
#Ok, m�ske er det ikke s� godt n�r der er s� mange punkter og udledningen er m�ske ikke helt rigtig. 

#Vi kan lige pr�ve med det originale Riemann integrale metoden.

rek <- rep(NA,(n))
for(i in 1:n){
  rek[i] <- (nydatax[i+1]-nydatax[i])*nydataf[i+1]
}

nyrek <- rek[-n]

Intapp <- sum(nyrek)#Approks med rektangler 

#For at f� en approksiering af det samlede areal trods negative arealer, s� tager vi bare abs(nyrek) og derefter sum(nyrek)



