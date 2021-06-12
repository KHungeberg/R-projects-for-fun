#Approksimering af bestemte integraler af stærkt voksende funktioner.
#Note, nederst er det Riemann integralet der benyttets. Denne metode virker på alle funktioner kontinuert mellem
#integralets grænser

#Vi starter med nogle tomme vektorer til dataen vi genererer og antallet af datapunkter:

n <- 10000 #Antal datapunkter

datax <- rep(NA,n) #Tom vektor til variabel-værdierne

dataf <- rep(NA,n) #Tom vektor til funktionsværdier

#Vi laver nu et for loop som genererer den data vi gerne vil have i form af at den uniformt tager værdier i integralets
#Nedre grænse og øvre grænse: 


for(i in 1:n){
  xsample <- runif(1,min=3,max=15)#0 er således vores nedre grænse og 5 er den øvre grænse
  datax[i] <- xsample #giver os vores x-værdier
  dataf[i] <- sqrt(xsample)#Giver os de tilsvarende funktionsværdier
}
#Vi vil nu gerne opskrive arealet af trekanterne op. 

dataA <- (max(datax)/2)*dataf #Arealet af alle trekanterne, men ikke sat op fra mindst til størst

#Nu skal vi så udregne det areal som hver trekant har disjunkt fra alle de andre trekanter

#For at få dette, tager vi arealet af hver trekant og trækker arealet af trekanten forans areal. Først må vi dog ordne 
#Vores datasæt så de er i nummerisk rækkefølge

nydatax <- sort(datax) #Sorteret variabel datasæt
nydataf <- sort(dataf) #Sorteret funktionsværdi datasæt
nydataA <- sort(dataA) #Trekanter sorteret fra mindste areal til største BEHØVES IKKE OG GIVER IKKE HELT MENING IFT HVAD VI PRØVER AT FÅ

#Nu udregner vi arealet af den mængde i hver trekant som er disjunkt alle de trekanter som er foran den.

#Skærringspunktet af hver trekant og trekanten foran udregnes ved: 

Skær <- rep(NA,n)

for(i in 1:(n+1)){
  Skær[i] <- abs((nydataf[i+1]*(max(nydatax)-nydatax[i]))-((nydataf[i+1]-nydataf[i])*(max(nydatax)-nydatax[i])+(nydataf[i]*(max(nydatax)-nydatax[i]))))/(max(nydatax)-nydatax[i])
}
#Ok, måske er det ikke så godt når der er så mange punkter og udledningen er måske ikke helt rigtig. 

#Vi kan lige prøve med det originale Riemann integrale metoden.

rek <- rep(NA,(n))
for(i in 1:n){
  rek[i] <- (nydatax[i+1]-nydatax[i])*nydataf[i+1]
}

nyrek <- rek[-n]

Intapp <- sum(nyrek)#Approks med rektangler 

#For at få en approksiering af det samlede areal trods negative arealer, så tager vi bare abs(nyrek) og derefter sum(nyrek)



