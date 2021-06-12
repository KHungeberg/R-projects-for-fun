# Algorithm to find the maximal distance between two spheres in space. 

#My method is creating a lot of random vectors within the parameterisation of the two spheres. The spheres are independently at an arbitrary point in R^3. 
# To create these random vectors within the sphere i use random spherical coordinates, letting theta be in (0;pi) og phi in [0;2pi]. 
# The "randomness" of these values will come from samples of a uniform distribution. 

# If a sphere is in space (R^3) we remember their equation: (x-a)^2+(y-b)^2+(z-c)^2=r^2
# Where the center of the sphere is the point (a,b,c). A known parametisation of the spere is then: 
# c(a,b,c)+r*c(sin(theta)*cos(phi),sin(phi)*sin(theta),cos(theta))

Kugle_1 <- matrix(NA,n,3)# Empty matrix where every entrance is a random point on the periphery on sphere 1

Kugle_2 <- matrix(NA,n,3)# Empty matrix where every entrance is a random point on the periphery on sphere 2

n <- 100000 # Number of points 

# Here i create a loop creating the random points. 
for(i in 1:n){

thetasample <- runif(1,min = 0,max = pi)
phisammple <- runif(1,min= 0,max=2*pi)

thetasample_2 <- runif(1,min = 0,max = pi)
phisammple_2 <- runif(1,min= 0,max=2*pi)

Kugle_1[i,] <- c(3,-2,2)+3*c(sin(thetasample)*cos(phisammple),sin(phisammple)*sin(thetasample),cos(thetasample))

Kugle_2[i,] <- c(13,8,12)+2*c(sin(thetasample_2)*cos(phisammple_2),sin(phisammple_2)*sin(thetasample_2),cos(thetasample_2))

}
Kuglediff <- Kugle_1-Kugle_2 # Measures the differense of all the points. 

afs <- rep(NA,n)# Empty vector to calculate the lenght of all the vectors in kuglediff

# Loop calculating the norm of every vector in kuglediff
for(i in 1:n){
  afs[i] <- sqrt(sum((Kuglediff[i,])^2))
}
max(afs) # Taking the biggest distance in afs

sqrt(3)*10+5-max(afs) #Calculating difference between the theoretically correct result and out approximation. 


