#1a)
#Generation of 1000 points for dimension from 1 to 15
set.seed(100)
rv1 <- split(runif(1000)*2-1,as.factor(sort(rep(1:1000,1))))
rv2 <- split(runif(1000*2)*2-1,as.factor(sort(rep(1:1000,2))))
rv3 <- split(runif(1000*3)*2-1,as.factor(sort(rep(1:1000,3))))
rv4 <- split(runif(1000*4)*2-1,as.factor(sort(rep(1:1000,4))))
rv5 <- split(runif(1000*5)*2-1,as.factor(sort(rep(1:1000,5))))
rv6 <- split(runif(1000*6)*2-1,as.factor(sort(rep(1:1000,6))))
rv7 <- split(runif(1000*7)*2-1,as.factor(sort(rep(1:1000,7))))
rv8 <- split(runif(1000*8)*2-1,as.factor(sort(rep(1:1000,8))))
rv9 <- split(runif(1000*9)*2-1,as.factor(sort(rep(1:1000,9))))
rv10 <- split(runif(1000*10)*2-1,as.factor(sort(rep(1:1000,10))))
rv11 <- split(runif(1000*11)*2-1,as.factor(sort(rep(1:1000,11))))
rv12 <- split(runif(1000*12)*2-1,as.factor(sort(rep(1:1000,12))))
rv13 <- split(runif(1000*13)*2-1,as.factor(sort(rep(1:1000,13))))
rv14 <- split(runif(1000*14)*2-1,as.factor(sort(rep(1:1000,14))))
rv15 <- split(runif(1000*15)*2-1,as.factor(sort(rep(1:1000,15))))
#Distance calculator function
calc <- function(vec){
  return(sqrt(sum(vec^2)))
}
#Calculator of distance of points to the origin function
dist <- function(vec){
  newarr <- rep(1,length(vec))
  for (i in 1:length(vec)) {
    newarr[i] <- lapply(vec[i], calc)  
  }
  
  return(newarr)
}
#Calculation of the fraction of points that are within distance 1 to the origin
frv1 <- sum(dist(rv1)<1)/1000
frv2 <- sum(dist(rv2)<1)/1000
frv3 <- sum(dist(rv3)<1)/1000
frv4 <- sum(dist(rv4)<1)/1000
frv5 <- sum(dist(rv5)<1)/1000
frv6 <- sum(dist(rv6)<1)/1000
frv7 <- sum(dist(rv7)<1)/1000
frv8 <- sum(dist(rv8)<1)/1000
frv9 <- sum(dist(rv9)<1)/1000
frv10 <- sum(dist(rv10)<1)/1000
frv11 <- sum(dist(rv11)<1)/1000
frv12 <- sum(dist(rv12)<1)/1000
frv13 <- sum(dist(rv13)<1)/1000
frv14 <- sum(dist(rv14)<1)/1000
frv15 <- sum(dist(rv15)<1)/1000
fractionVec <- c(frv1,frv2,frv3,frv4,frv5,frv6,frv7,frv8,frv9,frv10,frv11,frv12,frv13,frv14,frv15)
#Dimesion-Fraction Plot
plot(1:15,fractionVec,xlab = "Dimension", ylab = "Fraction",type = "l")



#1b)
#Use the value of this fraction at D = 2 and D = 3 to get estimates for the value of pi (??) as you know
#the area (for D = 2) and volume (for D = 3) formulae for these cases.
estPi <- 16*fractionVec[2]^3/(9*fractionVec[3]^2)
estPi

#1c)
#Perform the calculations in part (b) with larger sample sizes. You can use the following set: {5000,
#10000, 25000, 50000, 100000}. Visualize the estimated ?? for D = 2 and D = 3 cases. Comment on your results.
rv2_1 <- split(runif(5000*2)*2-1,as.factor(sort(rep(1:5000,2))))
rv3_1 <- split(runif(5000*3)*2-1,as.factor(sort(rep(1:5000,3))))
frv2_1 <- sum(dist(rv2_1)<1)/5000
frv3_1 <- sum(dist(rv3_1)<1)/5000
estPi_1 <- 16*frv2_1^3/(9*frv3_1^2)
estPi_1

rv2_2 <- split(runif(10000*2)*2-1,as.factor(sort(rep(1:10000,2))))
rv3_2 <- split(runif(10000*3)*2-1,as.factor(sort(rep(1:10000,3))))
frv2_2 <- sum(dist(rv2_2)<1)/10000
frv3_2 <- sum(dist(rv3_2)<1)/10000
estPi_2 <- 16*frv2_2^3/(9*frv3_2^2)
estPi_2

rv2_3 <- split(runif(25000*2)*2-1,as.factor(sort(rep(1:25000,2))))
rv3_3 <- split(runif(25000*3)*2-1,as.factor(sort(rep(1:25000,3))))
frv2_3 <- sum(dist(rv2_3)<1)/25000
frv3_3 <- sum(dist(rv3_3)<1)/25000
estPi_3 <- 16*frv2_3^3/(9*frv3_3^2)
estPi_3

rv2_4 <- split(runif(50000*2)*2-1,as.factor(sort(rep(1:50000,2))))
rv3_4 <- split(runif(50000*3)*2-1,as.factor(sort(rep(1:50000,3))))
frv2_4 <- sum(dist(rv2_4)<1)/50000
frv3_4 <- sum(dist(rv3_4)<1)/50000
estPi_4 <- 16*frv2_4^3/(9*frv3_4^2)
estPi_4

rv2_5 <- split(runif(100000*2)*2-1,as.factor(sort(rep(1:100000,2))))
rv3_5 <- split(runif(100000*3)*2-1,as.factor(sort(rep(1:100000,3))))
frv2_5 <- sum(dist(rv2_5)<1)/100000
frv3_5 <- sum(dist(rv3_5)<1)/100000
estPi_5 <- 16*frv2_5^3/(9*frv3_5^2)
estPi_5

piv <- c(estPi_1,estPi_2,estPi_3,estPi_4,estPi_5)
pv <- c(5000, 10000,25000,50000,100000)
plot(pv,piv, type = "l", xlab = "Point Count", ylab = "Estimate of pi")
abline(a = 3.14, b = 0)
#As we see, estimation starts with a high variation around the real pi value, but the variation decreases
#as the sample size increases


#1d)
rv1d <- split(runif(1000)*2-1,as.factor(sort(rep(1:1000,1))))
rv2d<- split(runif(1000*2)*2-1,as.factor(sort(rep(1:1000,2))))
rv3d <- split(runif(1000*3)*2-1,as.factor(sort(rep(1:1000,3))))
rv4d <- split(runif(1000*4)*2-1,as.factor(sort(rep(1:1000,4))))
rv5d <- split(runif(1000*5)*2-1,as.factor(sort(rep(1:1000,5))))
rv6d <- split(runif(1000*6)*2-1,as.factor(sort(rep(1:1000,6))))
rv7d <- split(runif(1000*7)*2-1,as.factor(sort(rep(1:1000,7))))
rv8d <- split(runif(1000*8)*2-1,as.factor(sort(rep(1:1000,8))))
rv9d <- split(runif(1000*9)*2-1,as.factor(sort(rep(1:1000,9))))
rv10d <- split(runif(1000*10)*2-1,as.factor(sort(rep(1:1000,10))))
rv11d <- split(runif(1000*11)*2-1,as.factor(sort(rep(1:1000,11))))
rv12d <- split(runif(1000*12)*2-1,as.factor(sort(rep(1:1000,12))))
rv13d <- split(runif(1000*13)*2-1,as.factor(sort(rep(1:1000,13))))
rv14d <- split(runif(1000*14)*2-1,as.factor(sort(rep(1:1000,14))))
rv15d <- split(runif(1000*15)*2-1,as.factor(sort(rep(1:1000,15))))

rv1t <- split(runif(100)*2-1,as.factor(sort(rep(1:100,1))))
rv2t<- split(runif(100*2)*2-1,as.factor(sort(rep(1:100,2))))
rv3t <- split(runif(100*3)*2-1,as.factor(sort(rep(1:100,3))))
rv4t <- split(runif(100*4)*2-1,as.factor(sort(rep(1:100,4))))
rv5t <- split(runif(100*5)*2-1,as.factor(sort(rep(1:100,5))))
rv6t <- split(runif(100*6)*2-1,as.factor(sort(rep(1:100,6))))
rv7t <- split(runif(100*7)*2-1,as.factor(sort(rep(1:100,7))))
rv8t <- split(runif(100*8)*2-1,as.factor(sort(rep(1:100,8))))
rv9t <- split(runif(100*9)*2-1,as.factor(sort(rep(1:100,9))))
rv10t <- split(runif(100*10)*2-1,as.factor(sort(rep(1:100,10))))
rv11t <- split(runif(100*11)*2-1,as.factor(sort(rep(1:100,11))))
rv12t <- split(runif(100*12)*2-1,as.factor(sort(rep(1:100,12))))
rv13t <- split(runif(100*13)*2-1,as.factor(sort(rep(1:100,13))))
rv14t <- split(runif(100*14)*2-1,as.factor(sort(rep(1:100,14))))
rv15t <- split(runif(100*15)*2-1,as.factor(sort(rep(1:100,15))))

distbet <- function(vec1, vec2){
  sqrt(sum((vec1-vec2)^2))
} 

mindistance <- function(vec1, vec2){
  dist <- c()
  for(i in 1:length(vec1)) {
    for(k in 1:length(vec2)){
      newValue <- distbet(vec1[[i]],vec2[[k]])
      dist <- c(dist, newValue)
    }
  }
  newStr <- split(dist,as.factor(sort(rep(1:1000,100))))
  mindist <- c()
  for(i in 1:length(newStr)){
    newMin <- min(newStr[[i]])  
    mindist <- c(mindist,newMin)
  }
  return(mindist)
}

meand1 <- mean(mindistance(rv1d,rv1t))
meand2 <- mean(mindistance(rv2d,rv2t))
meand3 <- mean(mindistance(rv3d,rv3t))  
meand4 <- mean(mindistance(rv4d,rv4t))
meand5 <- mean(mindistance(rv5d,rv5t))
meand6 <- mean(mindistance(rv6d,rv6t))
meand7 <- mean(mindistance(rv7d,rv7t))
meand8 <- mean(mindistance(rv8d,rv8t))
meand9 <- mean(mindistance(rv9d,rv9t))
meand10 <- mean(mindistance(rv10d,rv10t))
meand11 <- mean(mindistance(rv11d,rv11t))
meand12 <- mean(mindistance(rv12d,rv12t))
meand13 <- mean(mindistance(rv13d,rv13t))
meand14 <- mean(mindistance(rv14d,rv14t))
meand15 <- mean(mindistance(rv15d,rv15t))

meandv <- c(meand1,meand2,meand3,meand4,meand5,meand6,meand7,meand8,meand9,meand10,meand11,meand12,meand13,meand14,meand15)
plot(1:15,meandv,type = "l", xlab = "Dimension", ylab = "Mean Distance")



#2a)
#What is the structure of the variable that stores the image? What is the dimension?
#Display the image.
#package of imager installed.
install.packages("imager")
burak <- load.image('C:/Users/BURAK CAN HELVACI/Desktop/IMG_8751_resized.jpg')
#if(exists("rasterImage")){
#  plot(1:2, type='n')
#  rasterImage(burak,1,1,2,2)
#}
plot(burak)
typeof(burak)
dim(burak)
#Structure of image is 3 matrices with dimesions 512x512, meaning 512x512 RGB. Type of image is double.

#2b)
#Display each channel as separate image.
plot(R(burak))
plot(G(burak))
plot(B(burak))


#2c)
#For each channel, take the average of the columns and plot the average as a line plot for each
#channel on a single plot.
rmean <- colMeans(R(burak))
gmean <- colMeans(G(burak))
bmean <- colMeans(B(burak))

plot(1:512,rmean,type = "l", col="red")
lines(1:512,gmean,type="l",col="green")
lines(1:512,bmean,type="l", col = "blue")

#2d)
#For each channel, subtract one half of the image from the other half (choice of halves is up to you
#but dividing the head image vertically into two parts make more sense). If you observe negative pixel
#values, you can make them equal to zero. Then:
#Display the new image.Display each channel separately as separate image.
#for red channel
rFirst <-R(burak)[1:256,]
rSec <-R(burak)[257:512,]
rSecn <-rSec-rFirst
rSecn[rSecn<0] = 0 #negative pixels become 0
newmatr <- rbind(rFirst,rSecn)
burakr <- image(newmatr,useRaster=TRUE, axes=FALSE)

#for green channel
gFirst <-G(burak)[1:256,]
gSec <-G(burak)[257:512,]
gSecn <-gSec-gFirst
gSecn[gSecn<0] = 0 #negative pixels become 0
newmatg <- rbind(gFirst,gSecn)
burakg <- image(newmatg,useRaster=TRUE, axes=FALSE)

#for blue channel
bFirst <-B(burak)[1:256,]
bSec <-B(burak)[257:512,]
bSecn <-bSec-bFirst
bSecn[bSecn<0] = 0 #negative pixels become 0
newmatb <- rbind(bFirst,bSecn)
burakb <- image(newmatb,useRaster=TRUE, axes=FALSE)


#2e)
#In order to create a noisy image, add a random noise from uniform distribution with minimum value
#of 0 and a maximum value of "0.1 * maximum pixel value observed" to each pixel value for each
#channel of original image.

maxVal <- max(burak)
maxNoise <- 0.1*maxVal
randNoise <- runif(512*512*3,min = 0, max = maxNoise)
noisyImage <- burak + randNoise
plot(R(noisyImage))
plot(G(noisyImage))
plot(B(noisyImage))
