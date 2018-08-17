require(fields)

Pmax<-10
Alpha<-0.05
AC<-0.075
LS<-1000
R<-0.3

z<-c(-100:0)

uI.field <- NULL ###creating the underwater light field (it will be filled with values in the next loop)
pp.field <- NULL ######creating the underwater primary production field (it will be filled with values in the next loop)
###take time to understand how the loop is working
#####first time we will do this with a constant k

for(x in 1:length(LS)){
  
  
  uI.vec <- LS[x] #####setting the surface light level 
  pp.vec <- NA ####the NA is a place holder for the surface
  for(y in 2:length(z)){
    
    k=0.075 #####attenuation coefficient. Unit is m^-1
    pmax = 10 ####units are mgC/hr
    Ek = 50 ####units are W/m2
    R = 0.3 ###units are mgC/hr
    uI <- uI.vec[y-1]*exp(k*-1)######Lambert-Beer Law solved at every depth (delta z = -1) ##units are W/m2
    uI.vec <- c(uI.vec, uI)#####building the underwater light field with the results of the lambert-Beer Law
    
    pp <- pmax*tanh(Alpha*uI/Pmax)-R ###units are mgC/hr
    pp.vec <- c(pp.vec, pp)
    
  }###end y
  
  uI.field <- rbind(uI.field, rev(uI.vec))
  pp.field <- rbind(pp.field, rev(pp.vec))
  
}#####end x

uI.vec
pp.vec
