# plot_smooth_2.5D
source("utils.R")
source("plot_smooth_2.5D.R")

data("horseshoe2.5D")
mesh=horseshoe2.5D
FEMbasis <- create.FEM.basis(mesh)
fs.test.time<-function(x,y,t=y) {
  K <- (y/0.1*as.double((abs(y)<=0.1 & x>-0.5))+as.double((abs(y)>0.1 | x<=-0.5)))^2
  
  res=numeric(length =length(x))
  
  for(i in 1:length(x)) {
    if(x[i]>=0 && y[i]>0)
      res[i]=cos(t[i])*(0.25*pi+x[i])+(y[i]-0.5)^2
    
    if(x[i]>=0 && y[i]<=0)
      res[i]=cos(2*t[i])*(-0.25*pi-x[i])+(-y[i]-0.5)^2
    
    if(x[i]<0 && y[i]>0)
      res[i]=cos(t[i])*(-atan(y[i]/x[i])*0.5)+(sqrt(x[i]^2+y[i]^2)-0.5)^2*K[i]
    
    if(x[i]<0 && y[i]<=0)
      res[i]=cos(2*t[i])*(-atan(y[i]/x[i])*0.5)+(sqrt(x[i]^2+y[i]^2)-0.5)^2*K[i]
  }
  res
}

locations = mesh$nodes
nlocs = nrow(locations)
func1 = fs.test.time(x=locations[,1], y=locations[,2],  t=rep(0.5, nlocs))
func2 = fs.test.time(x=locations[,1], y=locations[,2],  t=rep(1, nlocs))
func3 = fs.test.time(x=locations[,1], y=locations[,2],  t=rep(1.5, nlocs))

FEMObject = FEM(func1, FEMbasis)
FEMObject2 = FEM(func2, FEMbasis)
FEMObject3 = FEM(func3, FEMbasis)
smooth_lim(FEMObject)
smooth_lim(FEMObject2)
smooth_lim(FEMObject3)
smooth_lim(FEMObject, FEMObject2, FEMObject3) # TOTALE

plot_smooth_2.5D(FEMObject)
