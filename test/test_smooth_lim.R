source("utils.R")
# 2d ---------------------------------------------------------------------------
data(horseshoe2D)
mesh=create.mesh.2D(nodes=horseshoe2D$boundary_nodes, 
                    segments = horseshoe2D$boundary_segments)
mesh = refine.mesh.2D(mesh, maximum_area = 0.025, minimum_angle = 30)
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

extract_coeff(FEMObject)
# 2.5d -------------------------------------------------------------------------

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

plot.FEM.2.5D(FEMObject)
# 3d ---------------------------------------------------------------------------

data("sphere3Ddata")
mesh=create.mesh.3D(sphere3Ddata$nodes, sphere3Ddata$tetrahedrons)
names(mesh)
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
plot(FEMObject)
smooth_lim(FEMObject)
smooth_lim(FEMObject2)
smooth_lim(FEMObject3)
smooth_lim(FEMObject, FEMObject2, FEMObject3) # TOTALE

# 1.5d -------------------------------------------------------------------------

eps = 1 / 2
x = c(0., 1)
y = c(0.,eps)
vertices = expand.grid(x,y)
vertices = cbind(vertices[,1], vertices[,2])
edges = matrix(c(1,2,1,3,3,4), nrow=3,ncol=2, byrow=T)

mesh = fdaPDE::create.mesh.1.5D(vertices, edges)
mesh = fdaPDE::refine.mesh.1.5D(mesh,delta=0.0125)

nnodes=dim(mesh$nodes)[1]
FEMbasis=fdaPDE::create.FEM.basis(mesh)

# Exact solution (pointwise at nodes)
aux.4 = function(x,y){
  h = 1
  source = 4 
  points_ = cbind(x,y)
  idx.ok = which( abs(points_[,2]-mesh$nodes[source,2]) < 10 * .Machine$double.eps )
  coef = vector(mode="numeric", length=length(x))
  
  for(i in idx.ok){
    delta = abs(points_[i,1] - mesh$nodes[source,1])
    if(delta < h ){
      coef[i] = 3*exp(1/(delta^2 / h^2 -1)+1 ) - 1
      
    }
  }
  
  return(coef)
}
aux.3 = function(x,y){
  
  h = eps
  source = 3 
  points_ = cbind(x,y)
  idx.ok = which( abs(points_[,1]-mesh$nodes[source,1]) < 10 * .Machine$double.eps )
  coef = vector(mode="numeric", length=length(x))
  
  for(i in idx.ok){
    delta = abs(points_[i,2] - mesh$nodes[source,2])
    if(delta < h ){
      coef[i] = -1 - 1/h*delta
    }
    
    
  }
  return(coef)
}
aux.1 = function(x,y){
  
  h = 1
  source = 1 
  points_ = cbind(x,y)
  idx.ok = which( abs(points_[,2]-mesh$nodes[source,2]) < 10 * .Machine$double.eps )
  coef = vector(mode="numeric", length=length(x))
  
  for(i in idx.ok){
    delta = abs(points_[i,1] - mesh$nodes[source,1])
    if(delta <= h ){
      coef[i] = -2 - 1/h*delta
    }
  }
  return(coef)
  
}

f= function(x,y){
  
  res = aux.1(x,y) + aux.3(x,y) + aux.4(x,y)
  return(res)
}

coeff = f(mesh$nodes[,1], mesh$nodes[,2])
FEMObject = fdaPDE::FEM(coeff, FEMbasis)
FEMObject2 = fdaPDE::FEM(-coeff, FEMbasis)
FEMObject3 = fdaPDE::FEM(2*coeff, FEMbasis)

smooth_lim(FEMObject, FEMObject2, FEMObject3)

plot_colorbar(FEMObject)
plot_colorbar(FEMObject2, colorscale = viridis, ncolor = 256)
