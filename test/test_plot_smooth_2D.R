# plot_smooth_2D
source("plot_smooth_2D.R")
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

plot_smooth_2D(FEMObject)

# esempio 
foldername = "imgs/"
if(!dir.exists(foldername)) dir.create(foldername)
coeff_lims = smooth_lim(FEMObject, FEMObject2, FEMObject3)

# named list ...
smooth_list = list(nome1 = FEMObject, nome2 = FEMObject2, nome3 = FEMObject3)
names(smooth_list)

for(i in 1:length(smooth_list)){
  plot_smooth_2D(smooth_list[[i]], coeff_lims = coeff_lims, colorscale = viridis)
  snapshot3d(filename = paste0(foldername, names(smooth_list)[i],".png"),
             fmt = "png", width = 800, height = 750, webshot = rgl.useNULL())
  close3d()  
}

plot_colorbar(FEMObject, coeff_lims = coeff_lims)


