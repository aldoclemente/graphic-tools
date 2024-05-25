# plot smooth time -------------------------------------------------------------
source("plot_smooth_2D.R")

foldername = "imgs/"
if(!dir.exists(foldername)) dir.create(foldername)

data(horseshoe2D)

mesh = create.mesh.2D(nodes=horseshoe2D$boundary_nodes, segments = horseshoe2D$boundary_segments)
mesh = refine.mesh.2D(mesh, maximum_area = 0.025, minimum_angle = 30)
locations = mesh$nodes
FEMbasis = create.FEM.basis(mesh)

f<-function(x,y,t)
{
  K <- (y/0.1*as.double((abs(y)<=0.1 & x>-0.5))+as.double((abs(y)>0.1 | x<=-0.5)))^2
  res=numeric(length =length(x))
  for(i in 1:length(x))
  {
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

n_time=10
mesh_time=seq(0,pi,length.out =n_time)

space_time_locations = cbind(rep(mesh_time,each=nrow(locations)),rep(locations[,1],n_time),rep(locations[,2],n_time))
sol_exact = f(space_time_locations[,2],space_time_locations[,3],space_time_locations[,1])
coeff = matrix(sol_exact, nrow=FEMbasis$nbasis, ncol=n_time)

FEMtimeObject =list()
for( t in 1:n_time){
  FEMtimeObject[[t]] = fdaPDE::FEM(coeff[,t], FEMbasis)
}

smooth_lim(unlist(FEMtimeObject, recursive = FALSE)) # :)

# Più funzioni "spazio-tempo" (f vera + stima di f, per esempio)
FEMtimeObject1 =list()
for( t in 1:n_time){
  FEMtimeObject1[[t]] = fdaPDE::FEM(-coeff[,t], FEMbasis)
}

smooth_lim(unlist(FEMtimeObject1, recursive = FALSE))
smooth_lim(unlist(FEMtimeObject, recursive = FALSE))

coeff_lims = smooth_lim(unlist(append(FEMtimeObject, 
                                      FEMtimeObject1), recursive = FALSE)) 

coeff_lims # :)

# ---

foldername = paste0(foldername, "test_plot_smooth_time_2D/")
if(!dir.exists(foldername)) dir.create(foldername)


plot_colorbar(FEMtimeObject[[1]], coeff_lims = coeff_lims, colorscale = viridis,
              file=paste0(foldername, "colorbar_time"))

folder1 = paste0(foldername, "smooth_time_1/")
if(!dir.exists(folder1)) dir.create(folder1)

names(FEMtimeObject) <- 1:n_time 
for(i in 1:length(FEMtimeObject)){
  plot_smooth_2D(FEMtimeObject[[i]], coeff_lims = coeff_lims, colorscale = viridis)
  snapshot3d(filename = paste0(folder1, "smooth_t_", names(FEMtimeObject)[i],".png"),
             fmt = "png", width = 800, height = 750, webshot = rgl.useNULL())
  close3d()  
}

folder2 = paste0(foldername, "smooth_time_2/")
if(!dir.exists(folder2)) dir.create(folder2)

names(FEMtimeObject1) <- 1:n_time 
for(i in 1:length(FEMtimeObject1)){
  plot_smooth_2D(FEMtimeObject1[[i]], coeff_lims = coeff_lims, colorscale = viridis)
  snapshot3d(filename = paste0(folder2, "smooth_t_", names(FEMtimeObject1)[i],".png"),
             fmt = "png", width = 800, height = 750, webshot = rgl.useNULL())
  close3d()  
}
