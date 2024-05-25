source("utils.R")
zoom = 0.7657689 # 0.6
userMatrix = rbind(c(  0.02091786,  0.99873853, -0.04564825,    0),
                   c( -0.13139695,  0.04800860,  0.99016660,    0),
                   c(  0.99110913, -0.01471432,  0.13223548,    0),
                   c(  0.00000000,  0.0000000,   0.0000000,     1))
windowRect = c(70,  106, 1920, 1117)

#pp <- par3d(no.readonly = TRUE)


plot_smooth_2.5D <- function(FEM, coeff_lims=NULL, 
                          colorscale = jet.col, ncolor = 128, alpha = 1,
                          ...){
  if (is.null(coeff_lims)){
    coeff_lims = extract_coeff(FEM)
  }
  
  triangles = c(t(FEM$FEMbasis$mesh$triangles))
  ntriangles = nrow(FEM$FEMbasis$mesh$triangles)
  order = FEM$FEMbasis$mesh$order
  nodes = FEM$FEMbasis$mesh$nodes
  edges = matrix(rep(0, 6*ntriangles), ncol = 2)
  for(i in 0:(ntriangles-1)){
    edges[3*i+1,] = c(triangles[3*order*i+1], triangles[3*order*i+2])
    edges[3*i+2,] = c(triangles[3*order*i+1], triangles[3*order*i+3])
    edges[3*i+3,] = c(triangles[3*order*i+2], triangles[3*order*i+3])
  }
  edges = edges[!duplicated(edges),]
  edges <- as.vector(t(edges))
  
  coeff = FEM$coeff
  
  FEMbasis = FEM$FEMbasis
  
  mesh = FEMbasis$mesh
  
  p = colorscale(n = ncolor, alpha = alpha)
  grDevices::palette(p)
  
  nsurf = dim(coeff)[[2]]
  for (isurf in 1:nsurf)
  {
    open3d(zoom = zoom, userMatrix = userMatrix, windowRect = windowRect)
    rgl.pop("lights") 
    light3d(specular = "black") 
    
    diffrange = diff(range(coeff_lims)) 
    
    col = coeff[triangles,isurf]
    col = (col - min(coeff[,isurf], na.rm =T))/diffrange*(ncolor-1)+1
    if(abs(diffrange) < 1e-10) col = rep(M, times=length(col)) # costanti
    rgl.triangles(x = nodes[triangles ,1], y = nodes[triangles ,2],
                  z = nodes[triangles,3],
                  color = col) #,...)
    # rgl.lines(x = nodes[edges ,1], y = nodes[edges ,2],
    #           z = nodes[edges,3],
    #           color = "black",...)
    aspect3d("iso")
    
    if (nsurf > 1 && isurf<nsurf)
    {readline("Press a button for the next plot...")}
  }
}
