source("utils.R")

zoom = 0.7657689
windowRect = c(70,  106, 1920, 1117)
plot_smooth_2D <- function(FEM, coeff_lims=smooth_lim(FEM), 
                           colorscale = jet.col, ncolor = 128, alpha = 1,
                           ...){
    nodes <- FEM$FEMbasis$mesh$nodes
    triangles <- as.vector(t(FEM$FEMbasis$mesh$triangles))
    coeff = FEM$coeff 
    p = colorscale(n = ncolor, alpha = alpha)
    grDevices::palette(p)
    open3d(zoom=zoom, windowRect=windowRect)
    pop3d("lights")
    light3d(specular="black")
      
    diffrange = diff(range(coeff_lims)) 
    col = coeff[triangles,]
    col = (col - min(coeff, na.rm =T))/diffrange*(ncolor-1)+1
    if(abs(diffrange) < 1e-10) col = rep(M, times=length(col)) # costanti
      
    #z <- FEM$coeff[triangles,]
    triangles3d(nodes[triangles,1], nodes[triangles,2], 0,
                  color = col,...)
      
    aspect3d(2,2,1)
    view3d(0,0)
}
