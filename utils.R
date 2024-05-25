if(!require(pacman)) install.packages("pacman")
pacman::p_load("fdaPDE" ,"plotrix", "latex2exp", "RColorBrewer", "viridis",
               "dplyr")

extract_coeff <- function(FEMObject){
  elements = NULL
  mesh = FEMObject$FEMbasis$mesh
  if( is(mesh, "mesh.2D") | is(mesh, "mesh.2.5D")){
    elements = mesh$triangles
  }else if( is(mesh, "mesh.1.5D")){
    elements = mesh$edges
  }else{
    elements = mesh$tetrahedrons # tetrahedrons
  }
  coeff <- apply(elements, MARGIN=1, FUN = function(row){
    mean(FEMObject$coeff[row,])
  })
  return(coeff)
}

smooth_lim <- function(FEMObject, ...){
  coeff <- extract_coeff(FEMObject)
  lims = c(1e10, -1e10)
  lims[1] = min(coeff, lims[1])
  lims[2] = max(coeff, lims[2])
  
  #coeffs_args = list()
  args = list(...)
  if( length(args) > 0L){
    for(i in 1:length(args)){
      if(! is(args[[i]], "FEM") ) stop("Provides ONLY FEM objects.")
        coeff = extract_coeff(args[[i]])
      lims[1] = min(coeff, lims[1])
      lims[2] = max(coeff, lims[2])
    }
  }
  return(lims)
}

# ---
plot_colorbar <- function(FEMObject, coeff_lims= NULL, 
                          colorscale = jet.col, ncolor = 128, 
                          cex.axis = 2, file = "colorbar"){
  coeff <- extract_coeff(FEMObject)
  if(is.null(coeff_lims)) coeff_lims = c(min(coeff, na.rm = T), max(coeff, na.rm = T))
  cmin = coeff_lims[1]; cmax=coeff_lims[2]
  
  exps <- -15:15
  range_ <- round( diff(range(coeff_lims)), digits = 2)
  cmin_exp <- which(floor(log10(abs(signif(signif(cmin, digits = 2) / 10^exps, digits = 0))))==0)
  cmax_exp <- which(floor(log10(abs(signif(signif(cmax, digits = 2) / 10^exps, digits = 0))))==0)
  k <- exps[max(cmin_exp, cmax_exp)]
  
  at = seq(0, 100, length.out=5)
  labels = as.character(round(seq(cmin*10^(-k), cmax*10^(-k), length.out=5), 2))
  text_ <- ifelse(k != 0, paste0("$\\times 10^{", k,"}$"), "")
  
  diffrange = cmax - cmin 
  if(abs(diffrange) < 1e-10) ncolor = 1 # costanti
  
  labels_grad = ifelse(text_ == "", "", TeX(text_))
  
  pdf(paste0(file, "_horiziontal.pdf"), family = "serif", width = 11, height = 3)
  par(mai = c(1,0.75,0,0))
  plot(c(0, 112.5), c(0, 15), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", frame.plot = F)
  gradient.rect(0, 0, 100, 5, col = colorscale(ncolor), border = "black")
  axis(1, at = at, labels = labels, # at = c(0,33.33,66.66,100)
       cex.axis = cex.axis, lwd.ticks = 0, lwd = 0) # lwd.ticks = 2, 
  text(107,2, labels_grad, cex = 2)
  dev.off()
  
  pdf(paste0(file, "_vertical.pdf"), family = "serif", width = 3, height = 11)
  par(mai = c(1,0.75,0,0))
  plot(c(0, 15), c(0, 112.5), type = "n", xlab = "", ylab = "", xaxt = "n", yaxt = "n", frame.plot = F)
  gradient.rect(0, 0, 5, 100, col = colorscale(ncolor), border = "black", gradient = "y")
  axis(4, at = at, labels = labels, # at = c(0,33.33,66.66,100)
       cex.axis = cex.axis, lwd.ticks = 0, lwd = 0,  line=-7.5) # lwd.ticks = 2, 
  text(2.5, 107, labels_grad, cex = 2)
  dev.off()
}



