source("utils.R")

foldername = "imgs/"
if(!dir.exists(foldername)) dir.create(foldername)

# 1
data = read.table("dataframe_1.txt")
head(data)
methods <- unique(data$methods)
n_obs   <- unique(data$n_obs)
data$n_obs   = factor(data$n_obs,   levels=n_obs)       #
data$methods = factor(data$methods, levels=methods) # o una permutazione... 

foldername = paste0(foldername, "test_boxplot/")
if(!dir.exists(foldername)) dir.create(foldername)

plot_boxplot(data, n_obs="n_obs", method="methods", 
             filename = paste0(foldername,"boxplots_1.pdf"))

# 2
data = read.table("dataframe_2.txt")
head(data)
methods <- unique(data$method)
n_obs   <- unique(data$n_obs)
data$n_obs   = factor(data$n_obs,   levels=n_obs)       #
data$method  = factor(data$method, levels=methods) # o una permutazione... 
plot_boxplot(data, n_obs="n_obs", method="method",
             filename = paste0(foldername,"boxplots_2.pdf"))

