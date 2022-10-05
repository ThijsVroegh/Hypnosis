## ---------------------------------------------------------------
library(metaSEM)
library(OpenMx)
library(ggplot2)
library(ggridges)
library(tidyverse)
library(semPlot)
library(qgraph)
library(igraph)
library(corrplot)
library(bnlearn)
library(Rgraphviz)
library(SEset)
library(kableExtra)
library(tidyr)
library(ggridges)

## Set seed
set.seed(123)

## ---------------------------------------------------------------
# read 7 correlation matrices from 7 different hypnosis studies

cordat6X6 <- readFullMat(file = "6x6corr_7.txt",skip = 1)

# Sample sizes of the individual studies on hypnotic experiences
N <- c(173,246,241,266,565,615,523) # N = 2629


## ---------------------------------------------------------------
is.pd(cordat6X6, check.aCov=FALSE, cor.analysis=TRUE, tol=1e-06)


## ---------------------------------------------------------------
df_studies <- data.frame (
  Authors = c("Kumar & Pekala", "Pekala, Forbes and Contrisciani", "Loi", "Varga, Józsa & Kekecs","Kolto", "Terhune & Cardeña", "Kumar,Pekala & Cummings"),
  Year = c(1988, 1989, 2011, 2014, 2015, 2010, 1996),
  Sample = c(173,246,241,266,565,615,523),
  Participants =c("students","students","students","volunteers","mainly students","volunteers","students"),
  Induction = c("HGSHS", "HGSHS", "a 10-minute trance induction audio file","SHSS-A", "HGSHS:A", "WSGC","HGSHS:A"),
  Administration = c("group", "group", "individual", "individual","group","group","group") 
                  )
  
  knitr::kable(df_studies, format="html")


## ---------------------------------------------------------------
stage1random_6 <- tssem1(Cov = cordat6X6, 
                         n = N, 
                         method = "REM",
                         RE.type="Diag")


## ---------------------------------------------------------------
if (stage1random_6$mx.fit$output$status$code > 1) 
{ stage1random_6 <- metaSEM::rerun(stage1random_6, checkHess=F, extraTries=30, silent=TRUE)}

## ---------------------------------------------------------------
summary(stage1random_6)


## ---------------------------------------------------------------
coef(stage1random_6, select = "fixed")


## ---------------------------------------------------------------
coef(stage1random_6, select = "random")


## ---------------------------------------------------------------
pooled6 <- vec2symMat(coef(stage1random_6,
                           select = "fixed"),
                           diag   = FALSE )

rownames(pooled6) <- colnames(pooled6) <- c("SA","AA","VC","AR","AE","AB")

pooled6 <- round(pooled6,3)

corpcor::is.positive.definite(pooled6, tol=1e-8)


## ---------------------------------------------------------------
corrplot(pooled6, method = "color", addCoef.col = "black",
         number.cex = .5, cl.pos = "n", diag=T,insig = "blank")


## ---------------------------------------------------------------
A <- create.mxMatrix(c(0, 0, 0,0, 0,"0.1*AB2SA",
                       0, 0, "0.1*VC2AA",0, "0.1*AE2AA", 0,
                       "0.1*SA2VC",0, 0, 0,"0.1*AE2VC", 0,
                       0,0,0,0,0,0,
                       "0.1*SA2AE",0,0,0,0,"0.1*AB2AE",
                       0,0,0,"0.1*AR2AB",0,0),
                     
                     type="Full", byrow=TRUE, ncol=6, nrow=6, name="A")


## ---------------------------------------------------------------
dimnames(A)[[1]] <- dimnames(A)[[2]] <- c("SA","AA","VC","AR","AE","AB")
A


## ---------------------------------------------------------------
S <- create.mxMatrix(c("0.1*var_SA",
                       0, "0.1*var_AA",
                       0, 0, "0.1*var_VC",
                       0, 0, 0, 1,
                       0, 0, 0, 0, "0.1*var_AE",
                       0, 0, 0, 0, 0, "0.1*var_AB"),
                     byrow=TRUE, type="Symm", ncol=6, nrow=6, name="S")

# inspect the model
dimnames(S)[[1]] <- dimnames(S)[[2]] <- c("SA","AA","VC","AR","AE","AB")
S

## ---------------------------------------------------------------
stage2random_6 <- tssem2(stage1random_6, 
                         Amatrix = A,
                         Smatrix = S,
                         diag.constraints = TRUE,
                         intervals="LB",
                         model.name = "TSSEM2 Random Effects Analysis Phenomenology of hypnosis")

## ---------------------------------------------------------------
if (stage2random_6$mx.fit$output$status$code > 1) 
{ stage2random_6 <- metaSEM::rerun(stage2random_6, checkHess=F, extraTries=20, silent=TRUE)}


## ---------------------------------------------------------------
summary(stage2random_6)


## ---------------------------------------------------------------
my.plot2 <- meta2semPlot(stage2random_6, 
                         manNames = c("SA","AA","VC","AR","AE","AB") )

Labels <- c(
  "Self-awareness",             #SA
  "Altered state of awareness", #AA
  "Volitional control",         #VC
  "Arousal",                    #AR
  "Altered experience",         #AE
  "Inward directed absorption") #AB

mm <- matrix(c("AR"   , "AB",  "SA",
                NA    , "AE"  , NA,
                NA    , "VC", "AA"), byrow = TRUE, 3, 3)

semPaths(my.plot2, whatLabels="path", nCharEdges=10, nCharNodes=10, layout=mm, color="yellow", edge.label.cex=0.8)


## ---------------------------------------------------------------
semPaths(my.plot2, whatLabels="est", nCharEdges=10, nCharNodes=10,layout=mm, color="green", edge.label.cex=0.8,
         nodeNames=Labels,legend.cex = 0.3)


## ---------------------------------------------------------------
estimate <- EBICglasso(pooled6, n = 2106,threshold = TRUE, returnAllResults = TRUE)

# qgraph method estimates a non-symmetric omega matrix, but uses forceSymmetric to create
# a symmetric matrix (see qgraph:::EBICglassoCore line 65)
library(Matrix)

# returns the precision matrix
omega <- as.matrix(forceSymmetric(estimate$optwi))

library(qgraph)
parcor <- qgraph::wi2net(omega)
pnet <- qgraph(parcor, repulsion = .8,vsize = c(10,15), theme = "colorblind", fade = F)

# Name the variables
dimnames(omega) <- dimnames(pooled6)
varnames <- rownames(omega)

# Estimate SE-set with rounding
SE_h <- network_to_SEset(omega, digits = 2, rm_duplicates = TRUE)


## ---------------------------------------------------------------
# Directed edge frequency
propd <- propcal(SE_h, rm_duplicate = TRUE, directed = TRUE)

# Plot as a network
qgraph(propd, edge.color = "darkgreen", layout = pnet$layout, edge.labels = T, maximum = 1)


## ---------------------------------------------------------------
r2set <- r2_distribution(SE_h, cormat = pooled6, names = NULL, indices = c(1,2,3,4,5,6))

df <- as.data.frame(r2set, col.names = paste0("X",1:6))
df2 <- tidyr::gather(df)

p <- ggplot(df2, aes(y = key, x = value)) + 
  geom_density_ridges(fill = "light seagreen") + 
  labs(y = "Variable", x = expression(paste("Controllability value ", R^2)))
p + theme(text = element_text(size = 20), 
            axis.title.x = element_text(size = 20),
            axis.title.y = element_text(size = 20), 
            panel.background = element_blank())


## ---------------------------------------------------------------
minvalue <- 50
minseed <- 9000

for(i in 35000:40000) {
  set.seed(i)
  M <- chol(pooled6)
  r <- t(M) %*% matrix(rnorm(6*2629), nrow = 6, ncol = 2629)
  r <- t(r)
  
  rdata <-  as.data.frame(r)
  corrdata <- round(cor(rdata),3)
  
  diff <- abs(pooled6-corrdata)
  diff2 <- diff > 0.05
  
  sumdiff <- sum(diff2)
  
  # updating minimal value
  minvalue <- ifelse ((sumdiff < minvalue), sumdiff, minvalue) 
  minseed <- ifelse ((sumdiff <= minvalue), i, minseed) 
}


## ---------------------------------------------------------------
minvalue
minseed


## ---------------------------------------------------------------
set.seed(minseed)

M <- chol(pooled6)
nvars <-dim(M)[1]

# number of observations to simulate
nobs <- 2629

# Random variables that follow the R correlation matrix
r <- t(M) %*% matrix(rnorm(nvars*nobs), nrow=nvars, ncol=nobs)
r <- t(r)
rdata <-  as.data.frame(r)
rrdata <- round(cor(rdata),3)

diff <- abs(pooled6-rrdata)
diff2 <- diff > 0.05
sum(diff2)


## ---------------------------------------------------------------
corrplot::corrplot(abs(pooled6-rrdata), method = "color", addCoef.col = "black", number.cex = .6, cl.pos = "n", diag=T, insig = "blank")
title(main="Absolute Difference")


## ---------------------------------------------------------------
source("SBN_robustNet.R")
source("importance.R") 
source("plot_s.R")
source("plot_DWI.R")


## ---------------------------------------------------------------
datiBN <- rdata
nomi   <- c("SA","AA","VC","AR","AE","AB")

blacklist <-  data.frame (from = c(rep("AA",6)),
                          to   = c(colnames(datiBN)[1:6]))

# view blacklist  
blacklist

# Specify the highlight node
target_node <- "AA"


## ---------------------------------------------------------------
net <- SBN_robustNet(datiBN, blacklist = blacklist)


## ---------------------------------------------------------------
table <- net$arcs

kbl(table) %>%
  kable_classic(fixed_thead = T, full_width = T) %>% 
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(1:12, width = "30em", background = "white") %>% 
  
  column_spec(13, color = "white",
              background = spec_color(table$tot, end = 0.7, option="A"))


## ---------------------------------------------------------------
x <- net$robust.net # x holds the robust network as derived from SBN_robustNet  
arcs(x)             # 12
score (x, datiBN)   # Network score -19681.47
graphviz.plot(x)    # plot DAG


## ---------------------------------------------------------------
# Like the BN derived above, let's make a bn-object based on the original model
xdag <- empty.graph(nodes = c("AR", "AB", "SA","VC", "AE","AA"))
arc.set <- matrix(c("AR", "AB",
                    "AB", "SA",
                    "AB", "AE",
                    "SA", "AE",
                    "SA", "VC",
                    "AE", "VC",
                    "AE", "AA",
                    "VC", "AA"),
                    byrow = TRUE, 
                    ncol = 2,
                    dimnames = list(NULL, c("from", "to")))
arcs(xdag) <- arc.set


## ---------------------------------------------------------------
# centralities original model
centr_original <- qgraph(xdag,DoNotPlot=T)

# centralities alternative model
centr_alternative <- qgraph(x,DoNotPlot=T)

centralityPlot(list(original = centr_original,
                    alternative = centr_alternative),
               theme_bw = T, scale = "z-scores")


## ---------------------------------------------------------------
set.seed(123)

R = 100
m = 100

ar  <- arcs(x) #arcs of the robust model
n.arcs <- dim(ar)[1] #29
ar  <- cbind(ar,paste(ar[,1],ar[,2]),paste(ar[,2],ar[,1]))
rep <- matrix(0,n.arcs,R)

for (i in 1:R){
  sam_bo <- datiBN[sample(c(1:dim(datiBN)[1]), m, replace = TRUE),] 
  
  # check for appropriate algorithm selection based on the outcome of SBN_robustNet 
    net.r <- hc(sam_bo, 
                score     = "bic-g", 
                blacklist = blacklist, 
                debug     = FALSE) 

  ar_s <- arcs(net.r)
  ar_s <- cbind(ar_s,paste(ar_s[,1],ar_s[,2]))
  rep[ar[,3]%in%ar_s[,3],i]<-1
  rep[ar[,4]%in%ar_s[,3],i]<-1
}

strength<-data.frame(ar[,1:2],apply(rep,1,mean))
strength


## ---------------------------------------------------------------
plot_s(x,strength)


## ---------------------------------------------------------------
impo <- imp(x,target_node,strength)

plot_DWI(x, impo$importance, target_node)


## ---------------------------------------------------------------
paModel      <- bnpa::gera.pa.model(x, datiBN)
pa.model.fit <- lavaan::sem(paModel, datiBN, std.lv = TRUE)


## ---------------------------------------------------------------
lavaan::summary(pa.model.fit, rsquare = TRUE, standardized = TRUE, modindices = TRUE)
lavaan::standardizedSolution(pa.model.fit) 


## ---------------------------------------------------------------
fitmeasures <- print(lavaan::fitMeasures(pa.model.fit, c("chisq", "df", "pvalue", "cfi","tli", "rmsea","rmsea.ci.lower","rmsea.ci.upper","rmsea.pvalue","srmr","gfi"), output = "text"), add.h0 = TRUE) %>% kbl() %>% 
kable_classic(full_width = F) %>% 
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(1:2, background = "white")


## ---------------------------------------------------------------
# define original layout of the experiential model
m <- matrix(c("AR" , NA   , NA  , NA,
              NA   , "AB", NA   , "SA",
              NA   , NA,  "VC"  , NA,
              NA   , "AE", NA   , "AA"), byrow = TRUE, 4, 4)

SEMmodel <- semPlot::semPaths(pa.model.fit,
                              whatLabels="std", 
                              layout = m,  
                              edge.label.cex=0.7,
                              borders = TRUE,
                              color  = "green",
                              edge.color = "black") 

# Mark all parameter estimates by asterisks based on p-Values
p_pa2 <- semptools::mark_sig(SEMmodel, pa.model.fit)

#We can use mark_se() to add the standard errors for the parameter estimates
p_pa2 <- semptools::mark_se(p_pa2, pa.model.fit, sep = "\n")

my_curve_list <- c("AA ~ AB" = -2,
                   "AE ~ SA" = -2,
                   "AA ~ AE" = -2)
            
p_pa2 <- semptools::set_curve(p_pa2, my_curve_list)

plot(p_pa2)
## see https://cran.r-project.org/web/packages/semptools/vignettes/semptools.html


## ---------------------------------------------------------------
options(stringsAsFactors = FALSE)
set.seed(1234)

# Prepare network by making an appropriate igraph object
g_w_d <- igraph::as.igraph(SEMmodel, attributes = TRUE)

igraph::is_weighted(g_w_d) #TRUE

V(g_w_d)$name <- V(g_w_d)$label # ensure that the igraph object has a name attribute

#plot(g_w_d, 
#     edge.arrow.size = .2,
#     edge.curved = .1)


## ---------------------------------------------------------------
# Initial activation values
initial_df <-data.frame(node = c('AB', "AE"), activation = 50, stringsAsFactors = F) 

initial_df

# Simulation
result <- spreadr::spreadr(network = g_w_d,
                           start_run = initial_df,                                                  decay = 0.01,
                           retention = 0.9, 
                           suppress = 0,
                           time = 20,
                           include_t0 = TRUE) 

head(result, 10)
tail(result, 10)

# visualize the results 
ggplot(data = result, 
       aes(x = time, 
           y = activation, 
           color = node, 
           group = node)) +
  geom_point() + geom_line() + ggtitle('weighted, directed network') 

