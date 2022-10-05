plot_s<-function(x,y){

# convert arc to edge 
mat<-matrix(0,length(nodes(x)),length(nodes(x)))
rownames(mat) <- nodes(x)
colnames(mat) <- nodes(x)
for (i in 1:dim(arcs(x))[1]){
mat[nodes(x)==arcs(x)[i,1],nodes(x)==arcs(x)[i,2]]<-1
}
# create the graphAM object from the bn object
g1 <- graphAM(adjMat=mat,edgemode="directed")
to.score= apply(y[,1:2], 1, paste, collapse = "~")
lab<-as.character(y[,3])
names(lab) <- to.score
g1 <- layoutGraph(g1, edgeAttrs=list(label=lab))
graph.par(list(nodes=list(lwd=1, lty=1,fontsize=22, shape="ellipse"),edges=list(fontsize=10, textCol="black")))
nodeRenderInfo(g1) <- list(shape="ellipse")
renderGraph(g1)
}


