plot_DWI<-function(x,y,target){

# convert arc to edge 
mat<-matrix(0,length(nodes(x)),length(nodes(x)))
rownames(mat) <- nodes(x)
colnames(mat) <- nodes(x)
for (i in 1:dim(arcs(x))[1]){
mat[nodes(x)==arcs(x)[i,1],nodes(x)==arcs(x)[i,2]]<-1
}
# create the graphAM object from the bn object.
g1 <- graphAM(adjMat=mat,edgemode="directed")

g1 <- layoutGraph(g1)
graph.par(list(nodes=list(col="darkgreen", lwd=1, lty=1,fontsize=22, shape="ellipse")))
nodeRenderInfo(g1) <- list(shape="ellipse")
#renderGraph(g1)
nodeRenderInfo(g1)[["fill"]][target] = "lightcoral"
nodeRenderInfo(g1)[["col"]][target] = "red"
#renderGraph(g1)

yy<-y[which(y>0)]
yy<-sort(yy)
pal <- colorRampPalette(c("#DCF7DC","#105E10"))
ColorRamp <- pal(length(yy))
for(cc in c(1:length(yy))){
nodeRenderInfo(g1)[["fill"]][names(yy[cc])] = ColorRamp[cc]
}
renderGraph(g1)
}