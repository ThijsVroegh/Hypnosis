SBN_robustNet<-function(data,blacklist,mr=6.6){

#######################################################################
#I run all the learning algorythms#
#######################################################################
datiBN<-data.frame(data)
###Score-based algorithms
net.hc1 <- hc(datiBN, score = "bic-g", blacklist = blacklist)
net.hc2 <- hc(datiBN, score = "aic-g", blacklist = blacklist)
net.tabu1 <- tabu(datiBN, score = "bic-g", blacklist = blacklist)
net.tabu2 <- tabu(datiBN, score = "aic-g", blacklist = blacklist)

###Constraint-based algorithms
net.gs <- gs(datiBN, blacklist= blacklist)
net.iamb <- iamb(datiBN, blacklist = blacklist)
net.fiamb <- fast.iamb(datiBN, blacklist = blacklist)
net.intamb <- inter.iamb(datiBN, blacklist = blacklist)

###Hybrid algorithms
net.mmhc1 <- mmhc(datiBN, blacklist = blacklist)
net.mmhc2 <- mmhc(datiBN,  blacklist = blacklist)
net.rsmax <- rsmax2(datiBN, blacklist = blacklist)


#######################################################################
#arcs across the different learning procedures#
#######################################################################

di<-data.frame(directed.arcs(net.hc1),rep(1, dim(directed.arcs(net.hc1))[1]))
undi<-data.frame(undirected.arcs(net.hc1),rep(0.5, dim(undirected.arcs(net.hc1))[1]))
colnames(di)<-c("from","to","Arc")
colnames(undi)<-c("from","to","Arc")
ar<-rbind(di,undi)
arcs<-paste(ar[,1],ar[,2])
aa<-data.frame(arcs,ar[,3])
colnames(aa)[2]<-"hc1"

di<-data.frame(directed.arcs(net.hc2),rep(1, dim(directed.arcs(net.hc2))[1]))
undi<-data.frame(undirected.arcs(net.hc2),rep(0.5, dim(undirected.arcs(net.hc2))[1]))
colnames(di)<-c("from","to","Arc")
colnames(undi)<-c("from","to","Arc")
ar<-rbind(di,undi)
arcs<-paste(ar[,1],ar[,2])
bb<-data.frame(arcs,ar[,3])
colnames(bb)[2]<-"hc2"

di<-data.frame(directed.arcs(net.tabu1),rep(1, dim(directed.arcs(net.tabu1))[1]))
undi<-data.frame(undirected.arcs(net.tabu1),rep(0.5, dim(undirected.arcs(net.tabu1))[1]))
colnames(di)<-c("from","to","Arc")
colnames(undi)<-c("from","to","Arc")
ar<-rbind(di,undi)
arcs<-paste(ar[,1],ar[,2])
cc<-data.frame(arcs,ar[,3])
colnames(cc)[2]<-"tabu1"

di<-data.frame(directed.arcs(net.tabu2),rep(1, dim(directed.arcs(net.tabu2))[1]))
undi<-data.frame(undirected.arcs(net.tabu2),rep(0.5, dim(undirected.arcs(net.tabu2))[1]))
colnames(di)<-c("from","to","Arc")
colnames(undi)<-c("from","to","Arc")
ar<-rbind(di,undi)
arcs<-paste(ar[,1],ar[,2])
dd<-data.frame(arcs,ar[,3])
colnames(dd)[2]<-"tabu2"

di<- data.frame(directed.arcs(net.gs),rep(1, dim(directed.arcs(net.gs))[1]))
undi<- data.frame(undirected.arcs(net.gs),rep(0.5, dim(undirected.arcs(net.gs))[1]))
colnames(di)<-c("from","to","Arc")
colnames(undi)<-c("from","to","Arc")
ar<-rbind(di,undi)
arcs<-paste(ar[,1],ar[,2])
ee<- data.frame(arcs,ar[,3])
colnames(ee)[2]<-"gs"

di<- data.frame(directed.arcs(net.iamb),rep(1, dim(directed.arcs(net.iamb))[1]))
undi<-data.frame(undirected.arcs(net.iamb),rep(0.5, dim(undirected.arcs(net.iamb))[1]))
colnames(di)<-c("from","to","Arc")
colnames(undi)<-c("from","to","Arc")
ar<-rbind(di,undi)
arcs<-paste(ar[,1],ar[,2])
ff<-data.frame(arcs,ar[,3])
colnames(ff)[2]<-"iamb"

di<-data.frame(directed.arcs(net.fiamb),rep(1, dim(directed.arcs(net.fiamb))[1]))
undi<-data.frame(undirected.arcs(net.fiamb),rep(0.5, dim(undirected.arcs(net.fiamb))[1]))
colnames(di)<-c("from","to","Arc")
colnames(undi)<-c("from","to","Arc")
ar<-rbind(di,undi)
arcs<-paste(ar[,1],ar[,2])
gg<-data.frame(arcs,ar[,3])
colnames(gg)[2]<-"fiamb"

di<-data.frame(directed.arcs(net.intamb),rep(1, dim(directed.arcs(net.intamb))[1]))
undi<-data.frame(undirected.arcs(net.intamb),rep(0.5, dim(undirected.arcs(net.intamb))[1]))
colnames(di)<-c("from","to","Arc")
colnames(undi)<-c("from","to","Arc")
ar<-rbind(di,undi)
arcs<-paste(ar[,1],ar[,2])
hh<-data.frame(arcs,ar[,3])
colnames(hh)[2]<-"intamb"

di<-data.frame(directed.arcs(net.mmhc1),rep(1, dim(directed.arcs(net.mmhc1))[1]))
undi<-data.frame(undirected.arcs(net.mmhc1),rep(0.5, dim(undirected.arcs(net.mmhc1))[1]))
colnames(di)<-c("from","to","Arc")
colnames(undi)<-c("from","to","Arc")
ar<-rbind(di,undi)
arcs<-paste(ar[,1],ar[,2])
ii<-data.frame(arcs,ar[,3])
colnames(ii)[2]<-"mmhc1"

di<-data.frame(directed.arcs(net.mmhc2),rep(1, dim(directed.arcs(net.mmhc2))[1]))
undi<-data.frame(undirected.arcs(net.mmhc2),rep(0.5, dim(undirected.arcs(net.mmhc2))[1]))
colnames(di)<-c("from","to","Arc")
colnames(undi)<-c("from","to","Arc")
ar<-rbind(di,undi)
arcs<-paste(ar[,1],ar[,2])
ll<-data.frame(arcs,ar[,3])
colnames(ll)[2]<-"mmhc2"

di<-data.frame(directed.arcs(net.rsmax),rep(1, dim(directed.arcs(net.rsmax))[1]))
undi<-data.frame(undirected.arcs(net.rsmax),rep(0.5, dim(undirected.arcs(net.rsmax))[1]))
colnames(di)<-c("from","to","Arc")
colnames(undi)<-c("from","to","Arc")
ar<-rbind(di,undi)
arcs<-paste(ar[,1],ar[,2])
mm<-data.frame(arcs,ar[,3])
colnames(mm)[2]<-"rsmax"

m1<-merge(aa,bb,by=c("arcs"),all = TRUE)
m2<-merge(m1,cc,by=c("arcs"),all = TRUE)
m3<-merge(m2,dd,by=c("arcs"),all = TRUE)
m4<-merge(m3,ee,by=c("arcs"),all = TRUE)
m5<-merge(m4,ff,by=c("arcs"),all = TRUE)
m6<-merge(m5,gg,by=c("arcs"),all = TRUE)
m7<-merge(m6,hh,by=c("arcs"),all = TRUE)
m8<-merge(m7,ii,by=c("arcs"),all = TRUE)
m9<-merge(m8,ll,by=c("arcs"),all = TRUE)
m10<-merge(m9,mm,by=c("arcs"),all = TRUE)

m10[is.na(m10)]=0

tot<-apply(m10[,2:12],1,sum)
tab<-cbind(m10,tot)

tab_rid<-tab[tab$tot>mr,]
tot_c<-sort(apply(tab_rid[,-c(1,13)],2,sum),decreasing = TRUE)

#Robust arcs
wl<-t(matrix(unlist(strsplit(as.character(tab_rid[,1]), " ")),2,dim(tab_rid)[1]))
colnames(wl)<-c("from","to")

#Robust network
if (length(tot_c[tot_c==(max(tot_c))])==1){
netR<-names(tot_c[1])}

if (length(tot_c[tot_c==(max(tot_c))])>1){
robust_net<-names(tot_c[tot_c==(max(tot_c))])
nr_net<-length(tot_c[tot_c==(max(tot_c))])
ntr<-dim(datiBN)[1]*80/100
rep<-matrix(0,100,nr_net)
ii<-0
for (net in robust_net){
ii<-ii+1

for (jj in 1:100){
training_s <- sample(1:nrow(datiBN),ntr, replace=FALSE)
training<-datiBN[training_s,]
test <- datiBN[-training_s,]
if (net=="hc1")
x<- hc(training, score = "bic-g", blacklist =blacklist)
if (net=="hc2")
x <- hc(training, score = "aic-g", blacklist = blacklist)
if (net=="tabu1")
x <- tabu(training, score = "bic-g", blacklist = blacklist)
if (net=="tabu2")
x <- tabu(training, score = "aic-g", blacklist = blacklist)
if (net=="gs")
x <- gs(training, blacklist= blacklist)
if (net=="iamb")
x<- iamb(training, blacklist = blacklist)
if (net=="fiamb")
x <- fast.iamb(training, blacklist = blacklist)
if (net=="intamb")
x <- inter.iamb(training, blacklist = blacklist)
if (net=="mmhc1")
x <- mmhc(training, blacklist = blacklist)
if (net=="mmhc2")
x <- mmhc(training, blacklist = blacklist)
if (net=="rsmax")
x<- rsmax2(training, blacklist = blacklist)

fit<-bn.fit(x,training)

fit1<-predict(fit,"AA", test)
CM1<-table(test$AA, fit1)
MR1<-100-(sum(diag(CM1))/sum(CM1)*100)
rep[jj,ii]<-MR1
}
}
colnames(rep)<-robust_net
misRate<-apply(rep,2,mean)
netR<-names(misRate[misRate==(min(misRate))])
}

if (netR=="hc1")
net.r<- hc(datiBN, score = "bic-g", blacklist =blacklist)
if (netR=="hc2")
net.r <- hc(datiBN, score = "aic-g", blacklist = blacklist)
if (netR=="tabu1")
net.r <- tabu(datiBN, score = "bic-g", blacklist = blacklist)
if (netR=="tabu2")
net.r <- tabu(datiBN, score = "aic-g", blacklist = blacklist)
if (netR=="gs")
net.r <- gs(datiBN, blacklist= blacklist)
if (netR=="iamb")
net.r<- iamb(datiBN, blacklist = blacklist)
if (netR=="fiamb")
net.r <- fast.iamb(datiBN, blacklist = blacklist)
if (netR=="intamb")
net.r <- inter.iamb(datiBN, blacklist = blacklist)
if (netR=="mmhc1")
net.r <- mmhc(datiBN, blacklist = blacklist)
if (netR=="mmhc2")
net.r <- mmhc(datiBN, blacklist = blacklist)
if (netR=="rsmax")
net.r<- rsmax2(datiBN, blacklist = blacklist)

tabs<-tab[order(tab$tot, decreasing=TRUE),]

############
####plot####
############
# convert arc to edge 
mat<-matrix(0,length(nodes(net.r)),length(nodes(net.r)))
rownames(mat) <- nodes(net.r)
colnames(mat) <- nodes(net.r)
for (i in 1:dim(arcs(net.r))[1]){
mat[nodes(net.r)==arcs(net.r)[i,1],nodes(net.r)==arcs(net.r)[i,2]]<-1
}
# create the graphAM object from the bn object.
g1 <- graphAM(adjMat=mat,edgemode="directed")
wltot<-t(matrix(unlist(strsplit(as.character(tab[,1]), " ")),2,dim(tab)[1]))
colnames(wltot)<-c("from","to")
to.score= apply(wltot, 1, paste, collapse = "~")
lab<-as.character(tab$tot)
names(lab) <- to.score
g1 <- layoutGraph(g1, edgeAttrs=list(label=lab))
graph.par(list(nodes=list(col="darkgreen", lwd=1, lty=1,fontsize=50, shape="ellipse"),edges=list(fontsize=10, textCol="black")))
nodeRenderInfo(g1) <- list(shape="ellipse")
to.highlight = apply(wl, 1, paste, collapse = "~")
edgeRenderInfo(g1)[["col"]][to.highlight] = "red"
edgeRenderInfo(g1)[["lwd"]][to.highlight] = 2
renderGraph(g1)
return(list(arcs=tab,robust.net=net.r,ra=wl))
}

