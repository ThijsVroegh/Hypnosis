
imp<-function(x,target,strength){
l_path<-list()	
p1<-x$nodes[[target]]$parents
path<-cbind(tar=rep(target,length(p1)),p1)
for (i in 1:dim(path)[1]){
l_path[[i]]<-path[i,]
}

ii<-dim(path)[1]
j=2
k1<-1
k2<-length(l_path)
o<-1
while (o>0){
o<-0
for (i in k1:k2){
par<-x$nodes[[l_path[[i]][length(l_path[[i]])]]]$parents
if (length(par)>0){
for (s in 1:length(par)){
ii=ii+1
l_path[[ii]]<-c(l_path[[i]],par[s])
o<-o+1
}
}
}
k1<-k2+1
k2<-length(l_path)
}

Itab<-matrix(0,length(l_path),2)
v_p<-rep(0,length(l_path))
for (i in 1:length(l_path)){
jj<-matrix(0,length(l_path[[i]])-1,2)
for (ar in 1:length(l_path[[i]])-1){
jj[ar,2]<-l_path[[i]][ar]
jj[ar,1]<-l_path[[i]][ar+1]
}
Itab[i,1]<-length(l_path[[i]])-1
Itab[i,2]<-(prod(c(strength[which(paste(strength$from, strength$to)%in%paste(jj[,1], jj[,2])),3],
strength[which(paste(strength$to, strength$from)%in%paste(jj[,1], jj[,2])),3])))^(length(l_path[[i]])-1)

v_p[i]<-l_path[[i]][length(l_path[[i]])]
}
Ip<-data.frame(v_p,Itab)
Ip<-Ip[order(Ip$v_p),]
colnames(Ip)<-c("node","n.arcs","pweight")
impo<-tapply(Ip$pweight,Ip$node,sum)
oo<-nodes(x)[nodes(x)!=target]
import<-rep(0,length(oo))
names(import)<-oo
for (ii in oo){
if (ii%in%names(impo))
import[ii]<-impo[ii]
}
return(list(importance=import,tab.I=I))
}