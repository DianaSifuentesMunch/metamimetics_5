###need to verify conflicts with libraries 
rm(list=ls(all=TRUE))
library(igraph)

g<-read.graph("graph.graphml" , format="graphml")


###OUTPUTS
#previous steps
ind1<-which(V(g)$MY.RULE==1)
ind2<-which(V(g)$MY.RULE==2)
ind3<-which(V(g)$MY.RULE==3)
ind4<-which(V(g)$MY.RULE==4)
closeness<-centralization.closeness (g)
knn<-graph.knn(g)
triangles<-adjacent.triangles(g)
between<-centralization.betweenness (g,directed=FALSE)
eigen<-centralization.evcent (g)
degrees<-degree(g)
degreesNormal<-degree(g,normalized=TRUE)
NumAgents<-vcount(g)




# adapted from http://www.shizukalab.com/toolkits/sna/node-level-calculations
reach2=function(x){
  r=vcount(x)
  n=neighborhood(x,2)
  l=unlist(lapply(n,function(x) length(x)/r))
}

# adapted from http://www.shizukalab.com/toolkits/sna/node-level-calculations
reach3=function(x){
  r=vcount(x)
  n=neighborhood(x,3)
  l=unlist(lapply(n,function(x) length(x)/r))
  }
  

# from http://www.shizukalab.com/toolkits/sna/node-level-calculations
dwreach=function(x){
  distances=shortest.paths(x) #create matrix of geodesic distances
  diag(distances)=1 # replace the diagonal with 1s
  weights=1/distances # take the reciprocal of distances
  apply(weights,1,sum) # sum for each node (row)
}

Reach2<-reach2(g)
Reach3<-reach3(g)
DWreach<-dwreach(g)

#LOCAL
#proportions
maxi<-length(ind1)*100/NumAgents
mini<-length(ind2)*100/NumAgents
conf<-length(ind3)*100/NumAgents
anti<-length(ind4)*100/NumAgents

#cooperation
ind<-which(V(g)$COOPERATE==TRUE)
cooperationRate<- length(ind) *100/ NumAgents
coopMaxi<-length(intersect(ind,ind1))*100/length(ind1)
coopMini<-length(intersect(ind,ind2))*100/length(ind2)
coopConf<-length(intersect(ind,ind3))*100/length(ind3)
coopAnti<-length(intersect(ind,ind4))*100/length(ind4)
gcoopMaxi<-length(intersect(ind,ind1))*100/NumAgents
gcoopMini<-length(intersect(ind,ind2))*100/NumAgents
gcoopConf<-length(intersect(ind,ind3))*100/NumAgents
gcoopAnti<-length(intersect(ind,ind4))*100/NumAgents

#satisfaction 
satisfactionRate<-mean(V(g)$SATISFACTION2)*100
satMaxi<-mean(V(g)$SATISFACTION2[ind1])*100 
satMini<-mean(V(g)$SATISFACTION2[ind2]) *100 
satConf<-mean(V(g)$SATISFACTION2[ind3])*100  
satAnti<-mean(V(g)$SATISFACTION2[ind4])*100  

#degree 
degreeMaxi<-mean(degrees[ind1]) 
degreeMini<-mean(degrees[ind2]) 
degreeConf<-mean(degrees[ind3]) 
degreeAnti<-mean(degrees[ind4]) 
NdegreeMaxi<-mean(degreesNormal[ind1]) 
NdegreeMini<-mean(degreesNormal[ind2]) 
NdegreeConf<-mean(degreesNormal[ind3]) 
NdegreeAnti<-mean(degreesNormal[ind4]) 


#cluster 
clustering<-transitivity(g,type="local",isolates="zero")
clusterMaxi<-mean(clustering[ind1]) 
clusterMini<-mean(clustering[ind2]) 
clusterConf<-mean(clustering[ind3]) 
clusterAnti<-mean(clustering[ind4]) 



#betweeness 
betweenvertex<-between$res
betweenMaxi<-mean(betweenvertex[ind1]) 
betweenMini<-mean(betweenvertex[ind2]) 
betweenConf<-mean(betweenvertex[ind3]) 
betweenAnti<-mean(betweenvertex[ind4]) 

#prank
pagerank<-page.rank(g,directed=FALSE)$vector
prMaxi<-mean(pagerank[ind1]) 
prMini<-mean(pagerank[ind2]) 
prConf<-mean(pagerank[ind3]) 
prAnti<-mean(pagerank[ind4]) 


#eigen
eigenvertex<-eigen$vector
eigenMaxi<-mean(eigenvertex[ind1]) 
eigenMini<-mean(eigenvertex[ind2]) 
eigenConf<-mean(eigenvertex[ind3]) 
eigenAnti<-mean(eigenvertex[ind4]) 

#close
closenessvertex<-closeness$res
closeMaxi<-mean(closenessvertex[ind1]) 
closeMini<-mean(closenessvertex[ind2]) 
closeConf<-mean(closenessvertex[ind3]) 
closeAnti<-mean(closenessvertex[ind4]) 

#changes
changes<-V(g)$N.CHANGES
changesMaxi<-mean(changes[ind1]) 
changesMini<-mean(changes[ind2]) 
changesConf<-mean(changes[ind3]) 
changesAnti<-mean(changes[ind4]) 

#timerule
timerule<-V(g)$TIME.RULE
timeruleMaxi<-mean(timerule[ind1]) 
timeruleMini<-mean(timerule[ind2]) 
timeruleConf<-mean(timerule[ind3]) 
timeruleAnti<-mean(timerule[ind4]) 

#timebehavior
timebehavior<-V(g)$TIME.BEHAVIOR
timebehaviorMaxi<-mean(timebehavior[ind1]) 
timebehaviorMini<-mean(timebehavior[ind2]) 
timebehaviorConf<-mean(timebehavior[ind3]) 
timebehaviorAnti<-mean(timebehavior[ind4]) 


#bonacich
bonacich<-bonpow(g)
bonacichMaxi<-mean(bonacich[ind1]) 
bonacichMini<-mean(bonacich[ind2]) 
bonacichConf<-mean(bonacich[ind3]) 
bonacichAnti<-mean(bonacich[ind4]) 

#eccentricity
eccentricity<-eccentricity(g)
eccentricityMaxi<-mean(eccentricity[ind1]) 
eccentricityMini<-mean(eccentricity[ind2]) 
eccentricityConf<-mean(eccentricity[ind3]) 
eccentricityAnti<-mean(eccentricity[ind4]) 

#authority
authority<-authority.score(g)$vector
authorityMaxi<-mean(authority[ind1]) 
authorityMini<-mean(authority[ind2]) 
authorityConf<-mean(authority[ind3]) 
authorityAnti<-mean(authority[ind4]) 

#hub
hub<-hub.score(g)$vector
hubMaxi<-mean(hub[ind1]) 
hubMini<-mean(hub[ind2]) 
hubConf<-mean(hub[ind3]) 
hubAnti<-mean(hub[ind4]) 

##neighbor stuff
allneighbors<-lapply( (1:NumAgents), function(x) neighbors(g,x))


#knn (degree of neighbors)
averageNNdegree<-knn$knn
averageNNdegreeMaxi<-mean(averageNNdegree[ind1]) 
averageNNdegreeMini<-mean(averageNNdegree[ind2]) 
averageNNdegreeConf<-mean(averageNNdegree[ind3]) 
averageNNdegreeAnti<-mean(averageNNdegree[ind4]) 
SDNNdegreeMaxi<-sd(averageNNdegree[ind1]) 
SDNNdegreeMini<-sd(averageNNdegree[ind2]) 
SDNNdegreeConf<-sd(averageNNdegree[ind3]) 
SDNNdegreeAnti<-sd(averageNNdegree[ind4]) 


#ec of neighbors
#eigenvertex<-eigen$vector
x<-unlist(lapply(allneighbors,function(x) mean(eigenvertex[x])))
eigenNNmaxi<-mean(x[ind1])
eigenNNmini<-mean(x[ind2])
eigenNNconf<-mean(x[ind3])
eigenNNanti<-mean(x[ind4])
SDeigenNNmaxi<-sd(x[ind1])
SDeigenNNmini<-sd(x[ind2])
SDeigenNNconf<-sd(x[ind3])
SDeigenNNanti<-sd(x[ind4])


#bc of neighbors
#betweenvertex<-between$res
x<-unlist(lapply(allneighbors,function(x) mean(betweenvertex[x])))
betweenNNmaxi<-mean(x[ind1])
betweenNNmini<-mean(x[ind2])
betweenNNconf<-mean(x[ind3])
betweenNNanti<-mean(x[ind4])
SDbetweenNNmaxi<-sd(x[ind1])
SDbetweenNNmini<-sd(x[ind2])
SDbetweenNNconf<-sd(x[ind3])
SDbetweenNNanti<-sd(x[ind4])

#cc of neighbors
#clustering<-transitivity(g,type="local",isolates="zero")
x<-unlist(lapply(allneighbors,function(x) mean(clustering[x])))
clusterNNmaxi<-mean(x[ind1])
clusterNNmini<-mean(x[ind2])
clusterNNconf<-mean(x[ind3])
clusterNNanti<-mean(x[ind4])
SDclusterNNmaxi<-sd(x[ind1])
SDclusterNNmini<-sd(x[ind2])
SDclusterNNconf<-sd(x[ind3])
SDclusterNNanti<-sd(x[ind4])


#pr of neighbors
#pagerank<-page.rank(g,directed=FALSE)$vector
x<-unlist(lapply(allneighbors,function(x) mean(pagerank[x])))
prNNmaxi<-mean(x[ind1])
prNNmini<-mean(x[ind2])
prNNconf<-mean(x[ind3])
prNNanti<-mean(x[ind4])
SDprNNmaxi<-sd(x[ind1])
SDprNNmini<-sd(x[ind2])
SDprNNconf<-sd(x[ind3])
SDprNNanti<-sd(x[ind4])


#closeness of neighbors
#closenessvertex<-closeness$res
x<-unlist(lapply(allneighbors,function(x) mean(closenessvertex[x])))
closeNNmaxi<-mean(x[ind1])
closeNNmini<-mean(x[ind2])
closeNNconf<-mean(x[ind3])
closeNNanti<-mean(x[ind4])
SDcloseNNmaxi<-sd(x[ind1])
SDcloseNNmini<-sd(x[ind2])
SDcloseNNconf<-sd(x[ind3])
SDcloseNNanti<-sd(x[ind4])


#bonacich neighbors
#bonacich<-bonpow(g)
x<-unlist(lapply(allneighbors,function(x) mean(bonacich[x])))
bonacichNNmaxi<-mean(x[ind1])
bonacichNNmini<-mean(x[ind2])
bonacichNNconf<-mean(x[ind3])
bonacichNNanti<-mean(x[ind4])
SDbonacichNNmaxi<-sd(x[ind1])
SDbonacichNNmini<-sd(x[ind2])
SDbonacichNNconf<-sd(x[ind3])
SDbonacichNNanti<-sd(x[ind4])

#eccentricity of neighbors
#eccentricity<-eccentricity(g)
x<-unlist(lapply(allneighbors,function(x) mean(eccentricity[x])))
eccentricityNNmaxi<-mean(x[ind1])
eccentricityNNmini<-mean(x[ind2])
eccentricityNNconf<-mean(x[ind3])
eccentricityNNanti<-mean(x[ind4])
SDeccentricityNNmaxi<-sd(x[ind1])
SDeccentricityNNmini<-sd(x[ind2])
SDeccentricityNNconf<-sd(x[ind3])
SDeccentricityNNanti<-sd(x[ind4])

#authority neighbors
#authority<-authority.score(g)$vector
x<-unlist(lapply(allneighbors,function(x) mean(authority[x])))
authorityNNmaxi<-mean(x[ind1])
authorityNNmini<-mean(x[ind2])
authorityNNconf<-mean(x[ind3])
authorityNNanti<-mean(x[ind4])
SDauthorityNNmaxi<-sd(x[ind1])
SDauthorityNNmini<-sd(x[ind2])
SDauthorityNNconf<-sd(x[ind3])
SDauthorityNNanti<-sd(x[ind4])


#hub neighbors
#hub<-hub.score(g)$vector
x<-unlist(lapply(allneighbors,function(x) mean(hub[x])))
hubNNmaxi<-mean(x[ind1])
hubNNmini<-mean(x[ind2])
hubNNconf<-mean(x[ind3])
hubNNanti<-mean(x[ind4])
SDhubNNmaxi<-sd(x[ind1])
SDhubNNmini<-sd(x[ind2])
SDhubNNconf<-sd(x[ind3])
SDhubNNanti<-sd(x[ind4])

Reach2<-reach2(g)
Reach3<-reach3(g)
DWreach<-dwreach(g)


###GLOBAL PROPERTIES
assortativityg<-assortativity.degree(g,directed=FALSE)
diameter<-diameter(g, directed=FALSE)
radius<-radius(g)
girth<-girth(g)$girth
density<-graph.density(g)
meanPath<-average.path.length(g,directed=FALSE)
globalClustering<-transitivity(g,type="undirected")
closenessgraph<-closeness$centralization
betweengraph<-between$centralization
eigengraph<-eigen$centralization


###create data.frame
lista<-ls()
index1<-which(unlist(lapply(lista,function(x) length(get(x))))==1)
index2<-which(unlist(lapply(lista,function(x) class(get(x))))=="numeric")
lista<-lista[intersect(index1,index2)]
data<-cbind(get(lista[1]),get(lista[2]))
for(i in 3:length(lista))
{
data<-cbind(data,get(lista[i]))  
}
data<-as.data.frame(data)
names(data)<-lista
write.csv(data,file="data.csv")



#y<-get.adjacency(g)
#centrality<-graphcent(g)
#load<-loadcent(as.matrix(y))
#prestige<-prestige(as.matrix(y))
#stress<-stresscent(as.matrix(y))
#information
#information<-infocent(g)
#informationMaxi<-mean(information[ind1]) 
#informationMini<-mean(information[ind2]) 
#informationConf<-mean(information[ind3]) 
#informationAnti<-mean(information[ind4]) 
#centralgraph<-central$centralization
#smallworld<-smallworldness(g)
#smallworld<-smallworld[1]
#global
#articulationPoints<-articulation.points(g)


#library(sna)
#library(qgraph)


#suffling 
#########################################
#do dataframe and plotting functions 
#do csv

#OTHER POSSIBLE MEASURES
#averageknnk<-knn$knnk
##similarity among vertices depending on connections 
#similarity.jaccard comon neighbors divides by vertices of both 
#similarity.dice 2x number of common neighbors over the sum of degrees of vertices 
#similarity.invlogweighted common neighbors weigthed by the inverse log of their degrees 
#SIR model and outputs summary for gra
#diversity<-graph.diversity(g,weights=rep(1,length(E(g))))
#eigenvalues of matrix?
#famous graphs?
#edge.betweenness.community
#fast.greedy.community
#infomap community
#graph.complementer
#independent vertex set
#label.propagation.community
#leading-eigenvector.community
#modularity
#multilevel.community
#power.law.fit
#degree.distribution(g,cumulative=TRUE)
#assortativity
#assortativity.nominal #to calculate how rules are related
#vertex.attributes(g)
#cliques
#communities
#modified versions of betweeness and clustering for flow of energy acc to fleischer and brenes 
#flow betweenness takes forever tocompute on 100 nodes.. flowbet(as.matrix(y))


