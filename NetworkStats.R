###need to verify conflicts with libraries 

library(igraph)
#library(sna)
#library(qgraph)

g<-read.graph("graph.graphml" , format="graphml")


###OUTPUTS
#previous steps
ind1<-which(V(g)$RULE==1)
ind2<-which(V(g)$RULE==2)
ind3<-which(V(g)$RULE==3)
ind4<-which(V(g)$RULE==4)
NumAgents<-length(V(g))
closeness<-centralization.closeness (g)
knn<-graph.knn(g)
triangles<-adjacent.triangles(g)
vertexConectivity<-vertex.connectivity(g)
between<-centralization.betweenness (g,directed=FALSE)
eigen<-centralization.evcent (g)
degrees<-degree(g)
degreesNormal<-degree(g,normalized=TRUE)
y<-get.adjacency(g)

information<-infocent(as.matrix(y))
centrality<-graphcent(as.matrix(y))
load<-loadcent(as.matrix(y))
prestige<-prestige(as.matrix(y))
stress<-stresscent(as.matrix(y))

# from http://www.shizukalab.com/toolkits/sna/node-level-calculations
reach2=function(x){
  r=vector(length=vcount(x))
  for (i in 1:vcount(x)){
    n=neighborhood(x,2,nodes=i)
    ni=unlist(n)
    l=length(ni)
    r[i]=(l)/vcount(x)}
  r}


# from http://www.shizukalab.com/toolkits/sna/node-level-calculations
reach3=function(x){
  r=vector(length=vcount(x))
  for (i in 1:vcount(x)){
    n=neighborhood(x,3)
    ni=unlist(n)
    l=length(ni)
    r[i]=(l)/vcount(x)}
  r}

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
maxi<-length(ind1)/NumAgents
mini<-length(ind2)/NumAgents
conf<-length(ind3)/NumAgents
anti<-length(ind4)/NumAgents

#cooperation
ind<-which(V(g)$COOPERATE?==TRUE)
cooperationRate<- length(ind) / NumAgents
coopMaxi<-length(intersect(ind,ind1))/length(ind1)
coopMini<-length(intersect(ind,ind2))/length(ind2)
coopConf<-length(intersect(ind,ind3))/length(ind3)
coopAnti<-length(intersect(ind,ind4))/length(ind4)
gcoopMaxi<-length(intersect(ind,ind1))/NumAgents
gcoopMini<-length(intersect(ind,ind2))/NumAgents
gcoopConf<-length(intersect(ind,ind3))/NumAgents
gcoopAnti<-length(intersect(ind,ind4))/NumAgents

#satisfaction 
satisfactionRate<-mean(V(g)$SATISFACTION2)
satMaxi<-mean(V(g)$SATISFACTION2[ind1]) 
satMini<-mean(V(g)$SATISFACTION2[ind2]) 
satConf<-mean(V(g)$SATISFACTION2[ind3]) 
satAnti<-mean(V(g)$SATISFACTION2[ind4]) 

#degree 
degreeMaxi<-mean(degree[ind1]) 
degreeMini<-mean(degree[ind2]) 
degreeConf<-mean(degree[ind3]) 
degreeAnti<-mean(degree[ind4]) 
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
changes<-V(g)$N-CHANGES
changesMaxi<-mean(changes[ind1]) 
changesMini<-mean(changes[ind2]) 
changesConf<-mean(changes[ind3]) 
changesAnti<-mean(changes[ind4]) 

#timerule
timerule<-V(g)$timerule
timeruleMaxi<-mean(timerule[ind1]) 
timeruleMini<-mean(timerule[ind2]) 
timeruleConf<-mean(timerule[ind3]) 
timeruleAnti<-mean(timerule[ind4]) 

#timebehavior
timebehavior<-V(g)$timebehavior
timebehaviorMaxi<-mean(timebehavior[ind1]) 
timebehaviorMini<-mean(timebehavior[ind2]) 
timebehaviorConf<-mean(timebehavior[ind3]) 
timebehaviorAnti<-mean(timebehavior[ind4]) 

#information
informationMaxi<-mean(information[ind1]) 
informationMini<-mean(information[ind2]) 
informationConf<-mean(information[ind3]) 
informationAnti<-mean(information[ind4]) 

#bonacich
bonacich<-bonpow(as.matrix(y))
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
authority<-V(g)$authority
authorityMaxi<-mean(authority[ind1]) 
authorityMini<-mean(authority[ind2]) 
authorityConf<-mean(authority[ind3]) 
authorityAnti<-mean(authority[ind4]) 

#hub
hub<-hub.score(g)$vector
hub<-V(g)$hub
hubMaxi<-mean(hub[ind1]) 
hubMini<-mean(hub[ind2]) 
hubConf<-mean(hub[ind3]) 
hubAnti<-mean(hub[ind4]) 

##neighbor stuff
#knn (degree of neighbors)
averageNNdegree<-knn$knn
averageNNdegreeMaxi<-mean(averageNNdegree[ind1]) 
averageNNdegreeMini<-mean(averageNNdegree[ind2]) 
averageNNdegreeConf<-mean(averageNNdegree[ind3]) 
averageNNdegreeAnti<-mean(averageNNdegree[ind4]) 

#ec of neighbors
#bc of neighbors
#cc of neighbors
#cc of neighbors
#ec of neighbors

#global
#articulationPoints<-articulation.points(g)
assortativityg<-assortativity.degree(g,directed=FALSE)
diameter<-diameter(g, directed=FALSE)
radius<-radius(g)
girth<-girth(g)$girth
density<-graph.density(g)
meanPath<-average.path.length(g,directed=FALSE)
globalClustering<-transitivity(g,type="undirected")
closenessgraph<-closeness$centralization
centralgraph<-central$centralization
betweengraph<-between$centralization
eigengraph<-eigen$centralization

smallworld<-smallworldness(g)
smallworldness<-smallworld[1]
smallworldness<-smallworld[1]

#########################################
#sds?
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

write.csv(c,file="betweenness.csv")

