# Hierachical Clustering and Kmeans Clustering
3.The file eupop.txt contains the population and percentage distribution by age for EU countries in 1999. The age categories are 0-14 years, 15-44 years, 45-64 years and 65 years and over. eupop <- read.table(“yourfolder/eupop.txt”, header=T, row.names=1) eupop <- eupop[,-5]

(a) Construct the euclidean distance matrix of the percentage variables. Use it to cluster the countries, using average linkage. Draw the dendrogram and interpret. Are there any outlier countries?

setwd("E:/GitHub/MachineLearning/assignment/1")
eupop <- read.table("eupop.txt", header=T, row.names=1)
eupop <- eupop[ ,-5]
eumatrix <- as.matrix(dist(eupop,method = "euclidean"))
h <- hclust(as.dist(eumatrix),method = "average")
d1 <- as.dendrogram(h)
plot(as.dendrogram(d1))


# Interpretation :
# Country Ireland is outlier since it is at largest distance from other cluster groupings.
# France and UK are in same cluster that is they have similar distribution of population for different age group.
# Austria and portugal are in same cluster and it is closely releated to the cluster formed between france and UK.It means their population distribution for different age group are closely releated.
(b) Examine the 3-cluster solution. Which countries belong to each of the three clusters? Summarise the partitions with sumPartition (in h1code.R) Interpret your findings.

cutree(h,3)
##     Austria     Belgium     Denmark     Finland      France  Luxembourg 
##           1           1           1           1           1           1 
## Netherlands    Portugal      Sweden          UK     Germany      Greece 
##           1           1           1           1           2           2 
##       Italy       Spain     Ireland 
##           2           2           3
# Interpretation
# Austria,Belgium,Denmark,Finland,France,Luxemborg,Netherlands,Portugal,Sweden,Uk belongs to cluster 1.There exists ten cluster in cluster 1.
#Germany,Greece,Italy,Spain belongs to cluster 2.There are four countries in same cluster.
#There is only one country Ireland in clutser 3.

source("sumPartition.R")
sumPartition(eupop, cutree(h,3))
## Final Partition
## 
## Number of clusters  3
## 
##           N.obs Within.clus.SS Ave.dist..Centroid Max.dist.centroid
## Cluster 1    10         55.305           2.211730          4.030199
## Cluster 2     4         17.535           1.831295          3.117090
## Cluster 3     1          0.000           0.000000          0.000000
## 
## 
## Cluster centroids
## 
##       Cluster 1 Cluster 2 Cluster 3 Grand centrd
## p014  18.23     15.25     22.2      17.7        
## p1544 42.52     43.775    46.2      43.1        
## p4564 23.94     24.25     20.3      23.78       
## p65.  15.36     16.725    11.3      15.45333    
## 
## 
## Distances between Cluster centroids
## 
##           Cluster 1 Cluster 2 Cluster 3
## Cluster 1  0.000000  3.523457  7.683521
## Cluster 2  3.523457  0.000000  9.960735
## Cluster 3  7.683521  9.960735  0.000000
#Interpretation
# cluster 1 and 2 are the closest to eah other and cluster 2 and 3 are farthest distance.
# cluster 1 is spread out and cluster 2 is less spread out.
# cluster 1 and 3 centroid are above grand centroid while cluster 2 are above centroid.
# In all the clusters the centroid value is higher for age group 15-44,hence the young population distribution of age group 15-44 years are more in cluster 2.
# Ireland has the highest number of young people.
(c) Use the kmeans algorithm to find another 3-cluster grouping of countries. Which countries belong to each of the three clusters?

set.seed(42)
kmeanseuc<- kmeans(eupop, centers = 3,nstart=25)
kmeanseuc
## K-means clustering with 3 clusters of sizes 6, 1, 8
## 
## Cluster means:
##       p014    p1544    p4564     p65.
## 1 15.81667 44.03333 23.91667 16.26667
## 2 22.20000 46.20000 20.30000 11.30000
## 3 18.55000 42.01250 24.11250 15.36250
## 
## Clustering vector:
##     Austria     Belgium     Denmark     Finland      France  Luxembourg 
##           1           3           3           3           3           3 
## Netherlands    Portugal      Sweden          UK     Germany      Greece 
##           3           1           3           3           1           1 
##       Italy       Spain     Ireland 
##           1           1           2 
## 
## Within cluster sum of squares by cluster:
## [1] 26.38333  0.00000 39.37625
##  (between_SS / total_SS =  61.7 %)
## 
## Available components:
## 
## [1] "cluster"      "centers"      "totss"        "withinss"    
## [5] "tot.withinss" "betweenss"    "size"         "iter"        
## [9] "ifault"
## Interpretation 
## 6 Countries associated with cluster 1 are austria,portugal,Germany,Greece,Italy,Spain.It means they are similar.
## Countries associated with cluster 2 are Ireland only.It states that there are no country which are simliar to ireland.
## 8 Countries associated with cluster 3 are similar to each other.They are Belgium,Sweden,Denmark,UK,Finland,France,Luxemborg,Netherlands.
(d) Construct a stars plot which shows the data and clustering obtained from kmeans.

Optional: can you think of a better way of showing the clusters?Can you think of a way to present the data and the clustering results of both methods on the same graphical display?

clusk <- kmeanseuc$cluster
o <- order(clusk)
stars(eupop[o,],nrow=3,col.stars=clusk[o]+1)


####optional
## Data and clustering results of both methods on same plot
pairs(eupop,col=kmeanseuc$cluster,pch=cutree(h,3)-1)


## Displaying results of both clustering on same graphical display
par(mfrow=c(1,2))
stars(eupop[o,],nrow=4,col.stars=clusk[o]+1,main = "Kmeans Clustering")
plot(as.dendrogram(d1),main="Hierarchical Clustering")


4. Music data from class.

(a) Run the k-means algorithm over the range k = 1; : : : ; 15 clusters and record the total within cluster sum of squares (TWSS). Let nstart = 25. Plot k versus TWSS and choose the best fitting number of clusters. What do you observe? Note: remember to scale the data.

musicd <- read.csv("music.csv", header=T, row.names=1)
head(musicd)
##               Artist Type     LVar      LAve  LMax   LFEner     LFreq
## Dancing Queen   Abba Rock 17600756 -90.00687 29921 105.9210  59.57379
## Knowing Me      Abba Rock  9543021 -75.76672 27626 102.8362  58.48031
## Take a Chance   Abba Rock  9049482 -98.06292 26372 102.3249 124.59397
## Mamma Mia       Abba Rock  7557437 -90.47106 28898 101.6165  48.76513
## Lay All You     Abba Rock  6282286 -88.95263 27940 100.3008  74.02039
## Super Trouper   Abba Rock  4665867 -69.02084 25531 100.2485  81.40140
musics <- musicd[ ,-(1:2)]
head(musics)
##                   LVar      LAve  LMax   LFEner     LFreq
## Dancing Queen 17600756 -90.00687 29921 105.9210  59.57379
## Knowing Me     9543021 -75.76672 27626 102.8362  58.48031
## Take a Chance  9049482 -98.06292 26372 102.3249 124.59397
## Mamma Mia      7557437 -90.47106 28898 101.6165  48.76513
## Lay All You    6282286 -88.95263 27940 100.3008  74.02039
## Super Trouper  4665867 -69.02084 25531 100.2485  81.40140
musics <- scale(musics)
head(musics)
##                      LVar      LAve      LMax     LFEner      LFreq
## Dancing Queen -0.08896444 -1.740926 0.8486157  0.3449916 -0.9724220
## Knowing Me    -0.39387339 -1.439330 0.5866691 -0.2176714 -0.9786106
## Take a Chance -0.41254918 -1.911548 0.4435401 -0.3109284 -0.6044384
## Mamma Mia     -0.46900893 -1.750758 0.7318526 -0.4401399 -1.0335940
## Lay All You   -0.51726134 -1.718598 0.6225084 -0.6801261 -0.8906611
## Super Trouper -0.57842749 -1.296458 0.3475501 -0.6896619 -0.8488880
k <- 1:15
kcluster <- lapply(k,function(i) kmeans(musics, centers=i,  nstart = 25))
twss <- lapply(k,function(i) kmeans(musics, centers=i,  nstart = 25)$tot.withinss)  
twssv <- rapply(twss,c)
plot(k,twssv,type="b",xlab = "Number Of Cluster",ylab = "Total Sum of Squares")


## As per plot TWSS is changing slowly at k=4,5 but the after k=5 ,the change is not significant.Hence the best fitting number of cluster is 5.
(b) Make a table of artist vs cluster solution from k = 5

datanew <- kmeans(musics, centers=5,  nstart = 25)
table(musicd$Artist,datanew$cluster)
##            
##             1 2 3 4 5
##   Abba      0 1 9 0 0
##   Beatles   8 0 2 0 0
##   Beethoven 0 1 2 5 0
##   Eels      7 0 3 0 0
##   Enya      0 2 1 0 0
##   Mozart    0 0 0 6 0
##   Vivaldi   0 3 1 5 1
5. Protein data. We want to study the similarities and differences in the protein composition of the diets of different countries. Using any methods that you choose from this course or otherwise, write a brief summary.

# Kmeans Clustering analysis 

# Reading the data of protein
proteind <- read.csv("protein.CSV", header=T, row.names=1)
head(proteind)
##                RedMeat WhiteMeat Eggs Milk Fish Cereals Starch Nuts Fr.Veg
## Albania           10.1       1.4  0.5  8.9  0.2    42.3    0.6  5.5    1.7
## Austria            8.9      14.0  4.3 19.9  2.1    28.0    3.6  1.3    4.3
## Belgium           13.5       9.3  4.1 17.5  4.5    26.6    5.7  2.1    4.0
## Bulgaria           7.8       6.0  1.6  8.3  1.2    56.7    1.1  3.7    4.2
## Czechoslovakia     9.7      11.4  2.8 12.5  2.0    34.3    5.0  1.1    4.0
## Denmark           10.6      10.8  3.7 25.0  9.9    21.9    4.8  0.7    2.4
# Standardizing/scaling the data of protein
proteins <- scale(proteind)

#Finding optimal number of clusters for kmeans
k1 <- 1:15
twss <- lapply(k1,function(i) kmeans(proteins, centers=i,  nstart = 25)$tot.withinss)  
twssv <- rapply(twss,c)
plot(k1,twssv,type="b",xlab = "Number Of Cluster",ylab = "Total Sum of Squares")


# As per plot the optimal cluster seems to be 6 as TWSS is not increasing significantly.The other two possible candidates are 5 and 7 clusters.

# Analysis of kmeans with 6 number of cluster
set.seed(1)
analysis <- kmeans(proteins, centers=6,  nstart = 25)
v <- order(analysis$cluster)
stars(proteins[v,],nrow=4,col.stars=analysis$cluster[v]+1)


analysis$cluster
##        Albania        Austria        Belgium       Bulgaria Czechoslovakia 
##              3              5              5              3              1 
##        Denmark    EastGermany        Finland         France         Greece 
##              6              1              6              5              4 
##        Hungary        Ireland          Italy    Netherlands         Norway 
##              1              5              4              5              6 
##         Poland       Portugal        Romania          Spain         Sweden 
##              1              2              3              2              6 
##    Switzerland             UK           USSR    WestGermany     Yugoslavia 
##              5              5              1              5              3
table(row.names(proteins),analysis$cluster)
##                 
##                  1 2 3 4 5 6
##   Albania        0 0 1 0 0 0
##   Austria        0 0 0 0 1 0
##   Belgium        0 0 0 0 1 0
##   Bulgaria       0 0 1 0 0 0
##   Czechoslovakia 1 0 0 0 0 0
##   Denmark        0 0 0 0 0 1
##   EastGermany    1 0 0 0 0 0
##   Finland        0 0 0 0 0 1
##   France         0 0 0 0 1 0
##   Greece         0 0 0 1 0 0
##   Hungary        1 0 0 0 0 0
##   Ireland        0 0 0 0 1 0
##   Italy          0 0 0 1 0 0
##   Netherlands    0 0 0 0 1 0
##   Norway         0 0 0 0 0 1
##   Poland         1 0 0 0 0 0
##   Portugal       0 1 0 0 0 0
##   Romania        0 0 1 0 0 0
##   Spain          0 1 0 0 0 0
##   Sweden         0 0 0 0 0 1
##   Switzerland    0 0 0 0 1 0
##   UK             0 0 0 0 1 0
##   USSR           1 0 0 0 0 0
##   WestGermany    0 0 0 0 1 0
##   Yugoslavia     0 0 1 0 0 0
# Interpretation
# Below countries are similar in protein composition of diets
# a)East germany ,czechoslovakia ,hungary,poland,USSR
# b) Portugal and spain
# c) Albania, Bulgaria ,Romania and yugoslavia (Here Albania have bit different structure and seems to be outlier)
# d) Greece and Italy
# e) Austria,Belgium,France,Netherlands,Switzerland,ireland ,UK,West Germany
# f) Denmark ,Finland,Norway and Sweden


aggregate(proteind, by=list(cluster=analysis$cluster), mean)
##   cluster RedMeat WhiteMeat   Eggs    Milk   Fish Cereals Starch   Nuts
## 1       1  7.9200   10.0400 2.8400 13.8400  2.740  35.740   5.56 2.5400
## 2       2  6.6500    3.5500 2.1000  6.7500 10.600  28.100   5.80 5.3000
## 3       3  7.1250    4.6750 1.2000  9.4500  0.750  51.125   1.95 5.0500
## 4       4  9.6000    4.0500 2.8500 15.6500  4.650  39.250   2.15 6.0500
## 5       5 13.2125   10.6375 3.9875 21.1625  3.375  24.700   4.65 2.0625
## 6       6  9.8500    7.0500 3.1500 26.6750  8.225  22.675   4.55 1.1750
##   Fr.Veg
## 1  4.260
## 2  7.550
## 3  2.975
## 4  6.600
## 5  4.175
## 6  2.125
# All the  countries in all the cluster are getting maximum protein from cereals while minimum protein content varies from cluter to cluster.

analysis$size
## [1] 5 2 4 2 8 4
# It displays the size of cluster
# The maximum size is 8 which is cluster 6 while minimum size of cluster is 2 present in cluster number 2 and 4.
