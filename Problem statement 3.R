library(lattice)
library(tidyr)
library(dplyr)
library(car)
library(fastDummies)
library(caret)

customer_data = read.csv('F:/Capstone/DS3/term3.csv', stringsAsFactors = F)
View(customer_data)

#Creating dummy variables
cust_df = fastDummies::dummy_cols(customer_data[,c(2:12)], remove_first_dummy = TRUE)

cust_df = cust_df[,-c(1,2,7,8)]

#The customers whom shipments are not reaching on time
cust_1 = cust_df %>% filter(cust_df$Reached.on.Time_Y.N == '1')

#scale the data to standardize
delayed_cust = scale(cust_1[,-c(7)])
dist.res=dist(delayed_cust, method='euclidean')

#Hierarchical clustering results
hc=hclust(dist.res, method='ward.D')

# Visualization of hclust
plot(hc, labels=FALSE, hang=-1)

# Add rectagle around 4 groups
rect.hclust(hc, k=2, border=2:4)

cut <- cutree(hc, 2)

table(cut)


#Elbow curve to find the number of clusters
wss=(nrow(delayed_cust)-1)*sum(apply(delayed_cust,2,var))
for(i in 2:15){wss[i]=sum(kmeans(delayed_cust, centers=i)$withinss)}
set.seed(20)
plot(1:15,wss, type="b", xlab="No. of Clusters", ylab="Within Group Sum of Squares ")

library(factoextra)
library(NbClust)

# K-means clustering
km.res <- eclust(delayed_cust, "kmeans", k = 6, nstart = 25, graph = FALSE)
# Visualize k-means clusters
fviz_cluster(km.res, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())

# Visualize silhouhette information
require("cluster")
sil <- silhouette(km.res$cluster, dist(delayed_cust))
fviz_silhouette(sil)

