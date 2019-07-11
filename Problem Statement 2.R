library(lattice)
library(tidyr)
library(dplyr)
library(car)
library(fastDummies)
library(caret)
library(cluster)

#customer_data = read.csv('C:/NewCrest/customer Analytics/term3_train.csv', stringsAsFactors = F)

customer_data = read.csv('F:/Capstone/DS3/term3.csv', stringsAsFactors = F)

View(customer_data)

cust_score = customer_data[,c(4:8,12)]

cust_score$Product_importance_score = ifelse(cust_score$Product_importance == 'high', 1, ifelse(cust_score$Product_importance == 'medium', 2,3))
cust_score$Customer_care_calls_score = ifelse(cust_score$Customer_care_calls <= '3', 1, 
                                              ifelse(cust_score$Customer_care_calls > '3' & cust_score$Customer_care_calls <= '5', 2,3))

cust_score$Customer_rating_score = ifelse(cust_score$Customer_rating == '5', 1, 
                                              ifelse(cust_score$Customer_rating == '3' | cust_score$Customer_rating == '4', 2,3))


cust_score$Prior_purchases_score = ifelse(cust_score$Prior_purchases == '2', 3, 
                                          ifelse(cust_score$Prior_purchases > '2' & cust_score$Prior_purchases <= '5', 2,1))


cust_score$Cost_of_the_Product_score = ifelse(cust_score$Cost_of_the_Product <= '169', 3, 
                                          ifelse(cust_score$Cost_of_the_Product > '169' & cust_score$Cost_of_the_Product < '251', 2,1))

cust_score$customer_score = paste0(cust_score$Customer_rating_score,
                                   cust_score$Prior_purchases_score,
                                   cust_score$Customer_care_calls_score,
                                   cust_score$Product_importance_score,
                                   cust_score$Cost_of_the_Product_score)

# Select top 1000 customers based on generated customer score features
cust_score_sorted = head(cust_score[order(cust_score$customer_score),], n=1000)
View(cust_score_sorted)

#summarise the Top 1000 customers
Top_cust_smry = cust_score_sorted %>% group_by(Reached.on.Time_Y.N) %>% summarise(cnt = n())

#To maintain same style for all plots
bold.16.text <- element_text(face = "bold", size = 16)

ggplot(Top_cust_smry, aes(c("Yes","No"), cnt)) + 
  geom_bar(stat="identity",width = .5, fill = "orange", color = "black") + 
  xlab("Reached on Time")+ ylab("Number of Customers")+
  theme(axis.title= bold.16.text,axis.text.x = bold.16.text)+
  geom_text(aes(label = cnt), size = 5,hjust = 0.5, vjust = 3)


#############Clustering#################
cust_cluster = cust_score[,-c(5,6)]

cust_cluster = scale(cust_cluster)

library(permute)
library(lattice)
library(vegan)
fit <- cascadeKM(cust_cluster, 1, 10, iter = 1000) 
plot(fit, sortg = TRUE, grpmts.plot = TRUE)

calinski.best <- as.numeric(which.max(fit$results[2,])) 
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")

mydata <- cust_cluster 
#Determine the optimal cluster size based on within sum of squares 
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss) 

#Plot the elbow chart to determine optimal cluster 
plot(1:15, wss, type="b", xlab="Number of Clusters", 
     ylab="Within groups sum of squares",col="mediumseagreen",pch=12)

###Run the kmeans algorithm to generate the clusters 
k1<-kmeans(cust_cluster, 3)

cust_score$cluster  <- k1$cluster

###########################################################

pr4 <- pam(cust_cluster, 4)
str(si <- silhouette(pr4))
(ssi <- summary(si))
plot(si)
plot(si, col = c("red", "green", "blue", "purple"))
si2 <- silhouette(pr4$clustering, dist(cust_cluster, "canberra"))
summary(si2)
plot(si2, nmax= 80, cex.names=0.6)

op <- par(mfrow= c(3,2), oma= c(0,0, 3, 0),
          mgp= c(1.6,.8,0), mar= .1+c(4,2,2,2))
for(k in 2:6)
  plot(silhouette(pam(cust_cluster, k=k)), main = paste("k = ",k), do.n.k=FALSE)
mtext("PAM(cust_cluster) as in Kaufman & Rousseeuw, p.101",
      outer = TRUE, font = par("font.main"), cex = par("cex.main")); frame()

## the same with cluster-wise colours:
c6 <- c("tomato", "forest green", "dark blue", "purple2", "goldenrod4", "gray20")
for(k in 2:6)
  plot(silhouette(pam(cust_cluster, k=k)), main = paste("k = ",k), do.n.k=FALSE,
       col = c6[1:k])
par(op)
count(customer_data)
str(customer_data$Mode_of_Shipment)
