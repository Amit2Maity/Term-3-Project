library(lattice)
library(tidyr)
library(dplyr)
library(car)
library(fastDummies)
library(caret)
library(randomForest)
library(corrplot)


customer_data = read.csv('F:/Capstone/DS3/term3.csv', stringsAsFactors = F)

customer_data$Reached.on.Time_Y.N = as.factor(customer_data$Reached.on.Time_Y.N)

summary(customer_data)

dim(customer_data)


##Missing value

View(colSums(is.na(customer_data)))

#There are no missing value, so treatment is not required

#To maintain same style for all plots
bold.16.text <- element_text(face = "bold", size = 16)

mosaicplot(customer_data$Mode_of_Shipment~customer_data$Reached.on.Time_Y.N, color='skyblue')


ggplot(customer_data, aes(x=Reached.on.Time_Y.N, y=Discount_offered, fill=Reached.on.Time_Y.N)) +  
  geom_boxplot()+
  scale_fill_discrete(name = "Reached on Time", labels = c("Yes","No"))+
  xlab("Reached on Time")+ ylab("Discount Offered")+
  theme(axis.title= bold.16.text,axis.text.x = bold.16.text)

ggplot(customer_data, aes(x=Reached.on.Time_Y.N, y=Weight_in_gms, fill=Reached.on.Time_Y.N)) +  
  geom_boxplot()+
  scale_fill_discrete(name = "Reached on Time", labels = c("Yes","No"))+
  xlab("Reached on Time")+ ylab("Weight in gms")+
  theme(axis.title= bold.16.text,axis.text.x = bold.16.text) 

ggplot(customer_data, aes(x=Reached.on.Time_Y.N, y=Customer_care_calls, fill=Reached.on.Time_Y.N)) +  
  geom_boxplot()+
  scale_fill_discrete(name = "Reached on Time", labels = c("Yes","No"))+
  xlab("Reached on Time")+ ylab("Customer care calls")+
  theme(axis.title= bold.16.text,axis.text.x = bold.16.text)

ggplot(customer_data, aes(x=as.factor(Prior_purchases), y=Discount_offered, 
color=as.factor(Prior_purchases))) +  geom_boxplot()+
  xlab("Prior Purchase")+ ylab("Discount Offered")+
  theme(axis.title= bold.16.text,axis.text.x = bold.16.text)+
  theme(legend.position = "none")




shipment_count = customer_data %>% group_by(Mode_of_Shipment) %>%  summarise(counts=n())

barplot(shipment_count$counts, names.arg = shipment_count$Mode_of_Shipment,col="blue",
        border="red",axis.lty="solid",xlab="Mode of Shipment",ylab="Counts of Items")
colors = c("green","orange","brown")


timimg_wrt_shipment = table(customer_data$Reached.on.Time_Y.N,customer_data$Mode_of_Shipment)

timimg_wrt_shipment.d = as.data.frame(timimg_wrt_shipment)
colnames(timimg_wrt_shipment.d) <- c("Reached_on_Time","mode_of_shipment","Frequency")

ggplot(timimg_wrt_shipment.d, aes(mode_of_shipment, Frequency, fill = Reached_on_Time)) + 
  geom_bar(stat="identity", position = "dodge") + 
  xlab("Mode of Shipment")+ ylab("Counts of Items")+
  scale_fill_discrete(name = "Reached on Time", labels = c("Yes","No"))+
  theme(axis.title= bold.16.text,axis.text.x = bold.16.text)

#scale_fill_brewer(palette = "Set1")+

Dscnt_smry = customer_data %>% group_by(Reached.on.Time_Y.N) %>% summarise(Avg_dscnt = mean(Discount_offered))

ggplot(Dscnt_smry, aes(c("Yes","No"), Avg_dscnt)) + 
  geom_bar(stat="identity",width = .5, fill = "orange", color = "black") + 
  xlab("Reached on Time")+ ylab("Average Discount")+
  theme(axis.title= bold.16.text,axis.text.x = bold.16.text)+
  geom_text(aes(label = round(Avg_dscnt,1)), size = 5,hjust = 0.5, vjust = 3)


Weight_smry = customer_data %>% group_by(Reached.on.Time_Y.N) %>% summarise(Avg_weight = mean(Weight_in_gms))

ggplot(Weight_smry, aes(c("Yes","No"), Avg_weight)) + 
  geom_bar(stat="identity",width = .5, fill = "orange", color = "black") + 
  xlab("Reached on Time")+ ylab("Average Weight (gms)")+
  theme(axis.title= bold.16.text,axis.text.x = bold.16.text)+
  geom_text(aes(label = round(Avg_weight,1)), size = 5,hjust = 0.5, vjust = 3)


cust_smry = table(customer_data$Reached.on.Time_Y.N,customer_data$Customer_care_calls)

cust_smry = as.data.frame(cust_smry)

colnames(cust_smry) = c("Reached_on_Time", "Customer_care_calls","Frequency")

ggplot(cust_smry, aes(Customer_care_calls, Frequency, fill = Reached_on_Time)) + 
  geom_bar(stat="identity", position = "dodge") + 
  xlab("Number of customer care calls")+ ylab("Counts of Items Purchased")+
  scale_fill_discrete(name = "Reached on Time", labels = c("Yes","No"))+
  theme(axis.title= bold.16.text,axis.text.x = bold.16.text)

importance_smry = table(customer_data$Product_importance,customer_data$Reached.on.Time_Y.N)

importance_smry = as.data.frame(importance_smry)

colnames(importance_smry) = c("Product_importance", "Reached_on_Time","Frequency")

ggplot(importance_smry, aes(Product_importance, Frequency, fill = Reached_on_Time)) + 
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_discrete(name = "Reached on Time", labels = c("Yes","No"))+
  theme(axis.title= bold.16.text,axis.text.x = bold.16.text)+
  xlab("Importance of Products")+ ylab("Number of Items")+
  geom_text(aes(label = Frequency),position = position_dodge(0.9), size = 5,hjust = 0.5, vjust = 2)

