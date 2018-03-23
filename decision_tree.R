data=read.csv(file.choose())
str(data)
data$activity = as.factor(data$activity)
data$date = as.Date(data$date, "%Y-%m-%d")
data$Year = as.integer(format(data$date, "%Y"))
data$Month = as.integer(format(data$date, "%m"))
data$Day = as.integer(format(data$date, "%d"))

str(data)

 
summary(data)
data=data[,-4]
library(ggplot2)

# Distribution of activity
activity_count = group_by(data, activity) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

ggplot(activity_count, aes(x = activity, y = count)) +
  geom_bar(stat = "identity", fill = "blue") + 
  xlab("Activity") + 
  ylab("Count") +
  geom_text(aes(label = count), vjust = -0.3, size = 4.5) +
  ggtitle("Distribution of the Activity")

# Distribution of the acceleration
ggplot(data, aes(x = acceleration_x)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue") + 
  geom_density(alpha = 0.2, fill = "red") +
  xlab("Acceleration X") + 
  ylab("Count") +
  ggtitle("Distribution of the acceleration X values") 

ggplot(data, aes(x = acceleration_y)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue") + 
  geom_density(alpha = 0.2, fill = "red") +
  xlab("Acceleration Y") + 
  ylab("Count") +
  ggtitle("Distribution of the acceleration y values") 

ggplot(data, aes(x = acceleration_z)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue") + 
  geom_density(alpha = 0.2, fill = "red") +
  xlab("Acceleration Z") + 
  ylab("Count") +
  ggtitle("Distribution of the acceleration z values") 

# Distribution of gyro
ggplot(data, aes(x = gyro_x)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue") + 
  geom_density(alpha = 0.2, fill = "red") +
  xlab("Gyro X") + 
  ylab("Count") +
  ggtitle("Distribution of the gyro x values") 

ggplot(data, aes(x = gyro_y)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue") + 
  geom_density(alpha = 0.2, fill = "red") +
  xlab("Gyro Y") + 
  ylab("Count") +
  ggtitle("Distribution of the gyro y values") 

ggplot(data, aes(x = gyro_z)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "blue") + 
  geom_density(alpha = 0.2, fill = "red") +
  xlab("Gyro Z") + 
  ylab("Count") +
  ggtitle("Distribution of the gyro z values")

# Splitting the data into a training set and a testing set
rtrain=data[1:30000,]
rtest=data[30001:88588,]

library(party)
result=ctree(activity ~ acceleration_x + acceleration_y + acceleration_z +
gyro_x + gyro_y + gyro_z)
plot(result)
pred=predict(result,rtest)
table(pred,rtest$activity)
