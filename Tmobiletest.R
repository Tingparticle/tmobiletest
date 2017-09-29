#library(RMySQL)




setwd("/Users/ting/Desktop")

'''
con <- dbConnect(MySQL(), user = 'root', password = 'xu3xjp4j4xu3',
                 host = '127.0.0.1',
                 database = 'ttest')
'''


data <- read.table(file = "Test.csv", header = TRUE, sep = ",", quote = "\"") 
data <- data[!is.na(data$id),]
data$date_data_use <- scale(data$date_data_use)
data$date_call_no <- scale(data$date_call_no)
data$date_call_sec <- scale(data$date_call_sec)
data$date_net_call_no <- scale(data$date_net_call_no)
data$date_net_call_sec <- scale(data$date_net_call_sec)
data$people <- scale(data$people)
View(data)
write.table(data, file = "/Users/ting/Desktop/test_data.csv", sep = ","
          , quote = F)


################24######################
data_24 <- data[data$case == 24 ,]
View(data_24)

par(mfrow = c(1, 1))
plot(data_24$date_data_use, type = "l", xlim = c(0,10))
hist(data_24$date_data_use)

model_24 <- lm(data_24$date_data_use~ factor(data_24$holiday) +
                 factor(data_24$typhoon) + data_24$date_call_no +
                 data_24$date_call_sec + data_24$date_net_call_no +
                 data_24$date_net_call_sec + data_24$people, data = data_24)
model_24
summary(model_24)


################25######################
data_25 <- data[data$case == 25 ,]
View(data_25)
plot(data_25$id, data_25$date_data_use, type = "l", xlim = c(11,20))
hist(data_25$date_data_use)

model_25 <- lm(data_25$date_data_use~ factor(data_25$holiday) +
                 factor(data_25$typhoon) + data_25$date_call_no +
                 data_25$date_call_sec + data_25$date_net_call_no +
                 data_25$date_net_call_sec + data_25$people, data = data_25)
model_25
summary(model_25)



################26######################
data_26 <- data[data$case == 26 ,]
View(data_26)

model_26 <- lm(data_26$date_data_use~ factor(data_26$holiday) +
                 factor(data_26$typhoon) + data_26$date_call_no +
                 data_26$date_call_sec + data_26$date_net_call_no +
                 data_26$date_net_call_sec + data_26$people, data = data_26)
model_26
summary(model_26)




################27######################
data_27 <- data[data$case == 27 ,]
View(data_27)

model_27 <- lm(data_27$date_data_use~ factor(data_27$holiday) +
                 factor(data_27$typhoon) + data_27$date_call_no +
                 data_27$date_call_sec + data_27$date_net_call_no +
                 data_27$date_net_call_sec + data_27$people, data = data_27)
model_27
summary(model_27)



################28######################
data_28 <- data[data$case == 28 ,]
View(data_28)

model_28 <- lm(data_28$date_data_use~ factor(data_28$holiday) +
                 factor(data_28$typhoon) + data_28$date_call_no +
                 data_28$date_call_sec + data_28$date_net_call_no +
                 data_28$date_net_call_sec + data_28$people, data = data_28)
model_28
summary(model_28)



################29######################
data_29 <- data[data$case == 29 ,]
View(data_29)

model_29 <- lm(data_29$date_data_use~ factor(data_29$holiday) +
                 factor(data_29$typhoon) + data_29$date_call_no +
                 data_29$date_call_sec + data_29$date_net_call_no +
                 data_29$date_net_call_sec + data_29$people, data = data_29)
model_29
summary(model_29)




################30######################
data_30 <- data[data$case == 30 ,]
View(data_30)

model_30 <- lm(data_30$date_data_use~ factor(data_30$holiday) +
                 factor(data_30$typhoon) + data_30$date_call_no +
                 data_30$date_call_sec + data_30$date_net_call_no +
                 data_30$date_net_call_sec + data_30$people, data = data_30)
model_30
summary(model_30)



################31######################
data_31 <- data[data$case == 31 ,]
View(data_31)

model_31 <- lm(data_31$date_data_use~ factor(data_31$holiday) +
                 factor(data_31$typhoon) + data_31$date_call_no +
                 data_31$date_call_sec + data_31$date_net_call_no +
                 data_31$date_net_call_sec + data_31$people, data = data_31)
model_31
summary(model_31)



################32######################
data_32 <- data[data$case == 32 ,]
View(data_32)

model_32 <- lm(data_32$date_data_use~ factor(data_32$holiday) +
                 factor(data_32$typhoon) + data_32$date_call_no +
                 data_32$date_call_sec + data_32$date_net_call_no +
                 data_32$date_net_call_sec + data_32$people, data = data_32)
model_32
summary(model_32)

#############for 24~32############
t_1 <- c()
for(i in c(24:32)){
  data_i <- data[data$case == i ,]
#  View(data_i)
  
  model_i <- lm(data_i$date_data_use~ factor(data_i$holiday) +
                   factor(data_i$typhoon) + data_i$date_call_no +
                   data_i$date_call_sec + data_i$date_net_call_no +
                   data_i$date_net_call_sec + data_i$people, data = data_i)
  model_i
  print(summary(model_i))
  t = summary(model_i)[4]
  t = as.data.frame(t)
  t = t[-c(1:3),-c(1:3)]
  t_1 = cbind(t_1,t)
}

View(t_1)
colnames(t_1) <- c(24:32)
rownames(t_1) <- c('date_call_no ', 'date_call_sec', 'date_net_call_no', 
                   'date_net_call_sec', 'people')
View(t_1)


##plot
par(mfrow = c(3, 3))
for(i in c(24:32)){
  data_i <- data[data$case == i ,]
  plot(data_i$date_data_use, type = "l", xlim = c(0,500), col = i)
}

##mean
for(i in c(24:32)){
  data_i <- data[data$case == i ,]
  k = mean(data_i$date_data_use)
  print(k)
}



#######data_1-->非正規劃
data_1 <- read.table(file = "Test.csv", header = TRUE, sep = ",", quote = "\"") 
data_1 <- data_1[!is.na(data_1$id),]

##summary  錯的，每天的check point 不一樣多 ex:9000一天記一次為9000 記三次一次為3000
total <- c()
for(i in c(24:32)){
  data_1_i <- data_1[data_1$case == i ,]
  k = summary(data_1_i$date_data_use)
#  print(k)
  total = rbind(total,k)
}

print(total)
total <- as.data.frame(total)
rownames(total) <- c(24:32)
View(total)
total_t <- t(total)
colnames(total_t) <- c(24:32)
View(total_t)



##sd
total_1 <- c()
for(i in c(24:32)){
  data_1_i <- data_1[data_1$case == i ,]
  k = sd(data_1_i$date_data_use)
#  print(k)
  total_1 = c(total_1,k)
}
print(total_1)
total_1 <- as.data.frame(total_1)
rownames(total_1) <- c(24:32)
View(total_1)


write.table(total_1, file = "/Users/ting/Desktop/sd.csv", sep = ","
          , quote = F)


rm(data_2)
diff_call_o_net <- data_1$date_net_call_sec/data_1$date_call_sec 
data_2 <- cbind(data_1, diff_call_o_net)
View(data_2)
write.table(data_2, file = "/Users/ting/Desktop/data_2.csv", sep = ","
            , quote = F)



total_2 <- c()
for(i in c(24:32)){
  data_2_i <- data_2[data_2$case == i ,]
  k = mean(data_2_i$diff_call_o_net)
  print(k)
  total_2 = c(total_2,k)
}

total_2 <- as.data.frame(total_2)

rownames(total_2) <- c(24:32)
View(total_2)

write.table(total_2, file = "/Users/ting/Desktop/total_2.csv", sep = ","
            , quote = F)












