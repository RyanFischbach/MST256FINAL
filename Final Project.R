data(Video_Games_Sales_as_at_22_Dec_2016)
glm(ride.alc.driver ~ smoke, family = "binomial", data=YouthRisk2007)
data = Video_Games_Sales_as_at_22_Dec_2016
pairs(data)

##omit
newData = na.omit(data)

##EDA
pairsData = newData[, c(2:5, 10:14, 16)]
pairs(pairsData)
newData[, c(14)]
my_data <- newData[, c(10:14)]
res = cor(my_data)
round(res, 2)
sapply(my_data, is.factor)
cor(my_data[sapply(my_data, function(x) !is.factor(x))])
newData2 = newData[-1,]
hist(newData2$NA_Sales, xlab = "Total Sales (millions)", main = "Frequency of Total Sales", breaks = 20)
mean(newData2$Global_Sales)

##Methods
lmfull = lm(Global_Sales ~ Platform + Year_of_Release + Genre + Publisher + Critic_Score + Critic_Count + User_Score + User_Count + Developer + Rating, data=newData)
lmred = lm(I(log(Global_Sales)) ~ Platform + Year_of_Release + Genre + Critic_Score + Critic_Count + User_Count + Rating, data = newData2)
step(lmfull, direction = "both")
summary(lmred)
plot(lmred)
which(Video_Games_Sales_as_at_22_Dec_2016$Platform == "DC")
Video_Games_Sales_as_at_22_Dec_2016[1103,]

newData$return <- ifelse(newData$Global_Sales > 2.4, 1, 0)
which(newData$return == 1)

glm11 = glm(return ~ Platform + Year_of_Release + Genre + Critic_Score + Critic_Count + User_Score + User_Count + Rating, data = newData, family = "binomial")
summary(glm11)
