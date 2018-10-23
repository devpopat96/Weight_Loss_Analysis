#################################################################################################################
# CREATED BY: DEVANSH POPAT
#######################
# Load Data File: 
#######################
weightLoss.data <- read.csv(file.choose(), header= TRUE)


#######################
# Data Wrangling 
#######################
# Renaming the columns to what the actually stand for
names(weightLoss.data)[1] <- "id"
names(weightLoss.data)[3:5] <- c("WeightLoss_month1","WeightLoss_month2", "WeightLoss_month3")
names(weightLoss.data)[6:8] <- c("SelfEsteem_month1", "SelfEsteem_month2", "SelfEsteem_month3")

# Reshaping three weight loss months to a single variable and doing the same for Self esteem
library(reshape2)
wl.data <- melt(weightLoss.data[, 1:5], id.vars = c("id", "group"))
names(wl.data)[3:4] <- c("WeightLoss_Month", "WeightLoss")

we.data <- melt(weightLoss.data[, c(1,2,6,7,8)], id.vars = c("id", "group"))
names(we.data)[3:4] <- c("SelfEsteem_Month", "SelfEsteem_Score")
data.long <- cbind(wl.data, we.data)[, -5:-6]

#######################
# Getting the weight loss frequencies by groups 
#######################
table(weightLoss.data$group, weightLoss.data$WeightLoss_month1)


#######################
# Visualizing the weight loss and self esteem by groups for each of three months 
#######################
library(ggplot2)
ggplot(data.long, aes(x=as.factor(WeightLoss), fill=group)) + labs(x = "Weight in pounds", y = "Count", title = "Weight Loss by Group within 3 months") + geom_bar() + facet_grid (WeightLoss_Month ~ group) + geom_line(aes(y =SelfEsteem_Score, fill=group)) + geom_point(aes(y = SelfEsteem_Score, colour= "blue")) + theme(legend.position='bottom', panel.grid.major.x =element_blank(), panel.grid.minor.x = element_blank(), panel.grid.minor.y =element_blank(), legend.key.size = unit(.5, "cm"), axis.ticks.y =element_blank(),plot.margin = unit( c(1,0,0,0) , units = "lines" ), plot.title = element_text(size =30, lineheight = .8, vjust = 1, family = "Bauhaus 93")) +scale_fill_discrete(guide_legend(title ="Group")) 



#######################
# Visualizing the weight loss and self esteem by groups for first month
#######################

#Scatterplot
ggplot(weight_loss,aes(x=WeightLoss_month1,y=SelfEsteem_month1,color=group))+geom_point()+labs(x ="Weight Loss",y ="Self-Esteem Score",title="Weight Loss vs. Self-Esteem - Month 1")+facet_wrap(~ group)
#Boxplot
ggplot(weight_loss)+geom_boxplot(aes(x=group,y=WeightLoss_month1))+geom_boxplot(aes(x=group,y=SelfEsteem_month1),fill="chartreuse4")+labs(x="Group",y ="Weight Loss   Self-Esteem Score",title="Weight Loss vs. Self-Esteem - Month 1")+coord_flip()

#######################
# Visualizing the weight loss and self esteem by groups for second month
#######################

#Scatterplot
ggplot(weight_loss,aes(x=WeightLoss_month2,y=SelfEsteem_month2,color=group))+geom_point()+labs(x ="Weight Loss",y ="Self-Esteem Score",title="Weight Loss vs. Self-Esteem - Month 2")+facet_wrap(~ group)
#Boxplot
ggplot(weight_loss)+geom_boxplot(aes(x=group,y=WeightLoss_month2))+geom_boxplot(aes(x=group,y=SelfEsteem_month2),fill="chartreuse4")+labs(x="Group",y ="Weight Loss   Self-Esteem Score",title="Weight Loss vs. Self-Esteem - Month 2")+coord_flip()

#######################
# Visualizing the weight loss and self esteem by groups for third month
#######################

#Scatterplot
ggplot(weight_loss,aes(x=WeightLoss_month3,y=SelfEsteem_month3,color=group))+geom_point()+labs(x ="Weight Loss",y ="Self-Esteem Score",title="Weight Loss vs. Self-Esteem - Month 3")+facet_wrap(~ group)
#Boxplot
ggplot(weight_loss)+geom_boxplot(aes(x=group,y=WeightLoss_month3))+geom_boxplot(aes(x=group,y=SelfEsteem_month3),fill="chartreuse4")+labs(x="Group",y ="Weight Loss   Self-Esteem Score",title="Weight Loss vs. Self-Esteem - Month 3")+coord_flip()

#######################
#  Adding a new variable to the data with subjects weight in kilograms
#######################
data.long$WeightLossInKg<-round((data.long$WeightLoss/2.204),2)

#####################################################################################################









