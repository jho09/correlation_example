library(tidyverse)


my.data <- read_csv("https://jeroenooms.github.io/multilevel/data/chapter2/popular2.csv")

write_csv(my.data,path="hox_data_chapter2.csv")

analytic.data <- select(my.data,class,extrav,popular,texp)
analytic.data

analytic.data$extrav
analytic.data$popular

cor.test(x=analytic.data$extrav,y=analytic.data$popular)

my.cor.result <- cor.test(x=analytic.data$extrav,y=analytic.data$popular)
my.cor.result
str(my.cor.result)


my.cor.result$estimate

my.scatter <- qplot(x=extrav,y=popular,data=analytic.data)
print(my.scatter)

my.scatter <- qplot(x=extrav,y=popular,data=analytic.data)
my.scatter <- my.scatter + geom_smooth(method="lm",se=TRUE,color="black")
print(my.scatter)

?geom_smooth

library(apaTables)
apa.cor.table(as.data.frame(analytic.data))


psych::pairs.panels(as.data.frame(analytic.data),lm=TRUE)

analytic.data.class.is.5 <- filter(analytic.data,class==5)

View(analytic.data.class.is.5)

cor.test(x=analytic.data.class.is.5$extrav,y=analytic.data.class.is.5$popular)

analytic.data.grouped.by.class <- group_by(analytic.data,class)

cor.by.group <- summarise(analytic.data.grouped.by.class,cor.test(x=extrav,y=popular)$estimate)
print(cor.by.group)

print(as.data.frame(cor.by.group))

analytic.data.groups.15.to.30 <- filter(analytic.data.grouped.by.class,class>=15 & class<=30)

my.scatter <- qplot(x=extrav,y=popular,data=analytic.data.groups.15.to.30)
my.scatter <- my.scatter + facet_wrap(~class)
print(my.scatter)


my.scatter <- qplot(x=extrav,y=popular,data=analytic.data.groups.15.to.30)
my.scatter <- my.scatter + geom_smooth(method="lm",se=FALSE,color="black")
my.scatter <- my.scatter + facet_wrap(~class)
print(my.scatter)


my.scatter <- qplot(x=extrav,y=popular,data=analytic.data.groups.15.to.30)
my.scatter <- my.scatter + geom_smooth(method="lm",se=TRUE,color="black")
my.scatter <- my.scatter + facet_wrap(~class)
print(my.scatter)


ggsave("groupScatterPlotWithCI.pdf",plot=my.scatter,width=6,height=6)
