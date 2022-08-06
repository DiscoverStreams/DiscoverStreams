

#install.packages("devtools")
devtools::install_github("tylerbhampton/budykoR")


library(ggplot2)
library(budyko)

blankBC
blankBC+coord_cartesian(xlim=c(0,3))

testdata=budyko::testdata
head(testdata)
blankBC+geom_point(data=testdata)+coord_cartesian(xlim=c(0,5))

#default budyko-type curve using default settings of budyko_sim()
ogbudyko=budyko_sim()
blankBC+geom_line(data=ogbudyko)+coord_cartesian(xlim=c(0,5))
blankBC+
  geom_line(data=ogbudyko)+
  geom_point(data=testdata)+
  coord_cartesian(xlim=c(0,5))


#custom fit the data
fit1=budyko_fit(data=testdata,method="Fu",dif="mae",silent = TRUE)
fit1
sim1=budyko_sim(fit=fit1)
blankBC+
  geom_line(data=ogbudyko,col=2)+
  geom_line(data=sim1)+
  geom_point(data=testdata)+
  coord_cartesian(xlim=c(0,5))
