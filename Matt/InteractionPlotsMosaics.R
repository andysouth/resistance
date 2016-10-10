nScenarios <- 10000

#to run 'extended' or not
experiment <- 'extended' #'curtis'


# Generate data for micro-mosaics where 10% of the population encounter two insecticides.
listOutTen <- sensiAnMosaicInteractions( nScenarios, insecticideUsed = 'mosaic', experiment='extended',propboth = 0.1  )

# Generate data for micro-mosaics where 20% of the population encounter two insecticides.
listOutTwenty  <- sensiAnMosaicInteractions( nScenarios, insecticideUsed = 'mosaic', experiment='extended',propboth = 0.2 )



# Generate data for micro-mosaics where 50% of the population encounter two insecticides.
listOutFifty  <- sensiAnMosaicInteractions( nScenarios, insecticideUsed = 'mosaic', experiment='extended',propboth = 0.5 )


# Generate data for micro-mosaics where 90% of the population encounter two insecticides.
listOutNinety  <- sensiAnMosaicInteractions( nScenarios, insecticideUsed = 'mosaic', experiment='extended',propboth = 0.9 )


#Find resistance points of previously generated sequential data
resistPointsI1 <- findResistancePoints(listOutI1, locus=1)
resistPointsI2 <- findResistancePoints(listOutI2, locus=2)
resistPointsSeq <- resistPointsI1 + resistPointsI2

#Find resistance points for the mosaic strategy with 10% interaction
resistPointsTen_1 <- findResistancePoints(listOutTen, locus='either')


resistPointsTen_A <- findResistancePointsMixResponsive(listOutTen, listOutI1, listOutI2)


resistPointsTen_2 <- findResistancePoints(listOutTen, locus='both')


#Find resistance points for the mosaic strategy with 20% interaction
resistPointsTwenty_1 <- findResistancePoints(listOutTwenty, locus='either')


resistPointsTwenty_A <- findResistancePointsMixResponsive(listOutTwenty, listOutI1, listOutI2)


resistPointsTwenty_2 <- findResistancePoints(listOutTwenty, locus='both')


#Find resistance points for the mosaic strategy with 50% interaction
resistPointsFifty_1 <- findResistancePoints(listOutFifty, locus='either')


resistPointsFifty_A <- findResistancePointsMixResponsive(listOutFifty, listOutI1, listOutI2)


resistPointsFifty_2 <- findResistancePoints(listOutFifty, locus='both')

#Find resistance points for the mosaic strategy with 90% interaction
resistPointsNinety_1 <- findResistancePoints(listOutNinety, locus='either')


resistPointsNinety_A <- findResistancePointsMixResponsive(listOutNinety, listOutI1, listOutI2)


resistPointsNinety_2 <- findResistancePoints(listOutNinety, locus='both')

resistPointsI1=resistPointsI1[,1:1000]
resistPointsI2=resistPointsI2[,1:1000]
resistPointsSeq=resistPointsSeq[,1:1000]

#Bind all resistance points in a data set
ggInsOuts <- rbind(resistPointsI1, resistPointsI2, resistPointsSeq,
                    resistPointsTen_2, resistPointsTwenty_2, resistPointsFifty_2, resistPointsNinety_2)  


#Disregard those that did not meet the threshold
ggInsOuts[ggInsOuts=="999"]<-NA


mosaics=ggInsOuts[c(3,6,9,12,15,18,21),]

didntReachThresh <- which(ggInsOuts$gen_cP0.5 == 999 | ggInsOuts$gen_cP0.5 > 500 |
                            ggInsOuts$gen_cP0.25 == 999 | ggInsOuts$gen_cP0.25 > 500 |
                            ggInsOuts$gen_cP0.1 == 999 | ggInsOuts$gen_cP0.1 > 500 )






par(mfrow=c(1,4))

plot(mosaics[3,],mosaics[4,],xlim=c(0,500),ylim=c(0,500), ylab="Generations Until Resistance (Mosaic)",xlab="Generations Until Resistance (Sequence)", main="10% interaction")
yx=seq(-500,700,by=1)
lines(yx,yx,col="red")

plot(mosaics[3,],mosaics[5,],xlim=c(0,500),ylim=c(0,500), ylab="Generations Until Resistance (Mosaic)",xlab="Generations Until Resistance (Sequence)", main="20% interaction")
lines(yx,yx,col="red")



plot(mosaics[3,],mosaics[6,],xlim=c(0,500),ylim=c(0,500), ylab="Generations Until Resistance (Mosaic)",xlab="Generations Until Resistance (Sequence)", main="50% interaction")
lines(yx,yx,col="red")


plot(mosaics[3,],mosaics[7,],xlim=c(0,500),ylim=c(0,500), ylab="Generations Until Resistance (Mosaic)",xlab="Generations Until Resistance (Sequence)", main="90% interaction")
lines(yx,yx,col="red")


