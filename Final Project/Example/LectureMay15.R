
library(RSiena)

friend.data.w1 <- as.matrix(read.table("s50-network1.dat"))
friend.data.w2 <- as.matrix(read.table("s50-network2.dat"))
friend.data.w3 <- as.matrix(read.table("s50-network3.dat"))
drink <- as.matrix(read.table("s50-alcohol.dat"))
smoke <- as.matrix(read.table("s50-smoke.dat"))
drinkingbeh <- sienaNet( drink, type = "behavior" )
smoke1 <- coCovar( smoke[ , 1 ] )

class(friendship)
dim(friendship)
attributes( friendship )
attributes( friendship )$type
smoke1 <- coCovar( smoke[ , 1 ] )
alcohol <- varCovar( drink )
attributes( alcohol )
mydata <- sienaDataCreate( friendship, smoke1, alcohol )

mybehdata <- sienaDataCreate( friendship, smoke1, drinkingbeh )
mybehdata

myeff <- getEffects( mydata )
myeff
names(myeff)
RShowDoc("effects", package="RSiena")

myalgorithm <- sienaModelCreate(useStdInits = FALSE, projname = 's50_3')

myeff <- getEffects( mydata )
myeff <- includeEffects( myeff, transTrip, cycle3 )
myeff
myeff <- includeEffects( myeff, cycle3, include=FALSE )
myeff
myeff <- includeEffects( myeff, egoX )
myeff

myeff <- includeEffects( myeff, egoX, altX, egoXaltX, interaction1 = "alcohol" )
myeff <- includeEffects( myeff, simX, interaction1 = "smoke1" )

myeff <- setEffect(myeff, denseTriads, parameter = 6)
myeff

ans <- siena07(myalgorithm, data=mydata, effects=myeff, batch=TRUE)

ans$theta

ans$covtheta

# Coevolution:
myCoEvolutionData <- sienaDataCreate( friendship, smoke1, drinkingbeh )
myCoEvolutionEff <- getEffects( myCoEvolutionData )
print01Report( myCoEvolutionData, myCoEvolutionEff, modelname = 's50_3_CoEvinit' )

# Effect of local structures on evolution:
myCoEvolutionEff <- includeEffects( myCoEvolutionEff, transTrip, cycle3)

#Effect homophily smoking behaviour on network evolution:
myCoEvolutionEff <- includeEffects( myCoEvolutionEff, simX,
											interaction1 = "smoke1" )
											
#sender(ego), reciver (alt), homophily(sim) effect/influence of drinking behaviour on network
myCoEvolutionEff <- includeEffects(myCoEvolutionEff, egoX, altX, simX,
										   interaction1 = "drinkingbeh" )

#influence of network on drinking behaviour:
myCoEvolutionEff <- includeEffects( myCoEvolutionEff,
								name = "drinkingbeh",
								avAlt,indeg, outdeg,
								interaction1 = "friendship" )

myCoEvolutionEff

myCoEvAlgorithm <- sienaModelCreate( projname = 's50CoEv_3' )

ans <- siena07( myCoEvAlgorithm, data = myCoEvolutionData, effects = myCoEvolutionEff, batch = TRUE )

summary(ans)
# For this small data set, the model for behavior dynamics is over-specified,
# leading to some very large standard errors.
# Running a model modified by

myCoEvolutionEff <- getEffects( myCoEvolutionData )

		myCoEvolutionEff <- includeEffects( myCoEvolutionEff,
								name = "drinkingbeh", indeg, outdeg,
								interaction1 = "friendship", include = FALSE)

#		(ans <- siena07( myCoEvAlgorithm, data = myCoEvolutionData,
#						effects = myCoEvolutionEff ))

# without degree effects on behaviour gives better results.

#### Refere to this part to start with your own model design ########
friend.data.w1 <- as.matrix(read.csv("Leisure2010.csv", sep = "\t", header = FALSE))
friend.data.w2 <- as.matrix(read.csv("Leisure2011.csv", sep = "\t", header = FALSE))
friend.data.w3 <- as.matrix(read.csv("Leisure2012.csv", sep = ";", header = FALSE))
gpa <- as.matrix(read.csv("GPA.csv", header = FALSE))
home.town <- as.matrix(read.csv("HomeTown.csv", header = FALSE))
gender <- as.matrix(read.csv("Gender.csv", header = FALSE))

friendship <- sienaNet(
                     array( c( friend.data.w1, friend.data.w2, friend.data.w3 ),
                     dim = c( 16, 16, 3 ) ) )
gpa.performance <- sienaNet(gpa, type = "behavior")
gender1 <- coCovar(gender[,1])
hometown1 <- coCovar(home.town[,1])
gpa.change <- varCovar(gpa)

myCoEvolutionData <- sienaDataCreate(friendship, gender1, gpa.performance)
myCoEvolutionEff <- getEffects( myCoEvolutionData )

myCoEvolutionEff <- includeEffects(myCoEvolutionEff,
								name = "gpa.performance", indeg, outdeg,
								interaction1 = "friendship", include = FALSE)
								
myCoEvAlgorithm <- sienaModelCreate( projname = 'Bilgi_CS' )

ans <- siena07( myCoEvAlgorithm, data = myCoEvolutionData, effects = myCoEvolutionEff, batch = TRUE )
