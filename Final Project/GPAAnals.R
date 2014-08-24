###############################################################################
#
# CoEvalModel.R: Routines to run essential network analysis and visualizations
#                in R.
# Author: Kemal Akkoyun
# Date: Jun 5, 2013
#
#
# Created by Kemal Akkoyun on 6/5/13.
# Copyright (c) 2013 Kemal Akkoyun. All rights reserved.
#
# This file is part of CoEvalModel.
# CoEvalModel is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Simple DHT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with CoEvalModel.  If not, see <http://www.gnu.org/licenses/>.
#
###############################################################################

library(sna)
library(network)

Leisure2010 <- read.csv("Input/Leisure2010.csv", sep = "\t", header = FALSE, stringsAsFactors =F)
Leisure2011 <- read.csv("Input/Leisure2011.csv", sep = "\t", header = FALSE, stringsAsFactors =F)
Leisure2012 <- read.csv("Input/Leisure2012.csv", sep = "\t", header = FALSE, stringsAsFactors =F)

nvertices <- length(Leisure2010)

mLeisure2010 <- as.matrix(Leisure2010[,1:nvertices])
mLeisure2011 <- as.matrix(Leisure2011[,1:nvertices])
mLeisure2012 <- as.matrix(Leisure2012[,1:nvertices])

gLeisure2010 <- as.network.matrix(mLeisure2010, matrix.type="adjacency")
plot(gLeisure2010)
gLeisure2011 <- as.network.matrix(mLeisure2011, matrix.type="adjacency")
plot(gLeisure2011)
gLeisure2012 <- as.network.matrix(mLeisure2012, matrix.type="adjacency")
plot(gLeisure2012)

Hometown <- read.csv("Input/HomeTown.csv", header = FALSE)
GPA <- read.csv("Input/GPA.csv", header = FALSE)
Gender <- read.csv("Input/Gender.csv", header = FALSE)

updateNetwork <-function(Network, Attributes, Name){
	Network %v% Name <- as.vector(Attributes)
	Network
	}
updateNetworkGPA <-function(Network, Attributes){
	GPA <- as.vector(Attributes)
	Network %v% 'GPA' <- as.numeric(GPA)
	Network
	}

gLeisure2010 <- updateNetwork(gLeisure2010, Hometown, 'Hometown')
gLeisure2010 <- updateNetwork(gLeisure2010, Gender, 'Gender')
gLeisure2010 <- updateNetworkGPA(gLeisure2010, GPA$V1)
gLeisure2011 <- updateNetwork(gLeisure2011, Hometown, 'Hometown')
gLeisure2011 <- updateNetwork(gLeisure2011, Gender, 'Gender')
gLeisure2011 <- updateNetworkGPA(gLeisure2011, GPA$V2)
gLeisure2012 <- updateNetwork(gLeisure2012, Hometown, 'Hometown')
gLeisure2012 <- updateNetwork(gLeisure2012, Gender, 'Gender')
gLeisure2012 <- updateNetworkGPA(gLeisure2012, GPA$V3)

summary(gLeisure2010)
summary(gLeisure2011)
summary(gLeisure2012)


AttributesLeisure2010 <- cbind(Hometown, Gender, GPA$V1)
colnames(AttributesLeisure2010) <- c('Hometown','Gender','GPA')
AttributesLeisure2011 <- cbind(Hometown, Gender, GPA$V2)
colnames(AttributesLeisure2011) <- c('Hometown','Gender','GPA')
AttributesLeisure2012 <- cbind(Hometown, Gender, GPA$V3)
colnames(AttributesLeisure2012) <- c('Hometown','Gender','GPA')

getNodeCentralities <- function(Attributes, Network){
	Attributes$Total.Degree <- degree(Network)
	Attributes$In.Degree <- degree(Network, cmode = 'indegree')
	Attributes$Out.Degree <- degree(Network, cmode = 'outdegree')
	Attributes$Betweenness <- betweenness(Network)
	Attributes$Closeness <- closeness(Network)
	Attributes
	}

AttributesLeisure2010 <- getNodeCentralities(AttributesLeisure2010, gLeisure2010)
AttributesLeisure2011 <- getNodeCentralities(AttributesLeisure2011, gLeisure2011)
AttributesLeisure2012 <- getNodeCentralities(AttributesLeisure2012, gLeisure2012)

#Network Visualisation
visualizeNetwork <- function(Network, Attributes, nvertices, fname){
	vsize <- Attributes$In.Degree / 1.5
	#You might need to play with the number for a better visualization.
	for (i in 1:nvertices){
		Attributes$Gender.Color[i]  <- 'grey'
		if (Attributes$Gender[i] == 0 ) Attributes$Gender.Color[i]  <- 'lightblue2'
		if (Attributes$Gender[i] == 1 ) Attributes$Gender.Color[i]  <- 'pink'
		}
	Network %v% 'Gender.Color' <- as.vector(Attributes$Gender.Color)
	png(fname, width = 1024, height = 1024, pointsize=24)
	gplot(Network, gmode = 'digraph', label=Network%v%'GPA', label.cex = 0.8, boxed.labels = T, vertex.cex = vsize, label.bg = Network%v%'Gender.Color')
	dev.off()
	Network
	}

gLeisure2010 <- visualizeNetwork(gLeisure2010, AttributesLeisure2010, nvertices, "Output/Leisure2010_InDegree.png")
gLeisure2011 <- visualizeNetwork(gLeisure2011, AttributesLeisure2011, nvertices, "Output/Leisure2011_InDegree.png")
gLeisure2012 <- visualizeNetwork(gLeisure2012, AttributesLeisure2012, nvertices, "Output/Leisure2012_InDegree.png")

computeNetworkLevelMeasures <- function(Network, Attributes){
	Network %n% 'Density' <- gden(Network)
	Network %n% 'Centralization.Degree' <- centralization(Network, degree)
	Network %n% 'Centralization.Closeness' <- centralization(Network, closeness)
	Network %n% 'Centralization.Betweenness' <- centralization(Network, betweenness)
	Network %n% 'Transitivity (CC)' <- gtrans(Network)
	Network %n% 'Reciprocity.Dyadic' <- grecip(Network)
	Network %n% 'Reciprocity.Edgewise' <- grecip(Network, measure ='edgewise')
	Network %n% 'Components.Strong' <- components(Network)
	Network %n% 'Components.Weak' <- components(Network, connected ='weak')
	Network
	}

gLeisure2010 <- computeNetworkLevelMeasures(gLeisure2010, AttributesLeisure2010)
gLeisure2011 <- computeNetworkLevelMeasures(gLeisure2011, AttributesLeisure2011)
gLeisure2012 <- computeNetworkLevelMeasures(gLeisure2012, AttributesLeisure2012)

AttributesLeisure2010$Component.Membership <- component.dist(gLeisure2010)$membership
AttributesLeisure2011$Component.Membership <- component.dist(gLeisure2011)$membership
AttributesLeisure2012$Component.Membership <- component.dist(gLeisure2012)$membership

visualizeNetworkComponents <- function(Network, Attributes, nvertices, fname, sizes, ratio = 15){
	vsize <- sizes / ratio
	for (i in 1:nvertices){
		Attributes$Gender.Color[i]  <- 'grey'
		if (Attributes$Gender[i] == '0' ) Attributes$Gender.Color[i]  <- 'lightblue2'
		if (Attributes$Gender[i] == '1' ) Attributes$Gender.Color[i]  <- 'pink'
		}
	Network %v% 'Gender.Color' <- as.vector(Attributes$Gender.Color)
	colors <- colors()
	for (i in 1:nvertices){
		c <- Attributes$Component.Membership[i] + 10
		#a simple yet not so robust color assignment, correct it later.
		Attributes$Component.Color[i]  <- colors[c]
		}
	png(fname, width = 1024, height = 1024, pointsize=24)
	gplot(Network, gmode = 'digraph', label=Network%v%'GPA',
		label.cex = 0.8, boxed.labels = T, vertex.cex = vsize,
		label.bg = Network%v%'Gender.Color',
		vertex.col = Attributes$Component.Color)
	dev.off()
	Network
	}

gLeisure2010 <- visualizeNetworkComponents(gLeisure2010, AttributesLeisure2010, nvertices, "Output/Leisure2010_InDeg_Components.png", AttributesLeisure2010$In.Degree, 1)
gLeisure2011 <- visualizeNetworkComponents(gLeisure2011, AttributesLeisure2011, nvertices, "Output/Leisure2011_InDeg_Components.png", AttributesLeisure2011$In.Degree, 1)
gLeisure2012 <- visualizeNetworkComponents(gLeisure2012, AttributesLeisure2012, nvertices, "Output/Leisure2012_InDeg_Components.png", AttributesLeisure2012$In.Degree, 1)


######### Results are to be tabulated ############

##### The Linear Model (Linear Regression) Testing: ######
extractValues <- function(GroupVector, type, ValueVector, initvalue){
	values <-c(initvalue)
	n <- length(GroupVector)
	for (i in 1:n){
		if (GroupVector[i] == type){values <- c(values, ValueVector[i])}
		}
	values[-1]
	}

males <- extractValues(AttributesLeisure2011$Gender, 0, AttributesLeisure2011$GPA, 0)
females <- extractValues(AttributesLeisure2011$Gender, 1, AttributesLeisure2011$GPA, 0)
wilcox.test(males, females)
# Result:
# data:  males and females
# W = 38.5, p-value = 0.4908

#1. GPA i yüksek olanlar olmayanlara göre daha sosyal (2010)
cor(AttributesLeisure2010$GPA, AttributesLeisure2010$Out.Degree)
# 0.6522856
model <- lm(AttributesLeisure2010$GPA ~ AttributesLeisure2010$Out.Degree)
summary(model)
# Call:
# lm(formula = AttributesLeisure2010$GPA ~ AttributesLeisure2010$Out.Degree)
# Residuals:
#      Min       1Q   Median       3Q      Max
# -1.17004 -0.46504  0.05632  0.42725  1.37267
# Coefficients:
#                                  Estimate Std. Error t value Pr(>|t|)
# (Intercept)                        1.5727     0.2734   5.752    5e-05 ***
# AttributesLeisure2010$Out.Degree   0.3373     0.1048   3.220  0.00617 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 0.7328 on 14 degrees of freedom
# Multiple R-squared:  0.4255,	Adjusted R-squared:  0.3844
# F-statistic: 10.37 on 1 and 14 DF,  p-value: 0.006171

#2. GPA i yüksek olanların sosyallik durumu geçtiğimiz 2010 senesine göre daha fazla (2011)
cor(AttributesLeisure2011$GPA, AttributesLeisure2011$Out.Degree)
# 0.6504846
model <- lm(AttributesLeisure2011$GPA ~ AttributesLeisure2011$Out.Degree)
summary(model)
# Call:
# lm(formula = AttributesLeisure2011$GPA ~ AttributesLeisure2011$Out.Degree)
# Residuals:
#     Min      1Q  Median      3Q     Max
# -1.1196 -0.3935  0.0628  0.4317  1.2052
# Coefficients:
#                                  Estimate Std. Error t value Pr(>|t|)
# (Intercept)                        1.3848     0.3065   4.519 0.000482 ***
# AttributesLeisure2011$Out.Degree   0.4949     0.1544   3.205 0.006363 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 0.6449 on 14 degrees of freedom
# Multiple R-squared:  0.4231,	Adjusted R-squared:  0.3819
# F-statistic: 10.27 on 1 and 14 DF,  p-value: 0.006363

#2. GPA in cinsiyet ile ilişkisi yok. (2011)
cor(AttributesLeisure2012$GPA, AttributesLeisure2012$Gender)
# -0.5586827
model <- lm(AttributesLeisure2012$GPA ~ AttributesLeisure2012$Gender)
summary(model)
# Call:
# lm(formula = AttributesLeisure2012$GPA ~ AttributesLeisure2012$Gender)
# Residuals:
#      Min       1Q   Median       3Q      Max
# -0.80667 -0.37917 -0.01286  0.26619  1.17333
# Coefficients:
#                              Estimate Std. Error t value Pr(>|t|)
# (Intercept)                    2.5067     0.1821   13.77 1.57e-09 ***
# AttributesLeisure2012$Gender  -0.6938     0.2753   -2.52   0.0245 *
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# Residual standard error: 0.5462 on 14 degrees of freedom
# Multiple R-squared:  0.3121,	Adjusted R-squared:  0.263
# F-statistic: 6.353 on 1 and 14 DF,  p-value: 0.02448

write.csv(AttributesLeisure2010, 'Output/AttributesLeisure2010.csv', row.names = FALSE)
write.csv(AttributesLeisure2011, 'Output/AttributesLeisure2011.csv', row.names = FALSE)
write.csv(AttributesLeisure2012, 'Output/AttributesLeisure2012.csv', row.names = FALSE)
