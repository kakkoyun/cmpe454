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

# install.packages("RSiena")

library(RSiena)

# Data Creation
###############################################################################

# Obtain relational leisure data from given data files.
friend.data.w1 <- as.matrix(read.csv("Input/Leisure2010.csv", sep = "\t", header = FALSE))
friend.data.w2 <- as.matrix(read.csv("Input/Leisure2011.csv", sep = "\t", header = FALSE))
friend.data.w3 <- as.matrix(read.csv("Input/Leisure2012.csv", sep = "\t", header = FALSE))

# Obtain attribute data from given data files.
gpa <- as.matrix(read.csv("Input/GPA.csv", header = FALSE))
home.town <- as.matrix(read.csv("Input/HomeTown.csv", header = FALSE))
gender <- as.matrix(read.csv("Input/Gender.csv", header = FALSE))

# Network Initializzaton
###############################################################################

# Initialize friendship network.
friendship <- sienaNet(
                     array(c(friend.data.w1, friend.data.w2, friend.data.w3),
                     dim = c(16, 16, 3)))

# Initialize gpa behaviour as a network.
gpa.performance <- sienaNet(gpa, type = "behavior")ti

# From the help request
#       ?sienaDataCreate
# We see that these can be of five kinds:
# coCovar : Constant actor covariates
# varCovar : Time-varying actor covariates
# coDyadCovar : Constant dyadic covariates
# varDyadCovar : Time-varying dyadic covariates
# compositionChange : Composition change indicators

# So Constant actor variates.
gender1 <- coCovar(gender[,1])
hometown1 <- coCovar(home.town[,1])
# and Time-varying actor covariates
gpa.change <- varCovar(gpa)

# Creating Model : Example | Gender/GPA/Friendship
###############################################################################

exampleCoEvolutionData <- sienaDataCreate(friendship, gender1, gpa.performance)
exampleCoEvolutionEff <- getEffects( exampleCoEvolutionData )
exampleCoEvolutionEff <- includeEffects(exampleCoEvolutionEff,
                name = "gpa.performance", indeg, outdeg,
                interaction1 = "friendship", include = FALSE)
exampleCoEvAlgorithm <- sienaModelCreate(projname = 'Output/GenderGPAFriendship')
ans0 <- siena07(exampleCoEvAlgorithm,
               data = exampleCoEvolutionData,
               effects = exampleCoEvolutionEff,
               batch = TRUE)

# Creating Model : Hometown/Friendship
###############################################################################

hometownCoEvolutionData <- sienaDataCreate(friendship, hometown1)
hometownCoEvolutionEff <- getEffects(hometownCoEvolutionData)
hometownEvolutionEff <- includeEffects(hometownCoEvolutionEff,
                name = "hometown.friendship", indeg, outdeg,
                interaction1 = "friendship", include = FALSE)
hometownCoEvAlgorithm <- sienaModelCreate(projname = 'Output/HometownFriendship')
ans1 <- siena07(hometownCoEvAlgorithm,
               data = hometownCoEvolutionData,
               effects = hometownCoEvolutionEff,
               batch = TRUE)

# Creating Model : GPA/Friendship
###############################################################################

gpaCoEvolutionData <- sienaDataCreate(friendship, gpa.performance)
gpaCoEvolutionEff <- getEffects(gpaCoEvolutionData)
gpaCoEvolutionEff <- includeEffects(gpaCoEvolutionEff,
                name = "gpa.friendship", indeg, outdeg,
                interaction1 = "friendship", include = FALSE)
gpaCoEvAlgorithm <- sienaModelCreate(projname = 'Output/GPAFriendship')
ans2 <- siena07(gpaCoEvAlgorithm,
               data = gpaCoEvolutionData,
               effects = gpaCoEvolutionEff,
               batch = TRUE)


# Creating Model : Friendship/Hometown/GPA
###############################################################################

hometown2CoEvolutionData <- sienaDataCreate(friendship, hometown1, gpa.performance)
hometown2CoEvolutionEff <- getEffects(hometown2CoEvolutionData)
hometown2CoEvolutionEff <- includeEffects(hometown2CoEvolutionEff,
                name = "hometown.gpa", indeg, outdeg,
                interaction1 = "friendship", include = FALSE)
hometown2CoEvAlgorithm <- sienaModelCreate(projname = 'Output/FriendshipHometownGPA')
ans3 <- siena07(hometown2CoEvAlgorithm,
               data = hometown2CoEvolutionData,
               effects = hometown2CoEvolutionEff,
               batch = TRUE)


summary(ans0)
summary(ans1)
summary(ans2)
summary(ans3)
