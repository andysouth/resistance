# malaria_code_andy.r 
# andy south 10/3/2015
# a few edits from :

# Malaria insecticide resistance model ###
# Bethany Levick, LSTM 2013 ##



#### INPUT PARAMETERS ####
#Enter file name containing parameters before saving code document 
#If not using input file, set params.csv to FALSE, and enter a calibration number
#Save file once these are set, and then run code
#andy commented out next 4 lines & replaced with below to run inbuilt scenario
# params.csv <- TRUE
#input <- read.csv( "full-dom_SD.csv", header=T, stringsAsFactors=F )	
# produce.plots <- FALSE	## If true, will automatically produce the plots listed below and save to same directory as code is stored
# calibration <- 100	## If inputting through a .csv, this will be overwritten by calibration row of input file

#andy trying to run an inbuilt scenario
tempDisabled <- TRUE
#if( !tempDisabled ) input <- read.csv( "input.parameters.col1.csv", header=T, stringsAsFactors=F ) 


## Beth to run in calibration mode to check behaviour
params.csv <- TRUE
produce.plots <- TRUE	## If true, will automatically produce the plots listed below and save to same directory as code is stored
## either go into this file and manually edit the calibration number
## this would allow for multiple runs with different calibrations
if( !tempDisabled ) input <- read.csv( "input.parameters.csv", header=T, stringsAsFactors=F )
## or hard code it here
calibration <- 102	

## Beth to run in built hard coded scenario to recreate curtis' plots
params.csv <- FALSE
produce.plots <- TRUE	## If true, will automatically produce the plots listed below and save to same directory as code is stored
## could leave this line here, it should be overwritten later on. 
if( !tempDisabled ) input <- read.csv( "input.parameters.csv", header=T, stringsAsFactors=F )
## And then select scenario here
calibration <- 1012

## looking at it now I realise this is not ideal
## a solution may be to provide a .csv file with the Curtis scenarios set up
## and then the need to set the calibration number and params.csv in the R script can be removed altogether
## so it is all set up in the .csv file - I think similar can be done with produce.plots
## so the only bit of R code the user will need to change is the setwd() and the read.csv()

#### RUN CODE ####
# source("malaria_code.r")

### OUTPUT - DATA MATRICES ####		
# Both methods will run the model, generating 3 matrices of results; results, genotype, fitness
# Results details freq of R allele at each loci in each sex and linkage disequilbrium of R allele in each sex, per generation
# Genotype details frequencies of each of the ten genotypes, per generation
# Fitness details the fitness scores of each genotype/niche combination (table 4. of Main Document)

# Where a file is inputted containing multiple scenarios, each of the three matrices is stored in a corresponding list
# ie results.list, genotype.list, fitness.list, where the number of the scenario gives the position of the matrix in the list

# If save.fitvals is set to 1 (i.e., true) then a .csv file will be generated
# containing the fitness values of each double genotype in each possible niche
# as in table three of brief document

### OUTPUT - GRAPHICS ###
# For each scenario run, four graphs will also be saved to the same drive as this document is located
# One details changing frequency of R allele over time by sex (results table "results")
# One details changing frequency of haplotypes over time by locus (results table "genotype")
# One details changing linkage disequilibrium over time (results table "results")
# One details changing frequency of R allele over time, but using the log percentage scale as in Curtis (1985) Fig 2
# Where calibration 1013 (see below) is set, the latter is replaced with a plot of similar scale but more restricted axis limits

### CALIBRATIONS ####
# Calibrations can be set to run specific stored scenarios, to generate certain plots or allow bug checking
## To recreate plots in Curtis (1985)
## Calibration 1011 runs the full dominance insecticide scenario given in Curtis (1985) Fig 1.
## Calibration 1012 runs the single insecticide and combination scenarios given in Curtis (1985) Fig 2. 


## For bug identifying/fixing
## Calibration 102 runs with no random mating
## Calibration 103 runs with no selection
## Calibration 104 runs with selection on one genotype
## set these below
# select.gen.m <- fs.m.SS1RS2  ## two locus genotype to select on MUST be fs not f, unhash to run
# select.gen.f <- fs.f.SS1RS2 # set for male and female form


#### FUNCTIONS TO RUN THE MODEL ###

	






	

## Resistance Allele frequencies over time
## Uses results matrices from list
# Plot of R at each locus (R1=R2, as in Curtis fig 1) 
# Points plotted in purple indicate generations where selection is relaxed
# takes results matrix without relaxation (nrelaxmat) and with relaxation (relaxmat) of selection
# and column numbers in these matrices for the generation (1) and frequency of allele at R1 (2)
curtis_f1 <- function ( nrelaxmat, relaxmat, gencol, r1col ){
  f <- c(0.1,0.5,1,5)				## for y axis 
  fl <- log10(f)
  labs <- c("0.1%", "0.5%", "1%", "5%")	
  
  # set frequencies in vectors as log10 percentages from frequencies
  nrelax <- log10( 100 * nrelaxmat[,r1col] )			# only prints results from males, m = f, A[R] = B[R], so only prints one col
  gens <- nrelaxmat[,gencol]	
  
  			
  par(pty="s") 			
  plot( 0, 0, type="n", axes=F,						## Blank square 1,1 plot
  	xlim=c(1,(max(nrelaxmat[,gencol]))), ylim=c((min(fl)),(max(fl))),
  	xlab="Generation", ylab="Allele Frequency - (A[R]= B[R])", main="Frequency of R allele through time.")
  
  axis( side=1, at=c(0,2,4,6,8,10,12,14,16,18), labels=c(0,2,4,6,8,10,12,14,16,18), tick=T )
  axis( side=2, at=fl, labels=labs, tick=T )
  
  lines( gens, nrelax, col="black" )
  
  ## relaxed selection
  relax <- relaxmat[,r1col]
  relax <- log10( 100 * relax )
  
  lines( gens, relax )
  points( c(3:12), relax[3:12], col="darkviolet", pch=16 )
  points( c(16:18), relax[16:18], col="darkviolet", pch=16 )
  
  abline( h=(log10(5) ) )
  
  
  pos <- log10( 4.5 )
  legend( 7, pos , legend=c("R at Locus 1( = R at Locus 2 )", "With relaxed selection"), 
  			col=c("black", "darkviolet"), pch=c(16,16), bty="n" )
  
  box()
	
}

## Linkage disequilibrium over time
## uses results matrices from list
# Plot of index of linkage disequilibrium, D 
# Points plotted in purple indicate generations where selection is relaxed
# takes results matrix without relaxation (nrelaxmat) and with relaxation (relaxmat) of selection
# and column numbers in these matrices for the generation (1) and linkage disequilibrium (4)
curtis_ld <- function(resultsmat, relaxedmat, gencol, ldcol){

  par(pty="s") ##, mfrow=c(2,1))		# two rows, one column 
  
  ## LD plot at top
  ymin <- log10(0.000001)
  ymax <- log10(0.01)
  
  res <- log10( resultsmat[,ldcol])
  rel <- log10( relaxedmat[,ldcol])
  
  gens <- resultsmat[,gencol]
  
  plot( 0, 0, type="n", axes=F,						## Blank square 1,1 plot
  	xlim=c(1,(max(resultsmat[,gencol]))), ylim=c(ymin,ymax),
  	xlab="Generation", ylab="Index of Linkage Disequilibrium - D", main="Linkage Disequilibrium")
  
  lines( gens, res )		#plot log10 LD against generation
  lines( gens, rel )
  
  points( c(3:12), rel[3:12], col="darkviolet", pch=16 )		# add purple points on relaxed line to indicate relaxed generations
  points( c(15:18), rel[15:18], col="darkviolet", pch=16 )
  
  axis( side=1, at=c(0,2,4,6,8,10,12,14,16,18), labels=c(0,2,4,6,8,10,12,14,16,18), tick=T )
  
  labs <- c(0.000001,0.00001,0.0001,0.001,0.01)
  labpos <- log10(labs)
  
  axis( side=2, at=labpos, labels=labs, tick=T )
  
  pos <- log10( 0.01 )
  legend( 9, pos , legend=c("Linkage disequilbrium","With relaxed selection"), 
  			col=c("black", "darkviolet"), pch=c(16,16), bty="n" )
  
  box()

}

### Lists to store results ####
results.list <- list()			# list storing results (R allele freq) matrices for each run
fitness.list <- list()			# list storing fitness values for each niche/genotype combination for each run
genotype.list <- list()			# list storing genotype frequency matrices for each run

#### INPUT PARAMETERS ####
## Scenario 1 Calibration 1011 - Fig 1 Curtis ####
if( params.csv == FALSE && calibration == 1011 ){  
input <- matrix( ncol=2, nrow=52 )
input[1,1] <- calibration

### Results Matrix ###
# Give number of generations
input[2,1] <- 18

### Saving Parameters ###
# To save parameters in a matrix, set coll.fitvals to 1 ##
input[3,1] <- 1
# To save this matrix to an external .csv (in same drive as this doc is saved), set save.params to 1 ##
input[4,1] <- 1		## please note this will OVERWRITE every time the matrix runs
						## so files will need to be renamed/moved to be kept.


### Genotype Frequencies ###
## setting up to get proportions of genotypes in population
## User enters value of P - frequency of resistance allele at locus 1&2 respectively
input[5,1] <- 0.001	# locus 1
input[6,1] <- 0.001	# locus 2

## From this, the function HW will find the proportions of each genotype
## RR = p, RS = pq, SS = q
## P = p = R

## Recombination ##
input[7,1] <- 0.5		# recombination rate

### Insecticides are represented as a and b ###
### small case = low concentration, upper case = high concentration, 0 = absence (zero not UC o). ###

### Exposure Values ###
## Exposure levels of males and females to each insecticide niche ##
# males
input[8,1] <- 0.1	#Niche -,-

input[9,1] <- 0
input[10,1] <- 0

input[11,1] <- 0
input[12,1] <- 0

input[13,1] <-
input[14,1] <- 0.9	#Niche A,B

input[15,1] <- 0
input[16,1] <- 0

#a.m <- sum(a.m_00, a.m_a0, a.m_A0, a.m_0b, a.m_0B, a.m_ab, a.m_AB, a.m_Ab, a.m_aB)
#if ( a.m != 1 ){		 
#	print( paste("Error in male exposures: must total one: ", a.m) )
	
#	}else{
#		print( paste( "Male exposures total 1: ", a.m ))
#		}
		

# females
input[17,1] <- 0.1	#niche -,-

input[18,1] <- 0
input[19,1] <- 0

input[20,1] <- 0
input[21,1] <- 0

input[22,1] <- 0
input[23,1] <- 0.9	#niche A,B

input[24,1] <- 0
input[25,1] <- 0

#a.f <- sum(a.f_00, a.f_a0, a.f_A0, a.f_0b, a.f_0B, a.f_ab, a.f_AB, a.f_Ab, a.f_aB)
#if ( a.f != 1 ){		 
#	print( paste("Error in female exposures: must total one: ", a.f) )
#	
#	}else{
#		print( paste( "female exposures total 1: ", a.f ))
#		}
		
		
### Selection from distributions ###
### Fitness Values ###
## Baseline of SS in each insecticide/concentration (NOT niche, see Table 3. of brief)
## User entered fitness values to allow some survival of homozygote susceptible due to chance
# set as variables to be used in function calls/equations
# phi = baseline fitness value
input[26,1] <- 0
input[27,1] <- 1	#Phi SS1 in A

input[28,1] <- 0
input[29,1] <- 1	#Phi SS2 in B

# fitness of SS in environment with no insecticide are set to 1
# W = fitness value
input[30,1] <- 1
input[31,1] <- 1

## Dominance and selection coefficients
## needed to find fitness values of genotype in exposure to relating insecticide 
# h = dominance coefficient
input[32,1] <- 0

input[33,1] <- 0
input[34,1] <- 1	#Dominance coefficient in A

input[35,1] <- 0

input[36,1] <- 0
input[37,1] <- 1	#Dominance coefficient in B

# s = selection coefficient
input[38,1] <- 0
input[39,1] <- 1	#in A

input[40,1] <- 0
input[41,1] <- 1	#in B

# z = fitness cost of resistance allele in insecticide free environment
input[42,1] <- 0
input[43,1] <- 0

### Toggle Insecticide Niches on and off ###
## Allows for setting of specific combinations of insecticide niches to be used
## if toggled FALSE the calculation of fitness in that niche is cancelled and results printed as 0
## even if all set to TRUE, calibration == 1011||1012 will change the correct ones to OFF to run Curtis/Comparator
input[44,1] <- 1	#Niche 00

input[45,1] <- 0
input[46,1] <- 0

input[47,1] <- 0
input[48,1] <- 0

input[49,1] <- 0
input[50,1] <- 1	#Niche AB

input[51,1] <- 0
input[52,1] <- 0

input[,2] <- input[,1]
}

# Calibration 1012 - Fig 2 Curtis: Sequential (B,A) and then combination treatments
if( params.csv == FALSE && calibration == 1012 ){
input <- matrix( ncol=3, nrow=52 )
colnames(input) <- c("B (HCH)", "A (DDT)","Combination")
### Sequential Treatment 
## Column 1 - Scenario 1, Insecticide B (Dieldrin)
input[1,1] <- calibration

### Results Matrix ###
# Give number of generations
input[2,1] <- 70

### Saving Parameters ###
# To save parameters in a matrix, set coll.fitvals to 1 ##
input[3,1] <- 1
# To save this matrix to an external .csv (in same drive as this doc is saved), set save.params to 1 ##
input[4,1] <- 1		## please note this will OVERWRITE every time the matrix runs
						## so files will need to be renamed/moved to be kept.


### Genotype Frequencies ###
## setting up to get proportions of genotypes in population
## User enters value of P - frequency of resistance allele at locus 1&2 respectively
input[5,1] <- 0.01	# locus 1
input[6,1] <- 0.01	# locus 2

## From this, the function HW will find the proportions of each genotype
## RR = p, RS = pq, SS = q
## P = p = R

## Recombination ##
input[7,1] <- 0.5		# recombination rate

### Insecticides are represented as a and b ###
### small case = low concentration, upper case = high concentration, 0 = absence (zero not UC o). ###

### Exposure Values ###
## Exposure levels of males and females to each insecticide niche ##
# males
input[8,1] <- 0.1	#Niche -,-

input[9,1] <- 0
input[10,1] <- 0

input[11,1] <- 0
input[12,1] <- 0.9  #Niche 0,B

input[13,1] <- 0
input[14,1] <- 0

input[15,1] <- 0
input[16,1] <- 0

#a.m <- sum(a.m_00, a.m_a0, a.m_A0, a.m_0b, a.m_0B, a.m_ab, a.m_AB, a.m_Ab, a.m_aB)
#if ( a.m != 1 ){		 
#	print( paste("Error in male exposures: must total one: ", a.m) )
	
#	}else{
#		print( paste( "Male exposures total 1: ", a.m ))
#		}
		

# females
input[17,1] <- 0.1	#niche -,-

input[18,1] <- 0
input[19,1] <- 0

input[20,1] <- 0
input[21,1] <- 0.9	#Niche 0,B

input[22,1] <- 0
input[23,1] <- 0

input[24,1] <- 0
input[25,1] <- 0

#a.f <- sum(a.f_00, a.f_a0, a.f_A0, a.f_0b, a.f_0B, a.f_ab, a.f_AB, a.f_Ab, a.f_aB)
#if ( a.f != 1 ){		 
#	print( paste("Error in female exposures: must total one: ", a.f) )
#	
#	}else{
#		print( paste( "female exposures total 1: ", a.f ))
#		}
		
		
### Selection from distributions ###
### Fitness Values ###
## Baseline of SS in each insecticide/concentration (NOT niche, see Table 3. of brief)
## User entered fitness values to allow some survival of homozygote susceptible due to chance
# set as variables to be used in function calls/equations
# phi = baseline fitness value
input[26,1] <- 0
input[27,1] <- 0.73	#Phi SS1 in A

input[28,1] <- 0
input[29,1] <- 1	#Phi SS2 in B

# fitness of SS in environment with no insecticide are set to 1
# W = fitness value
input[30,1] <- 1
input[31,1] <- 1

## Dominance and selection coefficients
## needed to find fitness values of genotype in exposure to relating insecticide 
# h = dominance coefficient
input[32,1] <- 0

input[33,1] <- 0
input[34,1] <- 0.17	#Dominance coefficient in A

input[35,1] <- 0

input[36,1] <- 0
input[37,1] <- 0.0016	#Dominance coefficient in B

# s = selection coefficient
input[38,1] <- 0
input[39,1] <- 0.23	#in A

input[40,1] <- 0
input[41,1] <- 0.43	#in B

# z = fitness cost of resistance allele in insecticide free environment
input[42,1] <- 0
input[43,1] <- 0

### Toggle Insecticide Niches on and off ###
## Allows for setting of specific combinations of insecticide niches to be used
## if toggled FALSE the calculation of fitness in that niche is cancelled and results printed as 0
## even if all set to TRUE, calibration == 1011||1012 will change the correct ones to OFF to run Curtis/Comparator
input[44,1] <- 1	#Niche 00

input[45,1] <- 0
input[46,1] <- 0

input[47,1] <- 0
input[48,1] <- 1	#Niche 0B

input[49,1] <- 0
input[50,1] <- 0

input[51,1] <- 0
input[52,1] <- 0

## Scenario 2 - Insecticide A (DDT) ####
input[1,2] <- calibration

### Results Matrix ###
# Give number of generations
input[2,2] <- 70

### Saving Parameters ###
# To save parameters in a matrix, set coll.fitvals to 1 ##
input[3,2] <- 1
# To save this matrix to an external .csv (in same drive as this doc is saved), set save.params to 1 ##
input[4,2] <- 1		## please note this will OVERWRITE every time the matrix runs
						## so files will need to be renamed/moved to be kept.


### Genotype Frequencies ###
## setting up to get proportions of genotypes in population
## User enters value of P - frequency of resistance allele at locus 1&2 respectively
input[5,2] <- 0.01	# locus 1
input[6,2] <- 0.01	# locus 2

## From this, the function HW will find the proportions of each genotype
## RR = p, RS = pq, SS = q
## P = p = R

## Recombination ##
input[7,2] <- 0.5		# recombination rate

### Insecticides are represented as a and b ###
### small case = low concentration, upper case = high concentration, 0 = absence (zero not UC o). ###

### Exposure Values ###
## Exposure levels of males and females to each insecticide niche ##
# males
input[8,2] <- 0.1	#Niche -,-

input[9,2] <- 0
input[10,2] <- 0.9	#Niche A,0

input[11,2] <- 0
input[12,2] <- 0

input[13,2] <- 0
input[14,2] <- 0

input[15,2] <- 0
input[16,2] <- 0

#a.m <- sum(a.m_00, a.m_a0, a.m_A0, a.m_0b, a.m_0B, a.m_ab, a.m_AB, a.m_Ab, a.m_aB)
#if ( a.m != 1 ){		 
#	print( paste("Error in male exposures: must total one: ", a.m) )
	
#	}else{
#		print( paste( "Male exposures total 1: ", a.m ))
#		}
		

# females
input[17,2] <- 0.1	#niche -,-

input[18,2] <- 0
input[19,2] <- 0.9	#Niche A,0

input[20,2] <- 0
input[21,2] <- 0

input[22,2] <- 0
input[23,2] <- 0

input[24,2] <- 0
input[25,2] <- 0

#a.f <- sum(a.f_00, a.f_a0, a.f_A0, a.f_0b, a.f_0B, a.f_ab, a.f_AB, a.f_Ab, a.f_aB)
#if ( a.f != 1 ){		 
#	print( paste("Error in female exposures: must total one: ", a.f) )
#	
#	}else{
#		print( paste( "female exposures total 1: ", a.f ))
#		}
		
		
### Selection from distributions ###
### Fitness Values ###
## Baseline of SS in each insecticide/concentration (NOT niche, see Table 3. of brief)
## User entered fitness values to allow some survival of homozygote susceptible due to chance
# set as variables to be used in function calls/equations
# phi = baseline fitness value
input[26,2] <- 0
input[27,2] <- 0.73	#Phi SS1 in A

input[28,2] <- 0
input[29,2] <- 1	#Phi SS2 in B

# fitness of SS in environment with no insecticide are set to 1
# W = fitness value
input[30,2] <- 1
input[31,2] <- 1

## Dominance and selection coefficients
## needed to find fitness values of genotype in exposure to relating insecticide 
# h = dominance coefficient
input[32,2] <- 0

input[33,2] <- 0
input[34,2] <- 0.17	#Dominance coefficient in A

input[35,2] <- 0

input[36,2] <- 0
input[37,2] <- 0.0016	#Dominance coefficient in B

# s = selection coefficient
input[38,2] <- 0
input[39,2] <- 0.23	#in A

input[40,2] <- 0
input[41,2] <- 0.43	#in B

# z = fitness cost of resistance allele in insecticide free environment
input[42,2] <- 0
input[43,2] <- 0

### Toggle Insecticide Niches on and off ###
## Allows for setting of specific combinations of insecticide niches to be used
## if toggled FALSE the calculation of fitness in that niche is cancelled and results printed as 0
## even if all set to TRUE, calibration == 1011||1012 will change the correct ones to OFF to run Curtis/Comparator
input[44,2] <- 1	#Niche 00

input[45,2] <- 0
input[46,2] <- 1	#Niche A0

input[47,2] <- 0
input[48,2] <- 0

input[49,2] <- 0
input[50,2] <- 0

input[51,2] <- 0
input[52,2] <- 0

### Scenario 3 Combination Treatment ####
input[1,3] <- calibration

### Results Matrix ###
# Give number of generations
input[2,3] <- 160

### Saving Parameters ###
# To save parameters in a matrix, set coll.fitvals to 1 ##
input[3,3] <- 1
# To save this matrix to an external .csv (in same drive as this doc is saved), set save.params to 1 ##
input[4,3] <- 1		## please note this will OVERWRITE every time the matrix runs
						## so files will need to be renamed/moved to be kept.


### Genotype Frequencies ###
## setting up to get proportions of genotypes in population
## User enters value of P - frequency of resistance allele at locus 1&2 respectively
input[5,3] <- 0.01	# locus 1
input[6,3] <- 0.01	# locus 2

## From this, the function HW will find the proportions of each genotype
## RR = p, RS = pq, SS = q
## P = p = R

## Recombination ##
input[7,3] <- 0.5		# recombination rate

### Insecticides are represented as a and b ###
### small case = low concentration, upper case = high concentration, 0 = absence (zero not UC o). ###

### Exposure Values ###
## Exposure levels of males and females to each insecticide niche ##
# males
input[8,3] <- 0.1	#Niche -,-

input[9,3] <- 0
input[10,3] <- 0

input[11,3] <- 0
input[12,3] <- 0

input[13,3] <-
input[14,3] <- 0.9	#Niche A,B

input[15,3] <- 0
input[16,3] <- 0

#a.m <- sum(a.m_00, a.m_a0, a.m_A0, a.m_0b, a.m_0B, a.m_ab, a.m_AB, a.m_Ab, a.m_aB)
#if ( a.m != 1 ){		 
#	print( paste("Error in male exposures: must total one: ", a.m) )
	
#	}else{
#		print( paste( "Male exposures total 1: ", a.m ))
#		}
		

# females
input[17,3] <- 0.1	#niche -,-

input[18,3] <- 0
input[19,3] <- 0

input[20,3] <- 0
input[21,3] <- 0

input[22,3] <- 0
input[23,3] <- 0.9	#niche A,B

input[24,3] <- 0
input[25,3] <- 0

#a.f <- sum(a.f_00, a.f_a0, a.f_A0, a.f_0b, a.f_0B, a.f_ab, a.f_AB, a.f_Ab, a.f_aB)
#if ( a.f != 1 ){		 
#	print( paste("Error in female exposures: must total one: ", a.f) )
#	
#	}else{
#		print( paste( "female exposures total 1: ", a.f ))
#		}
		
		
### Selection from distributions ###
### Fitness Values ###
## Baseline of SS in each insecticide/concentration (NOT niche, see Table 3. of brief)
## User entered fitness values to allow some survival of homozygote susceptible due to chance
# set as variables to be used in function calls/equations
# phi = baseline fitness value
input[26,3] <- 0
input[27,3] <- 0.73	#Phi SS1 in A

input[28,3] <- 0
input[29,3] <- 1	#Phi SS2 in B

# fitness of SS in environment with no insecticide are set to 1
# W = fitness value
input[30,3] <- 1
input[31,3] <- 1

## Dominance and selection coefficients
## needed to find fitness values of genotype in exposure to relating insecticide 
# h = dominance coefficient
input[32,3] <- 0

input[33,3] <- 0
input[34,3] <- 0.17	#Dominance coefficient in A

input[35,3] <- 0

input[36,3] <- 0
input[37,3] <- 0.00016	#Dominance coefficient in B

# s = selection coefficient
input[38,3] <- 0
input[39,3] <- 0.23	#in A

input[40,3] <- 0
input[41,3] <- 0.43	#in B

# z = fitness cost of resistance allele in insecticide free environment
input[42,3] <- 0
input[43,3] <- 0

### Toggle Insecticide Niches on and off ###
## Allows for setting of specific combinations of insecticide niches to be used
## if toggled FALSE the calculation of fitness in that niche is cancelled and results printed as 0
## even if all set to TRUE, calibration == 1011||1012 will change the correct ones to OFF to run Curtis/Comparator
input[44,3] <- 1	#Niche 00

input[45,3] <- 0
input[46,3] <- 0

input[47,3] <- 0
input[48,3] <- 0

input[49,3] <- 0
input[50,3] <- 1	#Niche AB

input[51,3] <- 0
input[52,3] <- 0
}

if( params.csv == FALSE ){
rownames(input) <- c( "Calibration (100 default)","Number of generations ",                    
"Collect fitness scores in matrix (1/0)","Export fitness scores matrix to .csv (1/0)",
"Frequency of R at locus 1","Frequency of R at locus 2","Recombination Rate","Exposure Males -,-","Exposure Males a,-",                      
"Exposure Males A,-","Exposure Males -,b","Exposure Males -,B","Exposure males a,b","Exposure Males A,B",                        
"Exposure Males A,b","Exposure Males a,B","Exposure Females -,-","Exposure Females a,-","Exposure Females A,-",                      
"Exposure Females -,b","Exposure Females -,B","Exposure Females a,b","Exposure Females A,B","Exposure Females A,b",                      
"Exposure Females a,B","Baseline fitness of SS1 in a,-","Baseline fitness of SS1 in A,-","Baseline fitness of SS2 in -,b",
"Baseline fitness of SS2 in -,B","Fitness of SS1 in -,-","Fitness of SS2 in -,-","Dominance coefficient L1 in -,-",         
"Dominance coefficient L1 in a,-","Dominance coefficient L1 in A,-","Dominance coefficient L2 in -,-","Dominance coefficient L2 in -,b",           
"Dominance coefficient L2 in -,B","Selection coefficient L1 in a","Selection coefficient L1 in A","Selection coefficient L2 in b",            
"Selection coefficient L2 in B","Fitness cost of R at L1 in -,-","Fitness cost of R at L2 in -,-","Niche -,- on (1/0)",                       
"Niche a,- on","Niche A,- on","Niche -,b on","Niche -,B on","Niche a,b on","Niche A,B on","Niche A,b on","Niche a,B on" )
}

## parameters inputted from input file - make into matrix from data.frame
if( params.csv == TRUE ){
input <- make.matrix(input, input$Input)
}


#### Run Model ####
## Run through columns of matrix "input" to run model for each scenario set
for (i in 1:ncol( input ) ){
  ### Calibrations ###
  calibration <- input[1,i]
  
  ### Results Matrix ###
  # Give number of generations
  max_gen <- input[2,i]
  
  ### Saving Parameters ###
  # To save parameters in a matrix, set coll.fitvals to 1 ##
  coll.fitvals <- input[3,i]
  # To save this matrix to an external .csv (in same drive as this doc is saved), set save.params to 1 ##
  save.fitvals <- input[4,i]		## please note this will OVERWRITE every time the matrix runs
  						## so files will need to be renamed/moved to be kept.
  
  
  ### Genotype Frequencies ###
  ## setting up to get proportions of genotypes in population
  ## User enters value of P - frequency of resistance allele at locus 1&2 respectively
  P_1 <- input[5,i]	# locus 1
  P_2	<- input[6,i]	# locus 2
  
  ## From this, the function HW will find the proportions of each genotype
  ## RR = p, RS = pq, SS = q
  ## P = p = R
  
  ## Recombination ##
  c <- input[7,i]		# recombination rate
  
  	
  ### Insecticides are represented as a and b ###
  ### small case = low concentration, upper case = high concentration, 0 = absence (zero not UC o). ###
  
  ### Exposure Values ###
  ## Exposure levels of males and females to each insecticide niche ##
  # males
  a.m_00 <- input[8,i]
  
  a.m_a0 <- input[9,i]
  a.m_A0 <- input[10,i]
  
  a.m_0b <- input[11,i]
  a.m_0B <- input[12,i]
  
  a.m_ab <- input[13,i]
  a.m_AB <- input[14,i]
  
  a.m_Ab <- input[15,i]
  a.m_aB <- input[16,i]
  
  #a.m <- sum(a.m_00, a.m_a0, a.m_A0, a.m_0b, a.m_0B, a.m_ab, a.m_AB, a.m_Ab, a.m_aB)
  #if ( a.m != 1 ){		 
  #	print( paste("Error in male exposures: must total one: ", a.m) )
  	
  #	}else{
  #		print( paste( "Male exposures total 1: ", a.m ))
  #		}
  		
  
  # females
  a.f_00 <- input[17,i]
  
  a.f_a0 <- input[18,i]
  a.f_A0 <- input[19,i]
  
  a.f_0b <- input[20,i]
  a.f_0B <- input[21,i]
  
  a.f_ab <- input[22,i]
  a.f_AB <- input[23,i]
  
  a.f_Ab <- input[24,i]
  a.f_aB <- input[25,i]
  
  #a.f <- sum(a.f_00, a.f_a0, a.f_A0, a.f_0b, a.f_0B, a.f_ab, a.f_AB, a.f_Ab, a.f_aB)
  #if ( a.f != 1 ){		 
  #	print( paste("Error in female exposures: must total one: ", a.f) )
  #	
  #	}else{
  #		print( paste( "female exposures total 1: ", a.f ))
  #		}
  		
  		
  ### Selection from distributions ###
  ### Fitness Values ###
  ## Baseline of SS in each insecticide/concentration (NOT niche, see Table 3. of brief)
  ## User entered fitness values to allow some survival of homozygote susceptible due to chance
  # set as variables to be used in function calls/equations
  # phi = baseline fitness value
  phi.SS1_a0 <- input[26,i]
  phi.SS1_A0 <- input[27,i]
  
  phi.SS2_0b <- input[28,i]
  phi.SS2_0B <- input[29,i]
  
  # fitness of SS in environment with no insecticide are set to 1
  # W = fitness value
  W.SS1_00 <- input[30,i]
  W.SS2_00 <- input[31,i]
  
  ## Dominance and selection coefficients
  ## needed to find fitness values of genotype in exposure to relating insecticide 
  # h = dominance coefficient
  h.RS1_00 <- input[32,i]
  
  h.RS1_a0 <- input[33,i]
  h.RS1_A0 <- input[34,i]
  
  h.RS2_00 <- input[35,i]
  
  h.RS2_0b <- input[36,i]
  h.RS2_0B <- input[37,i]
  
  # s = selection coefficient
  s.RR1_a0 <- input[38,i]
  s.RR1_A0 <- input[39,i]
  
  s.RR2_0b <- input[40,i]
  s.RR2_0B <- input[41,i]
  
  # z = fitness cost of resistance allele in insecticide free environment
  z.RR1_00 <- input[42,i]
  z.RR2_00 <- input[43,i]

  ### Toggle Insecticide Niches on and off ###
  ## Allows for setting of specific combinations of insecticide niches to be used
  ## if toggled FALSE the calculation of fitness in that niche is cancelled and results printed as 0
  ## even if all set to TRUE, calibration == 1011||1012 will change the correct ones to OFF to run Curtis/Comparator
  niche_00 <- input[44,i]
  
  niche_a0 <- input[45,i]
  niche_A0 <- input[46,i]
  
  niche_0b <- input[47,i]
  niche_0B <- input[48,i]
  
  niche_ab <- input[49,i]
  niche_AB <- input[50,i]
  
  niche_Ab <- input[51,i]
  niche_aB <- input[52,i]
  
  
  
  #### USER INPUT ENDS HERE ####

  ## Set up matrices to print results to ####
  # Set up results matrix - prints overall freq of R and S allele per locus per sex, LD and overall allele freq (i.e. 1)
  results <- matrix ( nrow = max_gen, ncol = 11 )
  colnames( results ) <- c( "Gen", "m.R1", "m.R2", "m.LD", 
  								"f.R1", "f.R2", "f.LD", "M", "F", "dprime", "r2" )
  								
  # set up fitness by niche matrix - records fitness scores for each niche for each genotype
  fitness <- matrix ( nrow = 10, ncol = 9, c(rep(0,90)))
  colnames(fitness) <- c( "-,-", "a,-", "A,-", "b,-", "B,-", "a,b", "A,B", "A,b", "a,B" )
  rownames(fitness) <- c( "SS1SS2", "SS2RS2", "SS1RR2", 
  						"RS1SS2", "RS1RS2_cis", "RS1RS2_trans", "RS1RR2",
  						"RR1SS2", "RR1RS2", "RR1RR2")
  
  # set up genotype matrix - records frequencies of each of the 9 two locus genotypes each generation
  genotype <- matrix( nrow=max_gen, ncol=11 )
  colnames(genotype) <- c("gen", "SS1SS2", "SS2RS2", "SS1RR2", 
  						"RS1SS2", "RS1RS2_cis", "RS1RS2_trans", "RS1RR2",
  						"RR1SS2", "RR1RS2", "RR1RR2")
  ## make.genotypemat function will use this data and make a matrix of the genotype frequencies
  ## frequencies of genotypes before selection - in HW equilibrium and same in male and female
  ## needs name of matrix and takes corresponding frequency of resistant allele in function call
  genotype.freq <- make.genotypemat ( P_1, P_2 )
  ## Check for errors
  #if ( sum(genotype.freq)!=1 ){			## Will print an error message if the genotype frequencies do not total 1. 
  #	print( "Error in frequencies" )		## Unhash to use for bugfixing
  #	
  #	}else{
  #		print( "Frequencies total 1")
  #		}
  

  ## Calculated fitnesses ####
  # fitnesses calculated from baselines/coefficients as according to calibration table (Table 1)
  
  # absence of insecticide
  ## fitness of SS in absence of insecticide is entered above as a parameter
  W.RS1_00 <- 1 - (h.RS1_00 * z.RR1_00)
  W.RR1_00 <- 1 - z.RR1_00
  
  W.RS2_00 <- 1 - (h.RS2_00 * z.RR2_00)
  W.RR2_00 <- 1 - z.RR2_00
  
  # low levels of insecticide a
  W.SS1_a0 <- 1 - phi.SS1_a0
  W.RS1_a0 <- W.SS1_a0 + (h.RS1_a0 * s.RR1_a0)
  W.RR1_a0 <- W.SS1_a0 + s.RR1_a0
  
  # high levels of insecticide A
  W.SS1_A0 <- 1 - phi.SS1_A0
  W.RS1_A0 <- W.SS1_A0 + (h.RS1_A0 * s.RR1_A0)
  W.RR1_A0 <- W.SS1_A0 + s.RR1_A0
  
  # low levels of insecticide b
  W.SS2_0b <- 1 - phi.SS2_0b
  W.RS2_0b <- W.SS2_0b + (h.RS2_0b * s.RR2_0b)
  W.RR2_0b <- W.SS2_0b + s.RR2_0b
  
  # high levels of insecticide B
  W.SS2_0B <- 1 - phi.SS2_0B
  W.RS2_0B <- W.SS2_0B + (h.RS2_0B * s.RR2_0B)
  W.RR2_0B <- W.SS2_0B + s.RR2_0B
  
  ### Two genotype fitnesses in two insecticide Niche ####
  ## The ifelse clauses allow niches to be toggled on/off, i.e. a fitness can be given for A0
  ## but if only the niche A,B is toggled on, the fitness scores for A0 and Ab will be set to 0
  ## Fitness in specific niche is calculated by multipling fitness of two insecticides/absences present
  ## See table 4 of briefing document
  
  # -,- niche
  if( niche_00 == 0 ){
  	# SS1
  	W.SS1SS2_00 <- 0
  	W.SS1RS2_00 <- 0
  	W.SS1RR2_00 <- 0
  	# RS1
  	W.RS1SS2_00 <- 0
  	W.RS1RS2_00 <- 0
  	W.RS1RR2_00 <- 0
  	# RR1
  	W.RR1SS2_00 <- 0
  	W.RR1RS2_00 <- 0
  	W.RR1RR2_00 <- 0
  	}else{
  		# SS1
  		W.SS1SS2_00 <- W.SS1_00 * W.SS2_00
  		W.SS1RS2_00 <- W.SS1_00 * W.RS2_00
  		W.SS1RR2_00 <- W.SS1_00 * W.RR2_00
  		# RS1
  		W.RS1SS2_00 <- W.RS1_00 * W.SS2_00
  		W.RS1RS2_00 <- W.RS1_00 * W.RS2_00
  		W.RS1RR2_00 <- W.RS1_00 * W.RR2_00
  		# RR1
  		W.RR1SS2_00 <- W.RR1_00 * W.SS2_00
  		W.RR1RS2_00 <- W.RR1_00 * W.RS2_00
  		W.RR1RR2_00 <- W.RR1_00 * W.RR2_00
  		}
  

  # a,- niche ####
  if( niche_a0 == 0 ){
  	# SS1
  	# SS1
  	W.SS1SS2_a0 <- 0
  	W.SS1RS2_a0 <- 0
  	W.SS1RR2_a0 <- 0
  	# RS1
  	W.RS1SS2_a0 <- 0
  	W.RS1RS2_a0 <- 0
  	W.RS1RR2_a0 <- 0
  	# RR1
  	W.RR1SS2_a0 <- 0
  	W.RR1RS2_a0 <- 0
  	W.RR1RR2_a0 <- 0
  	}else{
  		# SS1
  		W.SS1SS2_a0 <- W.SS1_a0 * W.SS2_00
  		W.SS1RS2_a0 <- W.SS1_a0 * W.RS2_00
  		W.SS1RR2_a0 <- W.SS1_a0 * W.RR2_00
  		# RS1
  		W.RS1SS2_a0 <- W.RS1_a0 * W.SS2_00
  		W.RS1RS2_a0 <- W.RS1_a0 * W.RS2_00
  		W.RS1RR2_a0 <- W.RS1_a0 * W.RR2_00
  		# RR1
  		W.RR1SS2_a0 <- W.RR1_a0 * W.SS2_00
  		W.RR1RS2_a0 <- W.RR1_a0 * W.RS2_00
  		W.RR1RR2_a0 <- W.RR1_a0 * W.RR2_00
  		}
  
  # A,- niche ####
  if( niche_A0 == 0 ){
  	# SS1
  	W.SS1SS2_A0 <- 0
  	W.SS1RS2_A0 <- 0
  	W.SS1RR2_A0 <- 0
  	# RS1
  	W.RS1SS2_A0 <- 0
  	W.RS1RS2_A0 <- 0
  	W.RS1RR2_A0 <- 0
  	# RR1
  	W.RR1SS2_A0 <- 0
  	W.RR1RS2_A0 <- 0
  	W.RR1RR2_A0 <- 0
  	}else{
  		# SS1
  		W.SS1SS2_A0 <- W.SS1_A0 * W.SS2_00
  		W.SS1RS2_A0 <- W.SS1_A0 * W.RS2_00
  		W.SS1RR2_A0 <- W.SS1_A0 * W.RR2_00
  		# RS1
  		W.RS1SS2_A0 <- W.RS1_A0 * W.SS2_00
  		W.RS1RS2_A0 <- W.RS1_A0 * W.RS2_00
  		W.RS1RR2_A0 <- W.RS1_A0 * W.RR2_00
  		# RR1
  		W.RR1SS2_A0 <- W.RR1_A0 * W.SS2_00
  		W.RR1RS2_A0 <- W.RR1_A0 * W.RS2_00
  		W.RR1RR2_A0 <- W.RR1_A0 * W.RR2_00
  		}
  

  # -,b niche ####
  if( niche_0B == 0 ){
  	# SS1
  	W.SS1SS2_0b <- 0
  	W.SS1RS2_0b <- 0
  	W.SS1RR2_0b <- 0
  	# RS1
  	W.RS1SS2_0b <- 0
  	W.RS1RS2_0b <- 0
  	W.RS1RR2_0b <- 0
  	# RR1
  	W.RR1SS2_0b <- 0
  	W.RR1RS2_0b <- 0
  	W.RR1RR2_0b <- 0
  	}else{
  		# SS1
  		W.SS1SS2_0b <- W.SS1_00 * W.SS2_0b
  		W.SS1RS2_0b <- W.SS1_00 * W.RS2_0b
  		W.SS1RR2_0b <- W.SS1_00 * W.RR2_0b
  		# RS1
  		W.RS1SS2_0b <- W.RS1_00 * W.SS2_0b
  		W.RS1RS2_0b <- W.RS1_00 * W.RS2_0b
  		W.RS1RR2_0b <- W.RS1_00 * W.RR2_0b
  		# RR1
  		W.RR1SS2_0b <- W.RR1_00 * W.SS2_0b
  		W.RR1RS2_0b <- W.RR1_00 * W.RS2_0b
  		W.RR1RR2_0b <- W.RR1_00 * W.RR2_0b
  		}
  
  # -,B niche ####
  if( niche_0B == 0 ){
  	# SS1
  	W.SS1SS2_0B <- 0
  	W.SS1RS2_0B <- 0
  	W.SS1RR2_0B <- 0
  	# RS1
  	W.RS1SS2_0B <- 0
  	W.RS1RS2_0B <- 0
  	W.RS1RR2_0B <- 0
  	# RR1
  	W.RR1SS2_0B <- 0
  	W.RR1RS2_0B <- 0
  	W.RR1RR2_0B <- 0
  	}else{
  		# SS1
  		W.SS1SS2_0B <- W.SS1_00 * W.SS2_0B
  		W.SS1RS2_0B <- W.SS1_00 * W.RS2_0B
  		W.SS1RR2_0B <- W.SS1_00 * W.RR2_0B
  		# RS1
  		W.RS1SS2_0B <- W.RS1_00 * W.SS2_0B
  		W.RS1RS2_0B <- W.RS1_00 * W.RS2_0B
  		W.RS1RR2_0B <- W.RS1_00 * W.RR2_0B
  		# RR1
  		W.RR1SS2_0B <- W.RR1_00 * W.SS2_0B
  		W.RR1RS2_0B <- W.RR1_00 * W.RS2_0B
  		W.RR1RR2_0B <- W.RR1_00 * W.RR2_0B
  		}
  
  # a,b niche ####
  if( niche_ab == 0 ){
  	# SS1
  	W.SS1SS2_ab <- 0
  	W.SS1RS2_ab <- 0
  	W.SS1RR2_ab <- 0
  	# RS1
  	W.RS1SS2_ab <- 0
  	W.RS1RS2_ab <- 0
  	W.RS1RR2_ab <- 0
  	# RR1
  	W.RR1SS2_ab <- 0
  	W.RR1RS2_ab <- 0
  	W.RR1RR2_ab <- 0
  
  	}else{
  		# SS1
  		W.SS1SS2_ab <- W.SS1_a0 * W.SS2_0b
  		W.SS1RS2_ab <- W.SS1_a0 * W.RS2_0b
  		W.SS1RR2_ab <- W.SS1_a0 * W.RR2_0b
  		# RS1
  		W.RS1SS2_ab <- W.RS1_a0 * W.SS2_0b
  		W.RS1RS2_ab <- W.RS1_a0 * W.RS2_0b
  		W.RS1RR2_ab <- W.RS1_a0 * W.RR2_0b
  		# RR1
  		W.RR1SS2_ab <- W.RR1_a0 * W.SS2_0b
  		W.RR1RS2_ab <- W.RR1_a0 * W.RS2_0b
  		W.RR1RR2_ab <- W.RR1_a0 * W.RR2_0b
  
  		}
  
  # A,B niche ####
  if( niche_AB == 0 ){
  	# SS1
  	W.SS1SS2_AB <- 0
  	W.SS1RS2_AB <- 0
  	W.SS1RR2_AB <- 0
  	# RS1
  	W.RS1SS2_AB <- 0
  	W.RS1RS2_AB <- 0
  	W.RS1RR2_AB <- 0
  	# RR1
  	W.RR1SS2_AB <- 0
  	W.RR1RS2_AB <- 0
  	W.RR1RR2_AB <- 0
  	}else{
  		# SS1
  		W.SS1SS2_AB <- W.SS1_A0 * W.SS2_0B
  		W.SS1RS2_AB <- W.SS1_A0 * W.RS2_0B
  		W.SS1RR2_AB <- W.SS1_A0 * W.RR2_0B
  		# RS1
  		W.RS1SS2_AB <- W.RS1_A0 * W.SS2_0B
  		W.RS1RS2_AB <- W.RS1_A0 * W.RS2_0B
  		W.RS1RR2_AB <- W.RS1_A0 * W.RR2_0B
  		# RR1
  		W.RR1SS2_AB <- W.RR1_A0 * W.SS2_0B
  		W.RR1RS2_AB <- W.RR1_A0 * W.RS2_0B
  		W.RR1RR2_AB <- W.RR1_A0 * W.RR2_0B
  
  		}
  
  
  # A,b niche ####
  if( niche_Ab == 0 ){
  	# SS1
  	W.SS1SS2_Ab <- 0
  	W.SS1RS2_Ab <- 0
  	W.SS1RR2_Ab <- 0
  	# RS1
  	W.RS1SS2_Ab <- 0
  	W.RS1RS2_Ab <- 0
  	W.RS1RR2_Ab <- 0
  	# RR1
  	W.RR1SS2_Ab <- 0
  	W.RR1RS2_Ab <- 0
  	W.RR1RR2_Ab <- 0
  	}else{
  		# SS1
  		W.SS1SS2_Ab <- W.SS1_A0 * W.SS2_0b
  		W.SS1RS2_Ab <- W.SS1_A0 * W.RS2_0b
  		W.SS1RR2_Ab <- W.SS1_A0 * W.RR2_0b
  		# RS1
  		W.RS1SS2_Ab <- W.RS1_A0 * W.SS2_0b
  		W.RS1RS2_Ab <- W.RS1_A0 * W.RS2_0b
  		W.RS1RR2_Ab <- W.RS1_A0 * W.RR2_0b
  		# RR1
  		W.RR1SS2_Ab <- W.RR1_A0 * W.SS2_0b
  		W.RR1RS2_Ab <- W.RR1_A0 * W.RS2_0b
  		W.RR1RR2_Ab <- W.RR1_A0 * W.RR2_0b
  
  		}
  
  # a,B niche
  if( niche_aB == 0 ){
  	# SS1
  	W.SS1SS2_aB <- 0
  	W.SS1RS2_aB <- 0
  	W.SS1RR2_aB <- 0
  	# RS1
  	W.RS1SS2_aB <- 0
  	W.RS1RS2_aB <- 0
  	W.RS1RR2_aB <- 0
  	# RR1
  	W.RR1SS2_aB <- 0
  	W.RR1RS2_aB <- 0
  	W.RR1RR2_aB <- 0
  	}else{
  		# SS1
  		W.SS1SS2_aB <- W.SS1_a0 * W.SS2_0B
  		W.SS1RS2_aB <- W.SS1_a0 * W.RS2_0B
  		W.SS1RR2_aB <- W.SS1_a0 * W.RR2_0B
  		# RS1
  		W.RS1SS2_aB <- W.RS1_a0 * W.SS2_0B
  		W.RS1RS2_aB <- W.RS1_a0 * W.RS2_0B
  		W.RS1RR2_aB <- W.RS1_a0 * W.RR2_0B
  		# RR1
  		W.RR1SS2_aB <- W.RR1_a0 * W.SS2_0B
  		W.RR1RS2_aB <- W.RR1_a0 * W.RS2_0B
  		W.RR1RR2_aB <- W.RR1_a0 * W.RR2_0B
  
  		}
  
  
  ### Calculating fitness determining selection ####  
  ## These are calculated from the individual's fitness by two locus genotype, and exposure to niche depending on gender
  
  ## Males
  ## SS1
  # SS2
  W.m.SS1SS2 <- (a.m_00 * W.SS1SS2_00) + 
  			  (a.m_a0 * W.SS1SS2_a0) + (a.m_A0 * W.SS1SS2_A0) + 
  			  (a.m_0b * W.SS1SS2_0b) + (a.m_0B * W.SS1SS2_0B) + 
  			  (a.m_ab * W.SS1SS2_ab) + (a.m_AB * W.SS1SS2_AB) + 
  			  (a.m_Ab * W.SS1SS2_Ab) + (a.m_aB * W.SS1SS2_aB) 
  
  # RS2
  W.m.SS1RS2 <- (a.m_00 * W.SS1RS2_00) + 
  			  (a.m_a0 * W.SS1RS2_a0) + (a.m_A0 * W.SS1RS2_A0) + 
  			  (a.m_0b * W.SS1RS2_0b) + (a.m_0B * W.SS1RS2_0B) + 
  			  (a.m_ab * W.SS1RS2_ab) + (a.m_AB * W.SS1RS2_AB) + 
  			  (a.m_Ab * W.SS1RS2_Ab) + (a.m_aB * W.SS1RS2_aB)
  			 
  # RR2
  W.m.SS1RR2 <- (a.m_00 * W.SS1RR2_00) + 
  			  (a.m_a0 * W.SS1RR2_a0) + (a.m_A0 * W.SS1RR2_A0) + 
  			  (a.m_0b * W.SS1RR2_0b) + (a.m_0B * W.SS1RR2_0B) + 
  			  (a.m_ab * W.SS1RR2_ab) + (a.m_AB * W.SS1RR2_AB) + 
  			  (a.m_Ab * W.SS1RR2_Ab) + (a.m_aB * W.SS1RR2_aB)
  			  
  ## RS1
  # SS2
  W.m.RS1SS2 <- (a.m_00 * W.RS1SS2_00) + 
  			  (a.m_a0 * W.RS1SS2_a0) + (a.m_A0 * W.RS1SS2_A0) + 
  			  (a.m_0b * W.RS1SS2_0b) + (a.m_0B * W.RS1SS2_0B) + 
  			  (a.m_ab * W.RS1SS2_ab) + (a.m_AB * W.RS1SS2_AB) + 
  			  (a.m_Ab * W.RS1SS2_Ab) + (a.m_aB * W.RS1SS2_aB)
  			  			  
  # RS2
  W.m.RS1RS2 <- (a.m_00 * W.RS1RS2_00) + 
  			  (a.m_a0 * W.RS1RS2_a0) + (a.m_A0 * W.RS1RS2_A0) + 
  			  (a.m_0b * W.RS1RS2_0b) + (a.m_0B * W.RS1RS2_0B) + 
  			  (a.m_ab * W.RS1RS2_ab) + (a.m_AB * W.RS1RS2_AB) + 
  			  (a.m_Ab * W.RS1RS2_Ab) + (a.m_aB * W.RS1RS2_aB)
   
  # RR2
  W.m.RS1RR2 <- (a.m_00 * W.RS1RR2_00) + 
  			  (a.m_a0 * W.RS1RR2_a0) + (a.m_A0 * W.RS1RR2_A0) + 
  			  (a.m_0b * W.RS1RR2_0b) + (a.m_0B * W.RS1RR2_0B) + 
  			  (a.m_ab * W.RS1RR2_ab) + (a.m_AB * W.RS1RR2_AB) + 
  			  (a.m_Ab * W.RS1RR2_Ab) + (a.m_aB * W.RS1RR2_aB) 
  			
  			  
  ## RR1
  # SS2
  W.m.RR1SS2 <- (a.m_00 * W.RR1SS2_00) + 
  			  (a.m_a0 * W.RR1SS2_a0) + (a.m_A0 * W.RR1SS2_A0) + 
  			  (a.m_0b * W.RR1SS2_0b) + (a.m_0B * W.RR1SS2_0B) + 
  			  (a.m_ab * W.RR1SS2_ab) + (a.m_AB * W.RR1SS2_AB) + 
  			  (a.m_Ab * W.RR1SS2_Ab) + (a.m_aB * W.RR1SS2_aB) 
    
  # RS2
  W.m.RR1RS2 <- (a.m_00 * W.RR1RS2_00) + 
  			  (a.m_a0 * W.RR1RS2_a0) + (a.m_A0 * W.RR1RS2_A0) +
  			  (a.m_0b * W.RR1RS2_0b) + (a.m_0B * W.RR1RS2_0B) + 
  			  (a.m_ab * W.RR1RS2_ab) + (a.m_AB * W.RR1RS2_AB) + 
  			  (a.m_Ab * W.RR1RS2_Ab) + (a.m_aB * W.RR1RS2_aB) 
  			 
  # RR2
  W.m.RR1RR2 <- (a.m_00 * W.RR1RR2_00) + 
  			  (a.m_a0 * W.RR1RR2_a0) + (a.m_A0 * W.RR1RR2_A0) + 
  			  (a.m_0b * W.RR1RR2_0b) + (a.m_0B * W.RR1RR2_0B) + 
  			  (a.m_ab * W.RR1RR2_ab) + (a.m_AB * W.RR1RR2_AB) + 
  			  (a.m_Ab * W.RR1RR2_Ab) + (a.m_aB * W.RR1RR2_aB)
  			  		  
  			  
  			  
  
  ## Female
  ## SS1
  # SS2
  # SS2
  W.f.SS1SS2 <- (a.f_00 * W.SS1SS2_00) + 
  			  (a.f_a0 * W.SS1SS2_a0) + (a.f_A0 * W.SS1SS2_A0) +
  			  (a.f_0b * W.SS1SS2_0b) + (a.f_0B * W.SS1SS2_0B) + 
  			  (a.f_ab * W.SS1SS2_ab) + (a.f_AB * W.SS1SS2_AB) + 
  			  (a.f_Ab * W.SS1SS2_Ab) + (a.f_aB * W.SS1SS2_aB) 
  # RS2
  W.f.SS1RS2 <- (a.f_00 * W.SS1RS2_00) + 
  			  (a.f_a0 * W.SS1RS2_a0) + (a.f_A0 * W.SS1RS2_A0) +
  			  (a.f_0b * W.SS1RS2_0b) + (a.f_0B * W.SS1RS2_0B) + 
  			  (a.f_ab * W.SS1RS2_ab) + (a.f_AB * W.SS1RS2_AB) + 
  			  (a.f_Ab * W.SS1RS2_Ab) + (a.f_aB * W.SS1RS2_aB)
  # RR2
  W.f.SS1RR2 <- (a.f_00 * W.SS1RR2_00) + 
  			  (a.f_a0 * W.SS1RR2_a0) + (a.f_A0 * W.SS1RR2_A0) +
  			  (a.f_0b * W.SS1RR2_0b) + (a.f_0B * W.SS1RR2_0B) + 
  			  (a.f_ab * W.SS1RR2_ab) + (a.f_AB * W.SS1RR2_AB) + 
  			  (a.f_Ab * W.SS1RR2_Ab) + (a.f_aB * W.SS1RR2_aB)
  
  ## RS1
  # SS2
  W.f.RS1SS2 <- (a.f_00 * W.RS1SS2_00) + 
  			  (a.f_a0 * W.RS1SS2_a0) + (a.f_A0 * W.RS1SS2_A0) + 
  			  (a.f_0b * W.RS1SS2_0b) + (a.f_0B * W.RS1SS2_0B) + 
  			  (a.f_ab * W.RS1SS2_ab) + (a.f_AB * W.RS1SS2_AB) + 
  			  (a.f_Ab * W.RS1SS2_Ab) + (a.f_aB * W.RS1SS2_aB) 
  # RS2
  W.f.RS1RS2 <- (a.f_00 * W.RS1RS2_00) + 
  			  (a.f_a0 * W.RS1RS2_a0) + (a.f_A0 * W.RS1RS2_A0) + 
  			  (a.f_0b * W.RS1RS2_0b) + (a.f_0B * W.RS1RS2_0B) +
  			  (a.f_ab * W.RS1RS2_ab) + (a.f_AB * W.RS1RS2_AB) + 
  			  (a.f_Ab * W.RS1RS2_Ab) + (a.f_aB * W.RS1RS2_aB)  
  # RR2
  W.f.RS1RR2 <- (a.f_00 * W.RS1RR2_00) + 
  			  (a.f_a0 * W.RS1RR2_a0) + (a.f_A0 * W.RS1RR2_A0) + 
  			  (a.f_0b * W.RS1RR2_0b) + (a.f_0B * W.RS1RR2_0B) +
  			  (a.f_ab * W.RS1RR2_ab) + (a.f_AB * W.RS1RR2_AB) + 
  			  (a.f_Ab * W.RS1RR2_Ab) + (a.f_aB * W.RS1RR2_aB)  
  			  
  ## RR1
  # SS2
  W.f.RR1SS2 <- (a.f_00 * W.RR1SS2_00) +
  			  (a.f_a0 * W.RR1SS2_a0) + (a.f_A0 * W.RR1SS2_A0) +
  			  (a.f_0b * W.RR1SS2_0b) + (a.f_0B * W.RR1SS2_0B) +
  			  (a.f_ab * W.RR1SS2_ab) + (a.f_AB * W.RR1SS2_AB) + 
  			  (a.f_Ab * W.RR1SS2_Ab) + (a.f_aB * W.RR1SS2_aB)  
  # RS2
  W.f.RR1RS2 <- (a.f_00 * W.RR1RS2_00) + 
  			  (a.f_a0 * W.RR1RS2_a0) + (a.f_A0 * W.RR1RS2_A0) + 
  			  (a.f_0b * W.RR1RS2_0b) + (a.f_0B * W.RR1RS2_0B) + 
  			  (a.f_ab * W.RR1RS2_ab) + (a.f_AB * W.RR1RS2_AB) + 
  			  (a.f_Ab * W.RR1RS2_Ab) + (a.f_aB * W.RR1RS2_aB) 
  # RR2
  W.f.RR1RR2 <- (a.f_00 * W.RR1RR2_00) + 
  			  (a.f_a0 * W.RR1RR2_a0) + (a.f_A0 * W.RR1RR2_A0) +
  			  (a.f_0b * W.RR1RR2_0b) + (a.f_0B * W.RR1RR2_0B) + 
  			  (a.f_ab * W.RR1RR2_ab) + (a.f_AB * W.RR1RR2_AB) + 
  			  (a.f_Ab * W.RR1RR2_Ab) + (a.f_aB * W.RR1RR2_aB)
   
  			  
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 		
  ### Loop to run the model from the initial conditions generated above ####
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  

  
  	for (k in 1:max_gen){
  	
  	# In calibration 1011, between generations 5 and 12 selection is relaxed
  	# fitnesses use relaxed selection fitnesses calculated above
  	if( calibration == 1011 & i==2 ){
  	relax <- TRUE
  		}else{
  		relax <- FALSE
  		}
  	
  	if( relax == TRUE & (2<k) & (k<12)){
  		# relaxed selection fitnesses
  		## Males
  		## SS1
  		# SS2
  		W.m.SS1SS2 <- 0.1 
  		# RS2
  		W.m.SS1RS2 <- 0.1
  		# RR2
  		W.m.SS1RR2 <- 0.1
  
  		## RS1
  		# SS2
  		W.m.RS1SS2 <- 0.1
  		# RS2
  		W.m.RS1RS2 <- 0.1  
  		# RR2
  		W.m.RS1RR2 <- 0.1  
  					  
  		## RR1
  		# SS2
  		W.m.RR1SS2 <- 0.1 
  		# RS2
  		W.m.RR1RS2 <- 0.1 
  		# RR2
  		W.m.RR1RR2 <- 0.1  
  					  
  					  
  
  		## Female
  		## SS1
  		# SS2
  		# SS2
  		W.f.SS1SS2 <- 0.1
  		# RS2
  		W.f.SS1RS2 <- 0.1
  		# RR2
  		W.f.SS1RR2 <- 0.1
  
  		## RS1
  		# SS2
  		W.f.RS1SS2 <- 0.1
  		# RS2
  		W.f.RS1RS2 <- 0.1 
  		# RR2
  		W.f.RS1RR2 <- 0.1 
  					  
  		## RR1
  		# SS2
  		W.f.RR1SS2 <- 0.1
  		# RS2
  		W.f.RR1RS2 <- 0.1 
  		# RR2
  		W.f.RR1RR2 <- 0.1
  		} else if( relax == TRUE & (k>11) & (k<15)){
  		# non relaxed fitnesses
  		## Males
  		## SS1
  		# SS2
  		W.m.SS1SS2 <- 0.1
  		# RS2
  		W.m.SS1RS2 <- 0.1
  		# RR2
  		W.m.SS1RR2 <- 0.1
  
  		## RS1
  		# SS2
  		W.m.RS1SS2 <- 0.1
  		# RS2
  		W.m.RS1RS2 <- 1  
  		# RR2
  		W.m.RS1RR2 <- 1  
  					  
  		## RR1
  		# SS2
  		W.m.RR1SS2 <- 0.1
  		# RS2
  		W.m.RR1RS2 <- 1 
  		# RR2
  		W.m.RR1RR2 <- 1 
  					  
  		## Female
  		## SS1
  		# SS2
  		# SS2
  		W.f.SS1SS2 <- 0.1
  		# RS2
  		W.f.SS1RS2 <- 0.1
  		# RR2
  		W.f.SS1RR2 <- 0.1
  
  		## RS1
  		# SS2
  		W.f.RS1SS2 <- 0.1
  		# RS2
  		W.f.RS1RS2 <- 1 
  		# RR2
  		W.f.RS1RR2 <- 1 
  					  
  		## RR1
  		# SS2
  		W.f.RR1SS2 <- 0.1
  		# RS2
  		W.f.RR1RS2 <- 1 
  		# RR2
  		W.f.RR1RR2 <- 1
  		} else if( relax == TRUE & (k>14) ){
  	# relaxed selection fitnesses
  		## Males
  		## SS1
  		# SS2
  		W.m.SS1SS2 <- 0.1 
  		# RS2
  		W.m.SS1RS2 <- 0.1
  		# RR2
  		W.m.SS1RR2 <- 0.1
  
  		## RS1
  		# SS2
  		W.m.RS1SS2 <- 0.1
  		# RS2
  		W.m.RS1RS2 <- 0.1  
  		# RR2
  		W.m.RS1RR2 <- 0.1  
  					  
  		## RR1
  		# SS2
  		W.m.RR1SS2 <- 0.1 
  		# RS2
  		W.m.RR1RS2 <- 0.1 
  		# RR2
  		W.m.RR1RR2 <- 0.1  
  					  
  					  
  
  		## Female
  		## SS1
  		# SS2
  		# SS2
  		W.f.SS1SS2 <- 0.1
  		# RS2
  		W.f.SS1RS2 <- 0.1
  		# RR2
  		W.f.SS1RR2 <- 0.1
  
  		## RS1
  		# SS2
  		W.f.RS1SS2 <- 0.1
  		# RS2
  		W.f.RS1RS2 <- 0.1 
  		# RR2
  		W.f.RS1RR2 <- 0.1 
  					  
  		## RR1
  		# SS2
  		W.f.RR1SS2 <- 0.1
  		# RS2
  		W.f.RR1RS2 <- 0.1 
  		# RR2
  		W.f.RR1RR2 <- 0.1
  		}
  
  
  	# set genotype frequencies as variables
  	# extracted from genotype frequency matrix generated above from initial value of P (frequency of R allele)
  	# set for male and for female: before first round of selection these are the same values
  	# f = frequency before selection
  	
  	# male
  	f.m.SS1SS2 <- genotype.freq[1,]
  	f.m.SS1RS2 <- genotype.freq[2,]
  	f.m.SS1RR2 <- genotype.freq[3,]
  	f.m.RS1SS2 <- genotype.freq[4,]
  	f.m.RS1RS2_cis <- genotype.freq[5,]		### cis
  	f.m.RS1RS2_trans <- genotype.freq[6,]	### trans
  	f.m.RS1RR2 <- genotype.freq[7,]
  	f.m.RR1SS2 <- genotype.freq[8,]
  	f.m.RR1RS2 <- genotype.freq[9,]
  	f.m.RR1RR2 <- genotype.freq[10,]
  	# female
  	f.f.SS1SS2 <- genotype.freq[1,]
  	f.f.SS1RS2 <- genotype.freq[2,]
  	f.f.SS1RR2 <- genotype.freq[3,]
  	f.f.RS1SS2 <- genotype.freq[4,]
  	f.f.RS1RS2_cis <- genotype.freq[5,]		### cis
  	f.f.RS1RS2_trans <- genotype.freq[6,]	### trans
  	f.f.RS1RR2 <- genotype.freq[7,]
  	f.f.RR1SS2 <- genotype.freq[8,]
  	f.f.RR1RS2 <- genotype.freq[9,]
  	f.f.RR1RR2 <- genotype.freq[10,]
  	
  
  	# Male
  	#male.freq <- (f.m.SS1SS2 + f.m.SS1RS2 + f.m.SS1RR2+
  	#	  f.m.RS1SS2 + f.m.RS1RS2_cis + f.m.RS1RS2_trans + f.m.RS1RR2+
  	#	  f.m.RR1SS2 + f.m.RR1RS2 + f.m.RR1RR2) 
  	#print( (paste("Male frequencies before selection total = ",male.freq) ) )
  	# Female
  	#female.freq <- (f.f.SS1SS2 + f.f.SS1RS2 + f.f.SS1RR2+
  	#	  f.f.RS1SS2 + f.f.RS1RS2_cis + f.f.RS1RS2_trans + f.f.RS1RR2+
  	#	  f.f.RR1SS2 + f.f.RR1RS2 + f.f.RR1RR2) 			
  	#print( (paste("Female frequencies before selection total = ",female.freq) ) )
  
  	### Prints record of genotype proportions each generation
  		genotype[k,1] <- k					
  		genotype[k,2] <- f.m.SS1SS2
  		genotype[k,3] <- f.m.SS1RS2
  		genotype[k,4] <- f.m.SS1RR2
  
  		genotype[k,5] <- f.m.RS1SS2
  		genotype[k,6] <- f.m.RS1RS2_cis
  		genotype[k,7] <- f.m.RS1RS2_trans
  		genotype[k,8] <- f.m.RS1RR2
  
  		genotype[k,9] <- f.m.RR1SS2
  		genotype[k,10] <- f.m.RR1RS2
  		genotype[k,11] <- f.m.RR1RR2
  		
  	#### Printing Results to matrix ####
  	# print generation
  	results[k,1] <- k
  
  	# frequency of resistance allele in males
  	# locus 1
  	m.R1 <- ( f.m.RR1SS2 + f.m.RR1RS2 + f.m.RR1RR2 ) + ( 0.5 * (f.m.RS1SS2 + f.m.RS1RS2_trans + f.m.RS1RS2_cis + f.m.RS1RR2 ) ) 
  	results[k,2] <- m.R1
  	# locus 2
  	m.R2 <- ( f.m.SS1RR2 + f.m.RS1RR2 + f.m.RR1RR2 ) + ( 0.5 * (f.m.SS1RS2 + f.m.RS1RS2_cis + f.m.RS1RS2_trans + f.m.RR1RS2 ) )
  	results[k,3] <- m.R2
  
  	
  	# frequency of resistance allele in females
  	# locus 1
  	f.R1 <- ( f.f.RR1SS2 + f.f.RR1RS2 + f.f.RR1RR2 ) + ( 0.5 * (f.f.RS1SS2 + f.f.RS1RS2_cis + f.f.RS1RS2_trans + f.f.RS1RR2 ) ) 
  	results[k,5] <- f.R1
  	# locus 2
  	f.R2 <- ( f.f.SS1RR2 + f.f.RS1RR2 + f.f.RR1RR2 ) + ( 0.5 * (f.f.SS1RS2 + f.f.RS1RS2_cis + f.f.RS1RS2_trans + f.f.RR1RS2 ) )
  	results[k,6] <- f.R2
  
  
  	# total males
  	results[k,8] <- ( f.m.SS1SS2 + f.m.SS1RS2 + f.m.SS1RR2 +
  					  f.m.RS1SS2 + f.m.RS1RS2_cis  + f.m.RS1RS2_trans + f.m.RS1RR2 +
  					  f.m.RR1SS2 + f.m.RR1RS2 + f.m.RR1RR2 )
  					  
  	# total females
  	results[k,9] <- ( f.f.SS1SS2 + f.f.SS1RS2 + f.f.SS1RR2 +
  					  f.f.RS1SS2 + f.f.RS1RS2_cis + f.f.RS1RS2_trans + f.f.RS1RR2 +
  					  f.f.RR1SS2 + f.f.RR1RS2 + f.f.RR1RR2 )
  
  	# 1013 - Fig 1. in Curtis, sequential application of insecticide
  	# stops loop running if this calibration is selected to change insecticide when condition met
  	# condition to change insecticide is that frequency of the R allele is >0.5 at locus under selection
  	#if( calibration == 1013 && m.R1 > 0.4999 || calibration == 1013 && m.R2 > 0.4999 ){
  	#stop( (paste("Frequency of R allele at or over 0.5, generation = ", k)) )
  	#}				  


  	## Gametes ####
  	### Estimated here from before selection frequencies to estimate linkage disequilibrium ###
  	# Gametes produced are estimated by the frequency of the genotype and their contribution to each genotype of gamete
  	# 1 - both parts of genotype contribute, 0.5 - half of genotype contributes, 0.0 - neither part of genotype can produce this gamete
  
  	# Male Gametes
  	G.m.S1.S2 <- 0
  	G.m.R1.S2 <- 0
  	G.m.S1.R2 <- 0
  	G.m.R1.R2 <- 0
  
  	# f.m.RS1RS2_cis
  	# no recombination
  	G.m.R1.R2 <- G.m.R1.R2 + f.m.RS1RS2_cis * 0.5 * ( 1-c )
  	G.m.S1.S2 <- G.m.S1.S2 + f.m.RS1RS2_cis * 0.5 * ( 1-c )
  	# recombination takes place
  	G.m.S1.R2 <- G.m.S1.R2 + f.m.RS1RS2_cis * 0.5 * c			
  	G.m.R1.S2 <- G.m.R1.S2 + f.m.RS1RS2_cis * 0.5 * c
  
  			
  	# f.m.RS1RS2_trans
  	# no recombination
  	G.m.R1.S2 <- G.m.R1.S2 + f.m.RS1RS2_trans * 0.5 * ( 1-c )		
  	G.m.S1.R2 <- G.m.S1.R2 + f.m.RS1RS2_trans * 0.5 * ( 1-c )
  	# recombination takes place
  	G.m.R1.R2 <- G.m.R1.R2 + f.m.RS1RS2_trans * 0.5 * c
  	G.m.S1.S2 <- G.m.S1.S2 + f.m.RS1RS2_trans * 0.5 * c
  
  
  	# SS Gametes
  	G.m.S1.S2 <- G.m.S1.S2 +
  	   (f.m.SS1SS2 * 1.0 +
  		f.m.SS1RS2 * 0.5 +
  		f.m.SS1RR2 * 0.0 +
  		
  		f.m.RS1SS2 * 0.5 +
  		f.m.RS1RR2 * 0.0 +
  		
  		f.m.RR1SS2 * 0.0 +
  		f.m.RR1RS2 * 0.0 +
  		f.m.RR1RR2 * 0.0 )
  	# RS Gametes
  	G.m.R1.S2 <- G.m.R1.S2 +
  	   (f.m.SS1SS2 * 0.0 +
  		f.m.SS1RS2 * 0.0 +
  		f.m.SS1RR2 * 0.0 +
  		
  		f.m.RS1SS2 * 0.5 +
  		f.m.RS1RR2 * 0.0 +
  		
  		f.m.RR1SS2 * 1.0 +
  		f.m.RR1RS2 * 0.5 +
  		f.m.RR1RR2 * 0.0 )
  	# SR Gametes
  	G.m.S1.R2 <- G.m.S1.R2 +
  	   (f.m.SS1SS2 * 0.0 +
  		f.m.SS1RS2 * 0.5 +
  		f.m.SS1RR2 * 1.0 +
  		
  		f.m.RS1SS2 * 0.0 +
  		f.m.RS1RR2 * 0.5 +
  		
  		f.m.RR1SS2 * 0.0 +
  		f.m.RR1RS2 * 0.0 +
  		f.m.RR1RR2 * 0.0 )
  	# RR Gametes
  	G.m.R1.R2 <- G.m.R1.R2 +
  	   (f.m.SS1SS2 * 0.0 +
  		f.m.SS1RS2 * 0.0 +
  		f.m.SS1RR2 * 0.0 +
  		
  		f.m.RS1SS2 * 0.0 +
  		f.m.RS1RR2 * 0.5 +
  		
  		f.m.RR1SS2 * 0.0 +
  		f.m.RR1RS2 * 0.5 +
  		f.m.RR1RR2 * 1.0 )
  		

  	# Female Gametes ###
  	G.f.S1.S2 <- 0
  	G.f.R1.S2 <- 0
  	G.f.S1.R2 <- 0
  	G.f.R1.R2 <- 0
  
  	# f.f.RS1RS2_cis
  	#no recombination
  	G.f.R1.R2 <- G.f.R1.R2 + f.f.RS1RS2_cis * 0.5 * ( 1-c ) 
  	G.f.S1.S2 <- G.f.S1.S2 + f.f.RS1RS2_cis * 0.5 * ( 1-c )
  	# recombination takes place
  	G.f.S1.R2 <- G.f.S1.R2 + f.f.RS1RS2_cis * 0.5 * c			
  	G.f.R1.S2 <- G.f.R1.S2 + f.f.RS1RS2_cis * 0.5 * c
  
  			
  	# f.f.RS1RS2_trans			
  	# no recombination
  	G.f.R1.S2 <- G.f.R1.S2 + f.f.RS1RS2_trans * 0.5 * ( 1-c )		
  	G.f.S1.R2 <- G.f.S1.R2 + f.f.RS1RS2_trans * 0.5 * ( 1-c )
  	# recombination takes place
  	G.f.R1.R2 <- G.f.R1.R2 + f.f.RS1RS2_trans * 0.5 * c
  	G.f.S1.S2 <- G.f.S1.S2 + f.f.RS1RS2_trans * 0.5 * c
  
  
  	# SS Gametes
  	G.f.S1.S2 <- G.f.S1.S2 +
  	   (f.f.SS1SS2 * 1.0 +
  		f.f.SS1RS2 * 0.5 +
  		f.f.SS1RR2 * 0.0 +
  		
  		f.f.RS1SS2 * 0.5 +
  		f.f.RS1RR2 * 0.0 +
  		
  		f.f.RR1SS2 * 0.0 +
  		f.f.RR1RS2 * 0.0 +
  		f.f.RR1RR2 * 0.0 )
  	# RS Gametes
  	G.f.R1.S2 <- G.f.R1.S2 +
  	   (f.f.SS1SS2 * 0.0 +
  		f.f.SS1RS2 * 0.0 +
  		f.f.SS1RR2 * 0.0 +
  		
  		f.f.RS1SS2 * 0.5 +
  		f.f.RS1RR2 * 0.0 +
  		
  		f.f.RR1SS2 * 1.0 +
  		f.f.RR1RS2 * 0.5 +
  		f.f.RR1RR2 * 0.0 )
  	# SR Gametes
  	G.f.S1.R2 <- G.f.S1.R2 +
  	   (f.f.SS1SS2 * 0.0 +
  		f.f.SS1RS2 * 0.5 +
  		f.f.SS1RR2 * 1.0 +
  		
  		f.f.RS1SS2 * 0.0 +
  		f.f.RS1RR2 * 0.5 +
  		
  		f.f.RR1SS2 * 0.0 +
  		f.f.RR1RS2 * 0.0 +
  		f.f.RR1RR2 * 0.0 )
  	# RR Gametes
  	G.f.R1.R2 <- G.f.R1.R2 +
  	   (f.f.SS1SS2 * 0.0 +
  		f.f.SS1RS2 * 0.0 +
  		f.f.SS1RR2 * 0.0 +
  		
  		f.f.RS1SS2 * 0.0 +
  		f.f.RS1RR2 * 0.5 +
  		
  		f.f.RR1SS2 * 0.0 +
  		f.f.RR1RS2 * 0.5 +
  		f.f.RR1RR2 * 1.0 )
  
  
  	### Linkage Disequilibrium ####
  	## Disequibilibrium of resistant allele in gametes ##
  	# Male
  	## Frequency of allele patterns
  	x.R1.S2 <- G.m.R1.S2/2
  	x.S1.R2 <- G.m.R1.S2/2
  	x.R1.R2 <- G.m.R1.R2
  	## Frequency of alleles at each locus
  	R1 <- x.R1.S2 + x.R1.R2		# frequency of R allele at locus 1
  	R2 <- x.R1.R2 + x.S1.R2		# frequency of R allele at locus 2
  
  	m.D <- x.R1.R2 - (R1 * R2)
  	
  	# Female
  	## Frequency of allele patterns
  	x.R1.S2 <- G.f.R1.S2/2
  	x.S1.R2 <- G.f.R1.S2/2
  	x.R1.R2<- G.f.R1.R2
  	## Frequency of alleles at each locus
  	R1 <- x.R1.S2 + x.R1.R2		# frequency of R allele at locus 1
  	R2 <- x.R1.R2 + x.S1.R2		# frequency of R allele at locus 2
  
  	f.D <- x.R1.R2 - (R1 * R2)
  	
  	# print to results matrix
  	# linkage disequilibrium
  	results[k,4] <- m.D
  	# linkage disequilibrium
  	results[k,7] <- f.D
  
  	# Finding D'
  	D <- m.D		# D is given as male LD
  	
  	S1 <- 1 - R1	# frequency of S at each allele = 1 - frequency of R
  	S2 <- 1 - R2
  	
  	p1q2 <- R1 * S2	# Find P1Q2 and P2Q1 (given P = loc 1 and 1 = R allele)
  	p2q1 <- S1 * R2
  	
  	if( p1q2 < p2q1 ){		#dmax is the lower of these two
  	dmax <- p1q2
  	}else{
  	dmax <- p2q1
  	}
  	
  	negp1q1 <- -( R1 * R2 )	#Find -p1q1 and -p2q2, given conditions above
  	negp2q2 <- -( S1 * S2 )
  	
  	if( negp1q1  > negp2q2 ){	#dmin is the highest of these
  	dmin <- negp1q1
  	}else{
  	dmin <- negp2q2
  	}
  	
  	if( D>0 ){				# if D is greater than 0
  	Dprime <- D/dmax		# D' = D/dmax 
  	}else{					# if D is less than 0
  	Dprime <- D/dmin		# D' = D/dmin
  	}
  	
  	results[k,10] <- Dprime	# prints to column ten of results matrix
  	
  	## R2
  	denom <- sqrt(R1 * S1 * R2 * S2)	# finds R2 using the allale frequencies calculated above
  	r2 <- D/denom						# use this and D to find r2
  	
  	results[k,11] <- r2					# prints to column eleven of results matrix
  
    ### Frequency following selection ####
  	
  	if(calibration==103){		## no selection calibration
  	## male
  	# SS1
  		fs.m.SS1SS2 <- f.m.SS1SS2
  		fs.m.SS1RS2 <- f.m.SS1RS2
  		fs.m.SS1RR2 <- f.m.SS1RR2
  	# RS1
  		fs.m.RS1SS2 <- f.m.RS1SS2 
  		fs.m.RS1RS2_cis <- f.m.RS1RS2_cis
  		fs.m.RS1RS2_trans <- f.m.RS1RS2_trans
  		fs.m.RS1RR2 <- f.m.RS1RR2
  	# RR2 
  		fs.m.RR1SS2 <- f.m.RR1SS2
  		fs.m.RR1RS2 <- f.m.RR1RS2
  		fs.m.RR1RR2 <- f.m.RR1RR2
  
  	## female
  	# SS1
  		fs.f.SS1SS2 <- f.f.SS1SS2
  		fs.f.SS1RS2 <- f.f.SS1RS2
  		fs.f.SS1RR2 <- f.f.SS1RR2
  	# RS1
  		fs.f.RS1SS2 <- f.f.RS1SS2 
  		fs.f.RS1RS2_cis <- f.f.RS1RS2_cis
  		fs.f.RS1RS2_trans <- f.f.RS1RS2_trans
  		fs.f.RS1RR2 <- f.f.RS1RR2
  	# RR2 
  		fs.f.RR1SS2 <- f.f.RR1SS2
  		fs.f.RR1RS2 <- f.f.RR1RS2
  		fs.f.RR1RR2 <- f.f.RR1RR2
  
  		
  	#### If calibration 103 not set, selection continues as normal
  		}else{
  		## W bar - Sum of numerators
  		# W bar males
  		W.bar.m <- (f.m.SS1SS2 * W.m.SS1SS2) + (f.m.SS1RS2 * W.m.SS1RS2) + (f.m.SS1RR2 * W.m.SS1RR2) +
  				   (f.m.RS1SS2 * W.m.RS1SS2) + 
  				   (f.m.RS1RS2_cis * W.m.RS1RS2) + (f.m.RS1RS2_trans * W.m.RS1RS2) + 
  				   (f.m.RS1RR2 * W.m.RS1RR2) +
  				   (f.m.RR1SS2 * W.m.RR1SS2) + (f.m.RR1RS2 * W.m.RR1RS2) + (f.m.RR1RR2 * W.m.RR1RR2)
  		# W bar females
  		W.bar.f <- (f.f.SS1SS2 * W.f.SS1SS2) + (f.f.SS1RS2 * W.f.SS1RS2) + (f.f.SS1RR2 * W.f.SS1RR2) +
  				   (f.f.RS1SS2 * W.f.RS1SS2) + 
  				   (f.f.RS1RS2_cis * W.f.RS1RS2) + (f.f.RS1RS2_trans * W.f.RS1RS2) + 
  				   (f.f.RS1RR2 * W.f.RS1RR2) +
  				   (f.f.RR1SS2 * W.f.RR1SS2) + (f.f.RR1RS2 * W.f.RR1RS2) + (f.f.RR1RR2 * W.f.RR1RR2)
  
  				   
  			## Frequencies --- Calculated with selection
  			## Males
  			## SS1
  			fs.m.SS1SS2 <- (f.m.SS1SS2 * W.m.SS1SS2) / W.bar.m
  			fs.m.SS1RS2 <- (f.m.SS1RS2 * W.m.SS1RS2) / W.bar.m
  			fs.m.SS1RR2 <- (f.m.SS1RR2 * W.m.SS1RR2) / W.bar.m
  			## RS1
  			fs.m.RS1SS2 <- (f.m.RS1SS2 * W.m.RS1SS2) / W.bar.m
  			fs.m.RS1RS2_cis <- (f.m.RS1RS2_cis * W.m.RS1RS2) / W.bar.m
  			fs.m.RS1RS2_trans <- (f.m.RS1RS2_trans * W.m.RS1RS2) / W.bar.m
  			fs.m.RS1RR2 <- (f.m.RS1RR2 * W.m.RS1RR2) / W.bar.m
  			## RR1
  			fs.m.RR1SS2 <- (f.m.RR1SS2 * W.m.RR1SS2) / W.bar.m
  			fs.m.RR1RS2 <- (f.m.RR1RS2 * W.m.RR1RS2) / W.bar.m
  			fs.m.RR1RR2 <- (f.m.RR1RR2 * W.m.RR1RR2) / W.bar.m
  
  			## Females
  			## SS1
  			fs.f.SS1SS2 <- (f.f.SS1SS2 * W.f.SS1SS2) / W.bar.f
  			fs.f.SS1RS2 <- (f.f.SS1RS2 * W.f.SS1RS2) / W.bar.f
  			fs.f.SS1RR2 <- (f.f.SS1RR2 * W.f.SS1RR2) / W.bar.f
  			## RS1
  			fs.f.RS1SS2 <- (f.f.RS1SS2 * W.f.RS1SS2) / W.bar.f
  			fs.f.RS1RS2_cis <- (f.f.RS1RS2_cis * W.f.RS1RS2) / W.bar.f
  			fs.f.RS1RS2_trans <- (f.f.RS1RS2_trans * W.f.RS1RS2) / W.bar.f
  			fs.f.RS1RR2 <- (f.f.RS1RR2 * W.f.RS1RR2) / W.bar.f
  			## RR1
  			fs.f.RR1SS2 <- (f.f.RR1SS2 * W.f.RR1SS2) / W.bar.f
  			fs.f.RR1RS2 <- (f.f.RR1RS2 * W.f.RR1RS2) / W.bar.f
  			fs.f.RR1RR2 <- (f.f.RR1RR2 * W.f.RR1RR2) / W.bar.f
  			}
  
  	## Calibration 104, selection on one genotype
  	if( calibration == 104 ){
  
  		x.m <- select.gen.m				## Setting fitness of genotype to select on as separate variable
  		x.f <- select.gen.f				## Not lost in reprinting in next step
  		
  		## Frequencies --- Reprinting after selection fitness with before selection to eliminate selection step
  		## Males
  		## SS1
  		fs.m.SS1SS2 <- f.m.SS1SS2
  		fs.m.SS1RS2 <- f.m.SS1RS2
  		fs.m.SS1RR2 <- f.m.SS1RR2
  		## RS1
  		fs.m.RS1SS2 <- f.m.RS1SS2
  		fs.m.RS1RS2_cis <- f.m.RS1RS2_cis
  		fs.m.RS1RS2_trans <- f.m.RS1RS2_trans
  		fs.m.RS1RR2 <- f.m.RS1RR2
  		## RR1
  		fs.m.RR1SS2 <- f.m.RR1SS2
  		fs.m.RR1RS2 <- f.m.RR1RS2
  		fs.m.RR1RR2 <- f.m.RR1RR2
  		
  		## Females
  		## SS1
  		fs.f.SS1SS2 <- f.f.SS1SS2
  		fs.f.SS1RS2 <- f.f.SS1RS2
  		fs.f.SS1RR2 <- f.f.SS1RR2
  		## RS1
  		fs.f.RS1SS2 <- f.f.RS1SS2
  		fs.f.RS1RS2_cis <- f.f.RS1RS2_cis
  		fs.f.RS1RS2_trans <- f.f.RS1RS2_trans
  		fs.f.RS1RR2 <- f.f.RS1RR2
  		## RR1
  		fs.f.RR1SS2 <- f.f.RR1SS2
  		fs.f.RR1RS2 <- f.f.RR1RS2
  		fs.f.RR1RR2 <- f.f.RR1RR2
  		
  		select.gen.m <- x.m			## Reprinting fitness that is intended to be selected on
  		select.gen.f <- x.f			## with after selection fitness saved as variable above
  
  	}
  
  	## Check for errors ##
  	## Will print an error message if the genotype frequencies do not total 1.
  	# Male
  	#male.freq <- (fs.m.SS1SS2 + fs.m.SS1RS2 + fs.m.SS1RR2+
  	#	  fs.m.RS1SS2 + fs.m.RS1RS2_cis + fs.m.RS1RS2_trans + fs.m.RS1RR2+
  	#	  fs.m.RR1SS2 + fs.m.RR1RS2 + fs.m.RR1RR2) 
  	#print( (paste("Male frequencies after selection total = ",male.freq) ) )
  	# Female
  	#female.freq <- (fs.f.SS1SS2 + fs.f.SS1RS2 + fs.f.SS1RR2+
  	#	  fs.f.RS1SS2 + fs.f.RS1RS2_cis + fs.f.RS1RS2_trans + fs.f.RS1RR2+
  	#	  fs.f.RR1SS2 + fs.f.RR1RS2 + fs.f.RR1RR2) 			
  	#print( (paste("Female frequencies after selection total = ",female.freq) ) )
  
  	## Gametes ####
  	### Estimated here to allow for the next generation to be created through random mating ###
  	# Gametes produced are estimated by the frequency of the genotype and their contribution to each genotype of gamete
  	# 1 - both parts of genotype contribute, 0.5 - half of genotype contributes, 0.0 - neither part of genotype can produce this gamete
  
  	# Male Gametes
  	G.m.S1.S2 <- 0
  	G.m.R1.S2 <- 0
  	G.m.S1.R2 <- 0
  	G.m.R1.R2 <- 0
  
  	# fs.m.RS1RS2_cis
  	# no recombination
  	G.m.R1.R2 <- G.m.R1.R2 + fs.m.RS1RS2_cis * 0.5 * ( 1-c )
  	G.m.S1.S2 <- G.m.S1.S2 + fs.m.RS1RS2_cis * 0.5 * ( 1-c )
  	# recombination takes place
  	G.m.S1.R2 <- G.m.S1.R2 + fs.m.RS1RS2_cis * 0.5 * c			
  	G.m.R1.S2 <- G.m.R1.S2 + fs.m.RS1RS2_cis * 0.5 * c
  
  			
  	# fs.m.RS1RS2_trans
  	# no recombination
  	G.m.R1.S2 <- G.m.R1.S2 + fs.m.RS1RS2_trans * 0.5 * ( 1-c )		
  	G.m.S1.R2 <- G.m.S1.R2 + fs.m.RS1RS2_trans * 0.5 * ( 1-c )
  	# recombination takes place
  	G.m.R1.R2 <- G.m.R1.R2 + fs.m.RS1RS2_trans * 0.5 * c
  	G.m.S1.S2 <- G.m.S1.S2 + fs.m.RS1RS2_trans * 0.5 * c
  
  
  	# SS Gametes
  	G.m.S1.S2 <- G.m.S1.S2 +
  	   (fs.m.SS1SS2 * 1.0 +
  		fs.m.SS1RS2 * 0.5 +
  		fs.m.SS1RR2 * 0.0 +
  		
  		fs.m.RS1SS2 * 0.5 +
  		fs.m.RS1RR2 * 0.0 +
  		
  		fs.m.RR1SS2 * 0.0 +
  		fs.m.RR1RS2 * 0.0 +
  		fs.m.RR1RR2 * 0.0 )
  	# RS Gametes
  	G.m.R1.S2 <- G.m.R1.S2 +
  	   (fs.m.SS1SS2 * 0.0 +
  		fs.m.SS1RS2 * 0.0 +
  		fs.m.SS1RR2 * 0.0 +
  		
  		fs.m.RS1SS2 * 0.5 +
  		fs.m.RS1RR2 * 0.0 +
  		
  		fs.m.RR1SS2 * 1.0 +
  		fs.m.RR1RS2 * 0.5 +
  		fs.m.RR1RR2 * 0.0 )
  	# SR Gametes
  	G.m.S1.R2 <- G.m.S1.R2 +
  	   (fs.m.SS1SS2 * 0.0 +
  		fs.m.SS1RS2 * 0.5 +
  		fs.m.SS1RR2 * 1.0 +
  		
  		fs.m.RS1SS2 * 0.0 +
  		fs.m.RS1RR2 * 0.5 +
  		
  		f.m.RR1SS2 * 0.0 +
  		f.m.RR1RS2 * 0.0 +
  		f.m.RR1RR2 * 0.0 )
  	# RR Gametes
  	G.m.R1.R2 <- G.m.R1.R2 +
  	   (fs.m.SS1SS2 * 0.0 +
  		fs.m.SS1RS2 * 0.0 +
  		fs.m.SS1RR2 * 0.0 +
  		
  		fs.m.RS1SS2 * 0.0 +
  		fs.m.RS1RR2 * 0.5 +
  		
  		fs.m.RR1SS2 * 0.0 +
  		fs.m.RR1RS2 * 0.5 +
  		fs.m.RR1RR2 * 1.0 )
  		
  	# Female Gametes
  	G.f.S1.S2 <- 0
  	G.f.R1.S2 <- 0
  	G.f.S1.R2 <- 0
  	G.f.R1.R2 <- 0
  
  	# fs.f.RS1RS2_cis
  	#no recombination
  	G.f.R1.R2 <- G.f.R1.R2 + fs.f.RS1RS2_cis * 0.5 * ( 1-c ) 
  	G.f.S1.S2 <- G.f.S1.S2 + fs.f.RS1RS2_cis * 0.5 * ( 1-c )
  	# recombination takes place
  	G.f.S1.R2 <- G.f.S1.R2 + fs.f.RS1RS2_cis * 0.5 * c			
  	G.f.R1.S2 <- G.f.R1.S2 + fs.f.RS1RS2_cis * 0.5 * c
  
  			
  	# fs.f.RS1RS2_trans			
  	# no recombination
  	G.f.R1.S2 <- G.f.R1.S2 + fs.f.RS1RS2_trans * 0.5 * ( 1-c )		
  	G.f.S1.R2 <- G.f.S1.R2 + fs.f.RS1RS2_trans * 0.5 * ( 1-c )
  	# recombination takes place
  	G.f.R1.R2 <- G.f.R1.R2 + fs.f.RS1RS2_trans * 0.5 * c
  	G.f.S1.S2 <- G.f.S1.S2 + fs.f.RS1RS2_trans * 0.5 * c
  
  
  	# SS Gametes
  	G.f.S1.S2 <- G.f.S1.S2 +
  	   (fs.f.SS1SS2 * 1.0 +
  		fs.f.SS1RS2 * 0.5 +
  		fs.f.SS1RR2 * 0.0 +
  		
  		fs.f.RS1SS2 * 0.5 +
  		fs.f.RS1RR2 * 0.0 +
  		
  		fs.f.RR1SS2 * 0.0 +
  		fs.f.RR1RS2 * 0.0 +
  		fs.f.RR1RR2 * 0.0 )
  	# RS Gametes
  	G.f.R1.S2 <- G.f.R1.S2 +
  	   (fs.f.SS1SS2 * 0.0 +
  		fs.f.SS1RS2 * 0.0 +
  		fs.f.SS1RR2 * 0.0 +
  		
  		fs.f.RS1SS2 * 0.5 +
  		fs.f.RS1RR2 * 0.0 +
  		
  		fs.f.RR1SS2 * 1.0 +
  		fs.f.RR1RS2 * 0.5 +
  		fs.f.RR1RR2 * 0.0 )
  	# SR Gametes
  	G.f.S1.R2 <- G.f.S1.R2 +
  	   (fs.f.SS1SS2 * 0.0 +
  		fs.f.SS1RS2 * 0.5 +
  		fs.f.SS1RR2 * 1.0 +
  		
  		fs.f.RS1SS2 * 0.0 +
  		fs.f.RS1RR2 * 0.5 +
  		
  		f.f.RR1SS2 * 0.0 +
  		f.f.RR1RS2 * 0.0 +
  		f.f.RR1RR2 * 0.0 )
  	# RR Gametes
  	G.f.R1.R2 <- G.f.R1.R2 +
  	   (fs.f.SS1SS2 * 0.0 +
  		fs.f.SS1RS2 * 0.0 +
  		fs.f.SS1RR2 * 0.0 +
  		
  		fs.f.RS1SS2 * 0.0 +
  		fs.f.RS1RR2 * 0.5 +
  		
  		fs.f.RR1SS2 * 0.0 +
  		fs.f.RR1RS2 * 0.5 +
  		fs.f.RR1RR2 * 1.0 )
  		
  	# Check male gamete total		
  	#G.m.t <- G.m.S1.S2 + G.m.R1.S2 + G.m.S1.R2 + G.m.R1.R2		
  	#if ( (G.m.t)!=1 ){			
  	#	print( (paste("Error in male gametes, gametes = ",G.m.t) ) )
  		
  	#	}else{
  	#		print( (paste("Male gametes correct, gametes = ",G.m.t) ) )
  	#		}
  			
  	#G.f.t <- G.f.S1.S2 + G.f.R1.S2 + G.f.S1.R2 + G.f.R1.R2		
  	#if ( (G.f.t)!=1 ){			
  	#	print( (paste("Error in female gametes, gametes = ",G.f.t) ) )
  	#	
  	#	}else{
  	#		print( (paste("Female gametes correct, gametes = ",G.f.t) ) )
  	#		}

    
  	### To run next loop ####
  	## Random Mating ##
  	# set blank variables for frequencies of each genotype
  	# Only calculated once and then treated as the same for males and females (when frequencies generated at start of loop)
  	f.m.SS1SS2 <- 0
  	f.m.SS1RS2 <- 0
  	f.m.SS1RR2 <- 0
  
  	f.m.RS1SS2 <- 0
  	f.m.RS1RS2_cis <- 0			#RS1RS2
  	f.m.RS1RS2_trans <- 0		#RS1SR2
  	f.m.RS1RR2 <- 0
  
  	f.m.RR1SS2 <- 0
  	f.m.RR1RS2 <- 0 
  	f.m.RR1RR2 <- 0
  	# SS male with SS female
  	f.m.SS1SS2 <- f.m.SS1SS2 + ( G.m.S1.S2 * G.f.S1.S2 )
  	# SS male with SR female
  	f.m.SS1RS2 <- f.m.SS1RS2 + ( G.m.S1.S2 * G.f.S1.R2 )
  	# SS male with RS female
  	f.m.RS1SS2 <- f.m.RS1SS2 + ( G.m.S1.S2 * G.f.R1.S2 )
  	# SS male with RR female
  	f.m.RS1RS2_cis <- f.m.RS1RS2_cis + ( G.m.S1.S2 * G.f.R1.R2 )
  
  	# SR male with SS female
  	f.m.SS1RS2 <- f.m.SS1RS2 + ( G.m.S1.R2 * G.f.S1.S2 )
  	# SR male with SR female
  	f.m.SS1RR2 <- f.m.SS1RR2 + ( G.m.S1.R2 * G.f.S1.R2 )
  	# SR male with RS female
  	f.m.RS1RS2_trans <- f.m.RS1RS2_trans + ( G.m.S1.R2 * G.f.R1.S2 )
  	# SR male with RR female
  	f.m.RS1RR2 <- f.m.RS1RR2 + ( G.m.S1.R2 * G.f.R1.R2 )
  
  	# RS male with SS female
  	f.m.RS1SS2 <- f.m.RS1SS2 + ( G.m.R1.S2 * G.f.S1.S2 )
  	# RS male with SR female
  	f.m.RS1RS2_trans <- f.m.RS1RS2_trans + ( G.m.R1.S2 * G.f.S1.R2 )
  	# RS male with RS female
  	f.m.RR1SS2 <- f.m.RR1SS2 + ( G.m.R1.S2 * G.f.R1.S2 )
  	# RS male with RR female
  	f.m.RR1RS2 <- f.m.RR1RS2 + ( G.m.R1.S2 * G.f.R1.R2 )
  
  	# RR male with SS female
  	f.m.RS1RS2_cis <- f.m.RS1RS2_cis + ( G.m.R1.R2 * G.f.S1.S2 ) 
  	# RR male with SR female
  	f.m.RS1RR2 <- f.m.RS1RR2 + ( G.m.R1.R2 * G.f.S1.R2 )
  	# RR male with RS female
  	f.m.RR1RS2 <- f.m.RR1RS2 + ( G.m.R1.R2 * G.f.R1.S2 )
  	# RR male with RR female
  	f.m.RR1RR2 <- f.m.RR1RR2 + ( G.m.R1.R2 * G.f.R1.R2 )
  
  	# check of total genotype frequencies
  	#gen.total <- ( f.m.SS1SS2 + f.m.SS1RS2 + f.m.SS1RR2 +
  	#			   f.m.RS1SS2 + f.m.RS1RS2_cis + f.m.RS1RS2_trans + f.m.RS1RR2 +
  	#			   f.m.RR1SS2 + f.m.RR1RS2 + f.m.RR1RR2 )
  
  	#print( paste( "Genotype totals after mating = ",gen.total ) )
  
  
  	## Puts frequencies back into genotype frequency matrix to restart the loop
  	if( calibration == 102 ){
  		genotype.freq <- genotype.freq 
  		}else{
  	## reprints genotype.freq with new frequencies from gametes
  		genotype.freq[1,] <- f.m.SS1SS2
  		genotype.freq[2,] <- f.m.SS1RS2
  		genotype.freq[3,] <- f.m.SS1RR2
  
  		genotype.freq[4,] <- f.m.RS1SS2
  		genotype.freq[5,] <- f.m.RS1RS2_cis
  		genotype.freq[6,] <- f.m.RS1RS2_trans
  		genotype.freq[7,] <- f.m.RS1RR2
  
  		genotype.freq[8,] <- f.m.RR1SS2
  		genotype.freq[9,] <- f.m.RR1RS2
  		genotype.freq[10,] <- f.m.RR1RR2
  		}
  			
  	}	# loop running model

  
  ## Assign results matrices to lists for multiple runs
  results.list[[i]] <- results
  genotype.list[[i]] <- genotype
  
  
  ## Plots ####
  if( produce.plots == TRUE ){
  # Plot of R and S allele frequencies over generations
  # Prints male frequency of R allele at locus 1 (blue) and locus 2 (green)
  # and same in female at locus 1 (red) and locus 2 (orange)
  genplot <- plotallele.freq( results.list[[i]] )
  # Saves plot into same directory as code documents
  dev.copy(png, (paste(i,'freq-Rallele-bygender.png')))		## WARNING: this will overwrite every time, move or rename files! ##
  dev.off()
  
  # Plot of RR, RS and SS at each locus over generations
  # locus 1: SS in pink, RS in orange, RR in red
  # locus 2: SS in cyan, RS in dark blue, RR in green
  genplot <- plothaplotype( genotype.list[[i]] )
  # Saves plot into same directory as code documents
  dev.copy(png,(paste(i,'haplotype-frequencies.png')))		## WARNING: this will overwrite every time, move or rename files! ##
  dev.off()
  
  # Plot of LD over time
  genplot <- plotlinkage( results.list[[i]] )
  # Saves plot into same directory as code documents
  dev.copy(png,(paste(i,'LD.png')))		## WARNING: this will overwrite every time, move or rename files! ##
  dev.off()
  }
  
  #### Prints fitnesses caluclated by niche by genotype to matrix ####
  ## To save in .csv, enter save.param as TRUE above ##
  if( coll.fitvals == 1 ){
  fbn <- matrix( ncol=9, nrow=9 )
  colnames(fbn) <- c("-,-", "a,-", "A,-", "-,b", "-,B", "a,b", "A,B", "A,b", "a,B")
  rownames(fbn) <- c("SS1SS2", "SS1RS2", "SS1RR2",
  					   "RS1SS2", "RS1RS2", "RS1RR2",
  					   "RR1SS2", "RR1RS2", "RR1RR2" )
  	# SS1			 
  	fbn[1,1] <- W.SS1SS2_00
  	fbn[1,2] <- W.SS1SS2_a0
  	fbn[1,3] <- W.SS1SS2_A0
  	fbn[1,4] <- W.SS1SS2_0b
  	fbn[1,5] <- W.SS1SS2_0B
  	fbn[1,6] <- W.SS1SS2_ab
  	fbn[1,7] <- W.SS1SS2_AB
  	fbn[1,8] <- W.SS1SS2_Ab
  	fbn[1,9] <- W.SS1SS2_aB
  
  	fbn[2,1] <- W.SS1RS2_00
  	fbn[2,2] <- W.SS1RS2_a0
  	fbn[2,3] <- W.SS1RS2_A0
  	fbn[2,4] <- W.SS1RS2_0b
  	fbn[2,5] <- W.SS1RS2_0B
  	fbn[2,6] <- W.SS1RS2_ab
  	fbn[2,7] <- W.SS1RS2_AB
  	fbn[2,8] <- W.SS1RS2_Ab
  	fbn[2,9] <- W.SS1RS2_aB
  							  
  	fbn[3,1] <- W.SS1RR2_00
  	fbn[3,2] <- W.SS1RR2_a0
  	fbn[3,3] <- W.SS1RR2_A0
  	fbn[3,4] <- W.SS1RR2_0b
  	fbn[3,5] <- W.SS1RR2_0B
  	fbn[3,6] <- W.SS1RR2_ab
  	fbn[3,7] <- W.SS1RR2_AB
  	fbn[3,8] <- W.SS1RR2_Ab
  	fbn[3,9] <- W.SS1RR2_aB						  

  	# RS1
  	fbn[4,1] <- W.RS1SS2_00
  	fbn[4,2] <- W.RS1SS2_a0
  	fbn[4,3] <- W.RS1SS2_A0
  	fbn[4,4] <- W.RS1SS2_0b
  	fbn[4,5] <- W.RS1SS2_0B
  	fbn[4,6] <- W.RS1SS2_ab
  	fbn[4,7] <- W.RS1SS2_AB
  	fbn[4,8] <- W.RS1SS2_Ab
  	fbn[4,9] <- W.RS1SS2_aB
  
  	fbn[5,1] <- W.RS1RS2_00
  	fbn[5,2] <- W.RS1RS2_a0
  	fbn[5,3] <- W.RS1RS2_A0
  	fbn[5,4] <- W.RS1RS2_0b
  	fbn[5,5] <- W.RS1RS2_0B
  	fbn[5,6] <- W.RS1RS2_ab
  	fbn[5,7] <- W.RS1RS2_AB
  	fbn[5,8] <- W.RS1RS2_Ab
  	fbn[5,9] <- W.RS1RS2_aB
  							  
  	fbn[6,1] <- W.RS1RR2_00
  	fbn[6,2] <- W.RS1RR2_a0
  	fbn[6,3] <- W.RS1RR2_A0
  	fbn[6,4] <- W.RS1RR2_0b
  	fbn[6,5] <- W.RS1RR2_0B
  	fbn[6,6] <- W.RS1RR2_ab
  	fbn[6,7] <- W.RS1RR2_AB
  	fbn[6,8] <- W.RS1RR2_Ab
  	fbn[6,9] <- W.RS1RR2_aB		

  	fbn[7,1] <- W.RR1SS2_00
  	fbn[7,2] <- W.RR1SS2_a0
  	fbn[7,3] <- W.RR1SS2_A0
  	fbn[7,4] <- W.RR1SS2_0b
  	fbn[7,5] <- W.RR1SS2_0B
  	fbn[7,6] <- W.RR1SS2_ab
  	fbn[7,7] <- W.RR1SS2_AB
  	fbn[7,8] <- W.RR1SS2_Ab
  	fbn[7,9] <- W.RR1SS2_aB
  
  	fbn[8,1] <- W.RR1RS2_00
  	fbn[8,2] <- W.RR1RS2_a0
  	fbn[8,3] <- W.RR1RS2_A0
  	fbn[8,4] <- W.RR1RS2_0b
  	fbn[8,5] <- W.RR1RS2_0B
  	fbn[8,6] <- W.RR1RS2_ab
  	fbn[8,7] <- W.RR1RS2_AB
  	fbn[8,8] <- W.RR1RS2_Ab
  	fbn[8,9] <- W.RR1RS2_aB
  							  
  	fbn[9,1] <- W.RR1RR2_00
  	fbn[9,2] <- W.RR1RR2_a0
  	fbn[9,3] <- W.RR1RR2_A0
  	fbn[9,4] <- W.RR1RR2_0b
  	fbn[9,5] <- W.RR1RR2_0B
  	fbn[9,6] <- W.RR1RR2_ab
  	fbn[9,7] <- W.RR1RR2_AB
  	fbn[9,8] <- W.RR1RR2_Ab
  	fbn[9,9] <- W.RR1RR2_aB						  

  	fitness.list[[i]] <- fbn
  	
  	}					  
  	if( save.fitvals==1 ){
  	write.csv ( fbn, (paste(i,"two-locus_fitness-scores.csv")), row.names=T)
  	}
  	
  	
}		### loop through columns of parameter input table - produces results lists ###
	

### Actions needing the full results.list ####

### Conditional plot commands for calibrations to Curtis paper ####

### Curtis Figures require multiple results matrices, so are called outside of looping through the input matrix
## Linkage Disequilibrium - top half of figure one
if( produce.plots == TRUE ){
if( calibration == 1011 ){
plot <- curtis_ld( results.list[[1]], results.list[[2]], 1, 4 )
dev.copy(png,('LD_curtis-fig1.png'))		## WARNING: this will overwrite every time, move or rename files! ##
dev.off()
}

# Bottom half of figure one
# Plot of total frequency of R allele over time
# as fig.2 of Curtis (1985)
if( calibration == 1011 ){
genplot <- curtis_f1( results.list[[1]], results.list[[2]], 1, 2 )
dev.copy(png,('curtis-fig1.png'))		## WARNING: this will overwrite every time, move or rename files! ##
dev.off()
}

## Fig 2 - plots sequential and combination, as in Curtis fig 2
if( calibration == 1012 ){
	curtis <- plotcurtis_f2( results.list[[3]], results.list[[1]], results.list[[2]], 1, 2, 3 )
	dev.copy(png,('curtis-fig2.png'))		## WARNING: this will overwrite every time, move or rename files! ##
	dev.off()
	}
	}
	

### Finding generations taken to reach a frequency of R of 0.5 at each locus ####
loc1_0.5 <- timetoFifty( 1, max_gen, results.list, input )
loc2_0.5 <- timetoFifty( 2, max_gen, results.list, input )