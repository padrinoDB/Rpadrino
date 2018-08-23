### Infile growth, survival, and reproduction data ###

if(Sys.getenv("USERNAME") == 'sl13sise'){
  oldwd <- getwd()
  setwd('C:/Users/sl13sise/Dropbox/Thesis_SL/Invader_Demography')
}

Ailanthus4R = read.csv("Ailanthus/IPM/Data/Ailanthus4R.csv", header = TRUE)
head(Ailanthus4R)

### Infile germination data ###
Ailanthus4R_Germ = read.csv("Ailanthus/IPM/Data/Ailanthus4R_Germ.csv")
head(Ailanthus4R_Germ)

### Subset data ###
# PopData is file for growth and survival analyses
# Good_PopData is used to model size distribution of only the good clones
# PopDataSDL is used to model size distribution of seedlings
# PopDataRA is used to model the relationship between size and probability of reproduction
# PopDataSeeds is used to model relationship between fecundity and size; it only includes reproducing individuals

###########################################################################
# Control without fire
PopData1 = Ailanthus4R[Ailanthus4R$Trt == 'Control' | Ailanthus4R$Trt == 'All',]
PopData = PopData1[PopData1$Burn != 'Y',]

PopDataSDL = Ailanthus4R[Ailanthus4R$Trt == 'Control' & Ailanthus4R$Burn != 'Y' & Ailanthus4R$Stage2013 == 'SDL',]

##########################################################################
# These subsets are used for all treatments
Good_PopData = PopData[PopData$Quality2013 =='good',]
Nonclones = Good_PopData[Good_PopData$Stage2012 !='gone' & Good_PopData$Stage2013 != 'SDL' & Good_PopData$Stage2012 != 'SDL',]
Clones1 = subset(PopData, Stage2013 != 'gone')
Clones2 = subset(Clones1, Stage2013 != 'SDL' & Stage2012 != 'SDL')
Clones = subset(Clones2, Stage2012 == 'gone' & Quality2013 == 'good')

PopDataRA1 = PopData[PopData$Stage2013 != 'RA',]
PopDataRA = PopDataRA1[PopDataRA1$Stage2013 != 'Boy',]
PopDataSeeds = Ailanthus4R[Ailanthus4R$Stage2013 == 'RA' | Ailanthus4R$Stage2013 == 'girl',]

Germ = subset(Ailanthus4R_Germ, Study == 'Germ')
Viable = subset(Ailanthus4R_Germ, Study == 'Viable')
FireY = subset(Ailanthus4R_Germ, Study == 'Fire' & Location == '0' & Burn == 'Yes')
FireN = subset(Ailanthus4R_Germ, Study == 'Fire' & Location == '0' & Burn == 'No')

p.vec=data.frame(
  surv.int=NA,           # Intercept from logistic regression of survival
  surv.slope=NA,         # Slope from logistic regression of survival
  growth.int=NA,         # Intercept from linear regression of growth
  growth.slope=NA,       # Slope from linear regression of growth
  growth.sd=NA,          # Residual sd from linear regression of growth
  seed.int=NA,           # Intercept from Poisson regression of seed number and size
  seed.slope=NA,         # Slope from Poisson regression of seed number and size
  prob.repro.int=NA,     # Intercept from logistic regression of reproduction
  prob.repro.slope=NA,   # Slope from logistic regression of reproduction
  recruit.size.mean=NA,  # Mean recruit size
  recruit.size.sd=NA,    # Standard deviation of recruit size
  establishment.prob=NA, # Probability of establishment
  clonal.size.mean=NA,   # Mean clone size
  clonal.size.sd=NA,     # Standard deviation of clone size
  clonal.prob=NA,        # Probability of producting a clone
  stay.seedbank.prob=NA, # Probability of staying in the seedbank
  go.seedbank.prob=NA,   # Probablity of entering the seedbank
  germ.seedbank.prob=NA  # Probablity of germinating from the seedbank
)

# 1. Survival: logistic regression
surv.reg=glm(Survival~Size,data=PopData,family=binomial())
summary(surv.reg)

p.vec$surv.int=coefficients(surv.reg)[1]
p.vec$surv.slope=coefficients(surv.reg)[2]

# 2. Growth: linear regression with x-intercept set to zero
growth.reg=lm(SizeNext~0+Size,data=PopData)
summary(growth.reg)
p.vec$growth.int=0  # If intercept is above one naturally, change to: coefficients(growth.reg)[1]
p.vec$growth.slope=coefficients(growth.reg)[1]  # And change this to: coefficients(growth.reg)[2]
p.vec$growth.sd=sd(resid(growth.reg))

# 3. Seeds: Divided into two parameters
# Logistic regression for probability of being reproductive
prob.repro.reg=glm(Fecundity~SizeNext,data=PopDataRA,family=binomial())
summary(prob.repro.reg)
p.vec$prob.repro.int=coefficients(prob.repro.reg)[1]
p.vec$prob.repro.slope=coefficients(prob.repro.reg)[2]

#  Poisson regression for relationship between SizeNext and number of seeds
seed.reg=glm(Seeds~SizeNext,data=PopDataSeeds,family=poisson())
summary(seed.reg)
p.vec$seed.int=coefficients(seed.reg)[1]
p.vec$seed.slope=coefficients(seed.reg)[2]

# Mean and standard deviation of seedling size for use in a normal distribution.
# Assume that offspring size is independent of maternal size,
# so we only need to describe the distribution of offspring sizes in terms of its mean and variance.

# 4. Size distribution of seedlings
p.vec$recruit.size.mean=mean(PopDataSDL$SizeNext)
p.vec$recruit.size.sd=sd(PopDataSDL$SizeNext)

# 5. Staying in continuous stage (i.e., does not enter seedbank)
k=1

v=mean(Germ$v)

g=mean(Germ$g)

### Infile seedling count data ###
AASeedlings = read.csv("Ailanthus/IPM/Data/Copy of AA SDL Counts by plot.csv", header = TRUE)

# Subset data
SDL_NoBurn = AASeedlings[AASeedlings$Burn == 'N',]

# Plot data
SDL_no.reg=lm(SDL_no~Year,data=SDL_NoBurn)
summary(SDL_no.reg)
xx=seq(2012,2013,by=1)
#plot(SDL_NoBurn$Year,SDL_NoBurn$SDL_no,xlab="Year",ylab="Seedling density")
#lines(xx,predict(SDL_no.reg,data.frame(Year=xx)),col='red',lwd=3)

# For unburned plots
e = 0.160585

p.vec$establishment.prob=v*g*e

# 6.  Size distribution of clones
p.vec$clonal.size.mean=mean(Clones$SizeNext, na.rm = TRUE)
p.vec$clonal.size.sd=sd(Clones$SizeNext, na.rm = TRUE)

# 7. Probability of clonal growth
# Estimated by dividing the number of new recruits (i.e., Clones) in 2013
# by the number of NRA and RA (i.e., Nonclones) in 2012
p.vec$clonal.prob=length(Clones$SizeNext)/length(Nonclones$Size)
ContN_clonal.prob=length(Clones$SizeNext)/length(Nonclones$Size)

# 8.  Probabilities associated with discrete stage (i.e., seedbank)
# Probability of staying in the seedbank
p.vec$stay.seedbank.prob=v*(1-g)*k

# Probablity of entering the seedbank from continuous stage (more below)
p.vec$go.seedbank.prob=v*(1-g)*k

# Probablity of germinating from the seedbank (more below)
p.vec$germ.seedbank.prob=v*g*e

### Define functions to describe life history ###

# 1. Survival probability function
s.x.CONTN=function(x,params) {
  u = exp(params$surv.int + params$surv.slope * x)
  return(u / (1 + u))
}

# 2. Growth function
g.yx.CONTN=function(xp,x,params) {
  dnorm(xp,
        mean = params$growth.int + params$growth.slope * x,
        sd = params$growth.sd)
}

# 3. Reproduction function
p.repro.x.CONTN=function(x,params) {
  u = exp(params$prob.repro.int + params$prob.repro.slope * x)
  return(u / (1 + u))
}

# Fecundity model while taking into account some seeds move into
# the discrete portion of the model (i.e., seedbank)
f.yx.CONTN=function(xp,x,params) {
  p.repro.x.CONTN(x,params) *
    params$establishment.prob *
    dnorm(xp, mean = params$recruit.size.mean, sd = params$recruit.size.sd) *
    exp(params$seed.int + params$seed.slope * x)
}

# 4. Seedling size distribution
d.x.CONTN=function(xp,x,params) {
  dnorm(xp, mean = p.vec$recruit.size.mean, sd = p.vec$recruit.size.sd) * p.vec$germ.seedbank.prob
}

# 5. Seeds entering seedbank
e.x.CONTN=function(xp,x,params) {
  p.repro.x.CONTN(x,params) *
    exp(params$seed.int + params$seed.slope * x) *
    params$go.seedbank.prob
}

# 6. Clonal function
c.yx.CONTN=function(xp,x,params) {
    params$clonal.prob *
    dnorm(xp, mean = params$clonal.size.mean, sd = params$clonal.size.sd)
}

### Combine vital rate functions to build the discretized IPM kernal (i.e., IPM matrix)
# Set number of classes (n.size), or points for midpoint rule approximation
# Define the boundary points (b; the edges of the cells defining the matrix),
# mesh points (y; the centers of the cells defining the matrix and the points at which
# the matrix is evaluated for the midpoint rule of numerical integration, and
# step size (h; the widths of the cells)
# The integration limits (min.size and max.size_ContN) span the range of sizes observed in the data set.

min.size=0.9*min(c(PopData$Size,PopData$SizeNext),na.rm=T)  # Use values slightly above and below limits
max.size_ContN=1.1*max(c(PopData$Size,PopData$SizeNext),na.rm=T)
n.size=50 # number of cells in the matrix
b=min.size+c(0:n.size)*(max.size_ContN-min.size)/n.size # boundary points
y=0.5*(b[1:n.size]+b[2:(n.size+1)]) # mesh points
h=y[2]-y[1] # step size

### Make IPM matrices ###
# The function outer() evaluates the matrix at all pairwise combinations of the two
# vectors y and y and returns matrices representing the kernel components for growth
# and fecundity, respectively. For the numerical integration, we’re using the midpoint
# rule estimate the area under a curve. The midpoint rule assumes a rectangular
# approximation. The heights of the rectangles are given by the outer function and
# the width of the rectangles is h.
# The result is n.size x n.size cell discretization of the kernel, K.


# Growth matrix
G_ContN = h * outer(y, y, g.yx.CONTN, params = p.vec)

# Larger individuals are evicted (see Williams et al. 2012), so return the evicted individuals to the
# cells at the boundaries where they were evicted (i.e., rerout growth to sizes outside the allowed range
# to the extreme sizes avoiding eviction).
for(i in 1:(n.size/2)){
  G_ContN[1, i]=G_ContN[1, i] + 1 - sum(G_ContN[ , i])
}

# Survival vector
S_ContN = s.x.CONTN(y,params=p.vec)


# Fecundity martix
F_ContN = array(0, dim = c(n.size, n.size))
F_ContN[1:n.size, 1:n.size] = h*outer(y,y,f.yx.CONTN,params=p.vec)

# Survival/growth matrix
P_ContN <- array(0, dim=c(n.size, n.size))

# Build growth/survival matrix including discrete seedbank stage
for(i in 1:n.size){
  P_ContN[i, 1:n.size] = S_ContN * G_ContN[i, ]
}

C_ContN <- array(0, dim = c(n.size, n.size))
C_ContN[1:n.size , 1:n.size] <- h*outer(y, y, c.yx.CONTN, params = p.vec)

# Build complete matrix
K_ContN = P_ContN + C_ContN + F_ContN

### Calculate eigenvalues and eigenvectors of the matrix ###
# Right eigenvector gives the stable stage distribution and
# left eigen vetor gives the reproductive value, when normalized.

ContN_lambda <- target_2 <- Re(eigen(K_ContN)$values[1])


setwd(oldwd)
