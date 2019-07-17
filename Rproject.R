# Final R Term Project
# Written by Rithvik Rao, Matthew Shabet, and Elijah Tai
# Math 23c: Mathematics for Computation and Data Science
# 15 May 2019
# Analysis of the 2017 NFL play-by-play data

# Import statements
# -----------------------------------------------------------------------------------------------------

# Our dataset
dataset <- read.csv("NFLpbp2017.csv")

# For logistic regression
# We need to maximize the function of alpha and beta
#install.packages("stats4")
library(stats4)

# This will help us parse dates
#install.packages("lubridate")
library(lubridate)

# This will help us make cool plots
# install.packages("ggplot2")
library(ggplot2)

# -----------------------------------------------------------------------------------------------------
# Barplots
# -----------------------------------------------------------------------------------------------------

# Define a function for creating bar graphs
BarPlot <- function(table, title, ctitles, xlabel, ylabel){
  nums <- as.vector(table)
  df <- data.frame(ctitles, nums)
  bp <- ggplot(df, aes(x=ctitles, y=nums))
  bp <- bp + geom_bar(stat = "identity")
  bp <- bp + ggtitle(title)
  bp <- bp + xlab(xlabel) + ylab(ylabel)
  bp
}

# The frequency of each down
index <- which(dataset$Down != 0); head(index)
downs <- dataset$Down[index]; head(downs)
tbl <- table(downs); tbl
columntitles <- c("Down 1", "Down 2", "Down 3", "Down 4")
BarPlot(tbl, "Frequency of each down", columntitles, "Downs", "Frequency")

# Analysis: Here it is clear that 1st downs are most common, followed by 2nd downs, 3rd downs, and 
# then 4th downs in that order. This makes sense: every drive must include a 1st down, but the later
# downs are less and less likely (to think about this, consider that an ideal team would never even get
# to second down, because they would always reach the first down checkpoint).


# The frequency of each timeout
T1 <- length(which(grepl("TIMEOUT #1",as.character(dataset$Description)))); T1
T2 <- length(which(grepl("TIMEOUT #2",as.character(dataset$Description)))); T2
T3 <- length(which(grepl("TIMEOUT #3",as.character(dataset$Description)))); T3
timeouts <- c(T1, T2, T3); timeouts
columntitles <- c("Timeout #1", "Timeout #2", "Timeout #3")
tbl <- as.table(setNames(timeouts, columntitles)); tbl
BarPlot(tbl, "Timeout Frequencies", columntitles, "Timeout Number", "Frequency")

# Analysis: Unsurprisingly, teams use timeout #1 more often than timeout #2, and timeout #3 more often
# than timeout #2. This is because there is no requirement to use timeouts, and can sometimes even be
# a disadvantage - consider that a team winning by a lot may not use their later touchdowns because
# this could offer an opportunity for the opponent to rethink their strategy.


# The frequency of penalties for each team
index <- which(dataset$PenaltyTeam != ""); head(index)
penaltyteam <- dataset$PenaltyTeam[index]; head(penaltyteam)
tbl <- table(penaltyteam); tbl <- tbl[-1]; tbl
BarPlot(tbl, "Penalties By Team", names(tbl), "Team", "Frequency") + theme(text = element_text(size=9), axis.text.x = element_text(angle=-90, hjust=1))

# Analysis: Generally, the number of penalties is constant for each team. The Seattle Seahawks stand
# out for a particularly large number of penalties given!


# -----------------------------------------------------------------------------------------------------
# Pie Charts
# -----------------------------------------------------------------------------------------------------

# First define a function for creating pie charts
PieChart <- function(table, title, ctitles, xlabel, ylabel) {
  nums <- as.vector(table)
  df <- data.frame(ctitles, nums)
  pie <- ggplot(df, aes(x="", y=nums, fill=ctitles))
  pie <- pie + geom_bar(width = 1, stat = "identity")
  pie <- pie + coord_polar("y", start=0)
  pie <- pie + ggtitle(title)
  pie <- pie + xlab(xlabel) + ylab(ylabel)
  pie <- pie + theme(text = element_text(size=7))
  pie <- pie + theme(legend.title = element_blank())
  pie
}

# The frequency of each formation
index <- which(dataset$Formation != ""); head(index)
formation <- dataset$Formation[index]; head(formation)
tbl <- table(formation); tbl <- tbl[-1]; tbl
PieChart(tbl, "Formation Frequencies", names(tbl), "Formation", "Frequency")

# Analysis: Under center and shotgun are the two most popular formations. The data set does not really
# have thorough data for formations, which is worth noting. This is evident when considering that no
# huddle and shotgun, which are presented as disjoint events in the data set (as presented in this pie
# chart), can actually both occur on the same play.


# The frequency of each play type
index <- which(dataset$PlayType != ""); head(index)
playtype <- dataset$PlayType[index]; head(playtype)
tbl <- table(playtype); tbl <- tbl[-1]; tbl
# Remove Values Less than 1000, but in Other
other <- 2289
tbl <- tbl[-1]; tbl <- tbl[-1]; tbl <- tbl[-3]; tbl <- tbl[-4]; tbl <- tbl[-5]; tbl <- tbl[-6]; tbl <- tbl[-8]; tbl <- tbl[-9];
names <- append(names(tbl), "OTHER")
nums <- append(as.vector(tbl), other)
tbl <- as.table(setNames(nums, names)); tbl
PieChart(tbl, "Play Type Frequencies", names, "Play Type", "Frequency")

# Analysis: Unsurprisingly, the two most popular play types are passes and rushes. The rest of the
# plays can generally be attributed to special teams circumstances, and the data set also counts things
# like timeouts as plays. 


# The frequency of each pass type
index <- which(dataset$PassType != ""); head(index)
passtype <- dataset$PassType[index]; head(passtype)
tbl <- table(passtype); tbl<- tbl[-1]; tbl
# Remove the values that are less than 5
tbl <- tbl[-1]; tbl <- tbl[-4]; tbl <- tbl[-4]; tbl <- tbl[-4]
PieChart(tbl, "Pass Type Frequencies", names(tbl), "Pass Type", "Frequency")

# Analysis: Each pass type is employed relatively often throughout the league, with the three most
# popular pass types (in increasing order of popularity) being short middle, then short left, and then
# short right. This result likely speaks to quarterback preference across the NFL. 


# The frequency of each rush direction
index <- which(dataset$RushDirection != ""); head(index)
rushdirection <- dataset$RushDirection[index]; head(rushdirection)
tbl <- table(rushdirection); tbl<- tbl[-1]; tbl
PieChart(tbl, "Rush Directions", names(tbl), "Direction", "Frequency")

# Analysis: The rush directions are pretty evenly distributed, with a particularly large proportion
# of rushes being center. It seems that the default rush may be center.


# The number of occurences of each penalty
index <- which(dataset$IsPenalty == 1);
penalties <- dataset$PenaltyType[index];
tbl <- table(penalties); tbl <- tbl[-1];
# Remove Values Less than 100, put in Other
initial <- sum(as.vector(tbl)); initial
rem <- c(-1, -2, -3, -4, -5, -10, -11, -12, -13, -14)
rem <- c(rem, -16, -17, -18, -20, -21, -22, -23, -24, -25, -26)
rem <- c(rem, -27, -28, -29, -31, -32, -33, -34, -35, -36, -37)
rem <- c(rem, -38, -40, -43, -44, -45, -47, -48, -49, -50, -52)
tbl <- tbl[rem]
nums <- as.vector(tbl)
other <- initial - sum(nums); other
nums <- c(nums, other); nums
names <- c("D. Hold.", "D. Offside", "D. Pass Intf.", "Delay", "False Strt.")
names <- c(names, "Ill. Block", "Ill. Hands", "Neutral Zone", "Off. Hold.", "Off. Pass Interf.")
names <- c(names, "Roughing Passer", "Roughness", "Other")
tbl <- as.table(setNames(nums, names)); tbl
PieChart(tbl, "Penalty Counts", names(tbl), "Penalty Type", "Frequency") + theme(text = element_text(size=9), axis.text.x = element_text(angle=-90, hjust=1))

# Analysis: The most common penalties by far are offensive holding and false starts. This matches
# intuition from watching football.


# -----------------------------------------------------------------------------------------------------
# Histograms
# -----------------------------------------------------------------------------------------------------

Histogram <- function(data, title, xlabel, b){
  df <- data.frame(data)
  hist <- ggplot(df, aes(x=data))
  hist <- hist + geom_vline(aes(xintercept=median(data)), color="green", linetype="dashed", size=1)
  hist <- hist + geom_vline(aes(xintercept=mean(data)), color="gray64", linetype="dashed", size=1)
  hist <- hist + ggtitle(title)
  hist <- hist + xlab(xlabel) + ylab("Count")
  hist <- hist + geom_histogram(color="black", fill="lightslategray", bins = b)
  hist
}

# The number of yards gained on pass plays
index <- which(dataset$IsPass == 1); head(index)
yardsgained <- dataset$Yards[index]; head(yardsgained)
Histogram(yardsgained, "Yards Gained On Pass Plays", "Yards", 50)

# Analysis: The large spike at zero yards gained can be almost certainly to failed passes (which are
# still counted by the dataset as passes, just incomplete ones). The histogram is skewed to the right, 
# as we might expect, because most plays in football tend not to result in significant gains in
# yardage relative to the whole field. The histogram shows that passes that result in lost yards are
# uncommon, but nonetheless occur a decent amount of the time. When they do occur, they do not result
# in too significant of loss in yardage.


# The number of yards gained on rush plays
index <- which(dataset$IsRush == 1); head(index)
yardsgained <- dataset$Yards[index]; head(yardsgained)
Histogram(yardsgained, "Yards Gained On Rush Plays", "Yards", 50)

# Analysis: As with the previous histogram, there is a peak at zero yards, though in this case the
# count for small numbers of yards above 0 is also fairly high. This speaks to the frequency of
# rush plays in the NFL which result in small but safe yard gains leading up to first downs, or to 
# rushes employed inches or a few yards from the end zone. Similarly, the histogram is skewed to the 
# right, as expected.


# Yards to go until the first down
index <- which(dataset$ToGo != 0); head(index)
togo <- dataset$ToGo[index]; head(togo)
Histogram(togo, "Yards To Go", "Yards", 50)
max(togo)

# Analysis: Yards to go spikes at 10, which makes sense because when teams reach first downs they are
# given ten yards to their next checkpoint. Most of the other data is between 0 and 10, which makes
# sense because ideally teams make positive movement toward the first down checkpoint with each down.
# However, sometimes (because of sacks or because of particularly sad tackles), teams have more than 
# ten yards to go. In this data, the max value taken on is 40 (!) yards. This quarterback must have had
# a very difficult time moving the ball forward!


# The yard line where a play was made
index <- which(dataset$YardLine != 0); head(index)
yardline <- dataset$YardLine[index]; head(yardline)
Histogram(yardline, "Yard-lines where Plays were Made", "Yards", 50)

# Analysis: The spikes in plays occuring are between 25 and 35 yards, which makes sense because
# touchbacks bring teams to these yard lines very often (and this is a decent spot for punt returns).
# Additionally, the spot from which PAT attempts are taken (85 yard line) also has another spike; this
# is attributable almost entirely to special teams. Other than these spikes, the distribution is
# fairly constant across the field, though understandably few plays are made in the first 20 or so 
# yards. In this formulation, yards 1-50 are a team's own, and 50-100 are the opponent's 50-1 in that
# order. 

# -----------------------------------------------------------------------------------------------------
# Linear Regression
# -----------------------------------------------------------------------------------------------------

# Our linear regression will regress starting yard line of plays, for plays that
# are rushes or passes, against yards gained (or lost) on the play.

# Get a vector of the yard line data 
YardLine <- dataset$YardLine; head(YardLine)
Yards <- dataset$Yards; head(Yards)

# Find all rows where the play was either a rush or a pass
index <- c(which(dataset$IsPass == 1), which(dataset$IsRush == 1)); head(index)

# Get all entries where the ball is either rushed or passed
yardlineRushPass <- YardLine[index]; head(yardlineRushPass)
yardsRushPass <- Yards[index]; head(yardsRushPass)

# Do the linear regression - there is very little correlation between the two variables, given the 
# miniscule R-squared value!
linreg <- lm(yardsRushPass ~ yardlineRushPass)
summary(linreg)

# Now, we will use ggplot to see what the data look like, and where our regression line goes
require(ggplot2)

ggplot(linreg$model, aes_string(x = names(linreg$model)[2], y = names(linreg$model)[1])) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  labs(title = "Linear regression: Yards gained vs. Starting yard line") +
  xlab("Starting yard line of rushes and passes") +
  ylab("Yards gained")

# The relationship between starting yard line and yards gained/lost is a very strange one. Notice
# for example the linear-looking band that runs from (0, 100) to (100,0) with a slope of nearly exactly
# -1. This might represent the rare case of touchdowns occurring (passes or rushes from any yard line
# resulting in a gain of exactly 100 minus that yard line). The vast majority of data is contained 
# between -10 and 25 yards gained, which makes sense because a vast majority of passes do not result
# in much gain. Surprisingly, the regression shows us that (a) there is very ltitle correlation between
# starting yard line and yards gained or lost, and (b) that the line of best fit is effectively 
# constant at around 10 yards gained regardless of starting yard line of rushes or passes.

# Now, to show that our quantitative analysis matches our qualitative analysis:

# Adjusted r-square value is VERY small. This confirms that there is little correlation between the 
# variables we are regressing. 
summary(linreg)$adj.r.squared

# Intercept is ~9.4. If the relationship is constant, then this represents the average yards expected
# to be gained on any rush or pass.
linreg$coef[[1]]

# Slope is ~-0.04. This is functionally constant, but there is a very slight (likely insignificant)
# decrease in the yards gained prediction depending on starting yard line. 
linreg$coef[[2]]

# -----------------------------------------------------------------------------------------------------
# Contingency Table with Chi-Squared with DEFENSETEAM and PLAYTYPE
# -----------------------------------------------------------------------------------------------------

# Create a contingency table
tbl <- table(dataset$DefenseTeam, dataset$PlayType); tbl

# Extract just the columns corresponding to passes and rushes
passrush <- cbind(tbl[,9], tbl[,13]); passrush

# Analysis: The contingency table allows us to explore two categorical variables' impact on each other.
# We hypothesize that teams modify their decision to rush vs. pass depending on the team they are 
# playing against - teams with very good pass protection for example might be worth rushing more 
# against. The data reflect some of this intuition. In all cases, offensive teams choose to pass more
# often than they rush, regardless of the defensive team. But for some teams this difference is much
# more pronounced than for others - against the Carolina Panthers, teams choose to pass around 1.8
# times more often than to rush, while against the Oakland Raiders it is more like 1.3. We now
# run a chi-square test.

# We do a chi-square test to determine the likelihood that our result was due to random chance if
# there is actually no relationship between the decision to pass or rush and the defensive team.
chisq.test(passrush)

# The p-value is very low, so it is very unlikely that this would occur due to random chance.

# -----------------------------------------------------------------------------------------------------
# Permuation Test
# -----------------------------------------------------------------------------------------------------

# Compute a permutation test to see if the number of touchdowns per game scored in the first half
# of the season differs from the number of touchdowns scored in the second half of the season
istouchdown <- which(dataset$IsTouchdown == 1)

# If we do this, use the following data:
ElapsedDays <- as.numeric(difftime(as.Date(dataset$GameDate, "%m/%d/%y"), as.Date(dataset$GameDate[1], "%m/%d/%y"), units = "days")); head(ElapsedDays)
# This gives is the number of days since the start of the season. 9/7/17 returns 0 and 
# 12/31/17 returns 115. Since the season has 116 days, the first half of the season is days (0, 57) 
# and the second half is days (58, 115)

# First get the touchdowns per game in the first half of the season
S1index <- which(ElapsedDays < 58 ); S1index
half1 <- intersect(istouchdown,S1index); half1
tbl1 <- table(dataset$GameId[half1]); tbl1
mean(tbl1)

# Get the number of touchdowns in the second half of the season
S2index <- which(ElapsedDays > 57 ); S2index
half2 <- intersect(istouchdown,S2index); half2
tbl2 <- table(dataset$GameId[half2]); tbl2
mean(tbl2)

# Find the difference in means
difference <- mean(tbl1)-mean(tbl2); difference

# Get just the wins column
vec1 <- as.numeric(as.matrix(tbl1)); vec1
vec2 <- as.numeric(as.matrix(tbl2)); vec2
touchdowns <- c(vec1,vec2); touchdowns

# Run a permutation test by scrambling the touchdowns vector
N <-10000; diff <- numeric(N)
for (i in 1:N) {
  scramble <- sample(touchdowns,length(touchdowns))
  diff[i] <- mean(scramble[0:127])-mean(scramble[128:253])
}
hist(diff)
abline(v=difference, col = "red")
pvalue <- mean(diff > difference); pvalue

# Analysis


# -----------------------------------------------------------------------------------------------------
# Comparison of analysis by classical methods (chi-square, CLT) and simulation methods
# -----------------------------------------------------------------------------------------------------

# Comparison of chi-square by simulation

# Generate data to sample from for simulation
FirstHalfTD <- sum(dataset$IsTouchdown * (ElapsedDays< 57));
SecondHalfTD <- sum(dataset$IsTouchdown * (ElapsedDays >= 57));
halves=c("Half 1 TD", "Half 2 TD"); halves
TDs <- c(rep("Half 1 TD", FirstHalfTD), rep("Half 2 TD", SecondHalfTD));

# This is the vector listing of touchdowns of our dataset
# Are an equal number of touchdowns in each half?

# Observed breakdown for each half
Obs<-table(TDs);Obs 

# We would expect that the catagories are actual equal, because each is half a season
Expected <- rep(sum(Obs)/length(Obs),length(Obs)); Expected

#The classical built-in test
Pvalue <-chisq.test(Obs); Pvalue #P-value of .042 suggests that distribution is not uniform

# Our ChiSq function
ChiSq <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
}

CSq <- ChiSq(Obs, Expected);

# Perform Simulation
N =10^4 ; result<-numeric(N)
for (i in 1:N){
  TDs.sim<-sample(halves,sum(Obs), replace= TRUE)
  Obs.sim<-table(TDs.sim);
  Expected <- rep(sum(Obs.sim)/length(Obs.sim),length(Obs.sim))
  result[i]<-ChiSq(Obs.sim, Expected)
}

hist(result, breaks = "FD", probability = TRUE)
abline(v = CSq, col = "blue")

# This is our p-value; we should get a similar value as the pvalue from the built-in test
mean(result >= CSq)    
curve(dchisq(x, df=1), col = "red", add= TRUE)

# This is the result from the distribution function
pchisq(CSq, df = 1, lower.tail = FALSE)

# So the simulated Chi-Squared test via simulations and the built-in classical Chi-Square
# test give similar results

# Comparison of CLT and Simulation methods

# Get Data
YardLine <- dataset$YardLine;
NonZero <- which(YardLine != 0);
NonZeroYardLine <- YardLine[NonZero]; head(NonZeroYardLine)

# Pretend this is our population as we simulate the central limit theorem
hist(NonZeroYardLine)
PopSize <- length(NonZeroYardLine); PopSize

# Let's test with samples of size 30. Then the CTL says that the sampling mean will approach the population mean
SampleSize <- 30


# Simulation
N = 10^4 ; result<-numeric(N)
for (i in 1:N){
  result[i]<- mean(sample(NonZeroYardLine, size = SampleSize, replace = TRUE)) 
}
hist(result)

# And we can see that our sampling mean agrees with the CLT
mean(result)
mean(NonZeroYardLine)

# We can do the same simulation for standard deviation
PopSD <- sd(NonZeroYardLine) * (PopSize - 1) / PopSize;

# CLT says the sd of the normal approximation is
SamplingSD <- PopSD / sqrt(SampleSize); SamplingSD
# And we got
sd(result)

# We can overlay a CLT normal distribution over our simulated sample distribution
hist(result, freq = FALSE)
curve(dnorm(x, mean = mean(NonZeroYardLine), sd = SamplingSD), col = "red", add= TRUE)
# And we can see our CLT simulation and the Normal Distribution match nicely


# -----------------------------------------------------------------------------------------------------
# A probability density graph overlaid on a histogram
# -----------------------------------------------------------------------------------------------------

# Each team in a game gets three timeouts per half, which they are free to use whenever they’d like

# Convert quarter/minute/second into seconds since the start of the half
ElapsedTime <- (dataset$Quarter - 1) * 15 * 60 + (15 - dataset$Minute) * 60 +
  (60 - dataset$Second); head(ElapsedTime)

# SECTION 1: First half of the game -------------------------------------------------------------------

# Indices of all rows in Q1 or Q2
half1 <- c(which(dataset$Quarter == 1), which(dataset$Quarter == 2)); head(half1)

# PART 1: Use of timeout #1
# Search all rows where timout #1 was called
timeout1 <- which(grepl("TIMEOUT #1",as.character(dataset$Description))); head(timeout1)

# Take the intersection to get the indices of all rows in the first half where timeout #1 was taken
index <- intersect(half1, timeout1); head(index)

# The average time timeout 1 is used in the first half is ~1350 seconds (~22.5 minutes) into the half
mean(ElapsedTime[index])

# Here is the histogram of the frequency of timeout 1 usage with elapsed seconds, with the probability
# density overlaid. It shows that it is not rare for teams to use timeout #1 early in the half, but
# most opt to use it later.
ggplot(data.frame(TOs=ElapsedTime[index]), aes(x=TOs)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", binwidth=60) +
  geom_density() +
  labs(title="Uses of Timeout #1 vs. Elapsed Seconds") +
  xlab("Elapsed seconds") + 
  ylab("Frequency of Timeout #1 usage")


# PART 2: Use of timeout #2
# Search all rows where timout #2 was called
timeout2 <- which(grepl("TIMEOUT #2",as.character(dataset$Description))); head(timeout2)

# Take the intersection to get the indices of all rows in the first half where timeout #1 was taken
index <- intersect(half1, timeout2); head(index)

# The average time timeout 2 is used in the first half is ~1730 seconds (~29 minutes) into the half
mean(ElapsedTime[index])

# Here is the histogram of the frequency of timeout 2 usage with elapsed seconds, with the probability
# density overlaid. It shows that it is fairly rare to use timeout #2 before about 1250 seconds (~21 
# minutes) into the first half, and again that the vast majority of teams opt to use their timeouts
# fairly late into the game.
ggplot(data.frame(TOs=ElapsedTime[index]), aes(x=TOs)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", binwidth=60) +
  geom_density() +
  labs(title="Uses of Timeout #2 vs. Elapsed Seconds") +
  xlab("Elapsed seconds") + 
  ylab("Frequency of Timeout #2 usage")


# PART 3: Use of timeout #3
# Search all rows where timout #3 was called
timeout3 <- which(grepl("TIMEOUT #3",as.character(dataset$Description))); head(timeout3)

# Take the intersection to get the indices of all rows in the first half where timeout #3 was called
index <- intersect(half1, timeout3); head(index)

# The average time timeout 3 is used in the first half is ~1810 seconds (~30 minutes) into the half
mean(ElapsedTime[index])

# Here is the histogram of the frequency of timeout 3 usage with elapsed seconds, with the probability
# density overlaid. It shows that most teams which use timeout 3 opt to use it at the very end of the
# half, which makes sense because teams tend to use their timeouts in order to score late in the game.
ggplot(data.frame(TOs=ElapsedTime[index]), aes(x=TOs)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", binwidth=40) +
  geom_density() +
  labs(title="Uses of Timeout #3 vs. Elapsed Seconds") +
  xlab("Elapsed seconds") + 
  ylab("Frequency of Timeout #3 usage")

# SECTION 2: Second half of the game -------------------------------------------------------------------

# Update the vector to hold the number of seconds since the start of the second half
ElapsedTime <- (dataset$Quarter - 3) * 15 * 60 + (15 - dataset$Minute) * 60 + (60 - dataset$Second); head(ElapsedTime)

# Indices of all rows in Q3 or Q4
half2 <- c(which(dataset$Quarter == 3), which(dataset$Quarter == 4)); head(half2)


# PART 1: Use of timeout #1
# Search all rows where timout #1 was called
timeout1 <- which(grepl("TIMEOUT #1",as.character(dataset$Description))); head(timeout1)

# Take the intersection to get the indices of all rows in the second half where timeout #1 was taken
index <- intersect(half2, timeout1); head(index)

# The average time timeout 1 is used in the second half is ~1250 seconds (~21 minutes) into the half
mean(ElapsedTime[index])

# Here is the histogram of the frequency of timeout 1 usage with elapsed seconds, with the probability
# density overlaid. It shows that teams are willing to use their first timeout earlier in the second
# half than in the first half, and that there is not a huge spike in the probability density throughout
# the half. However, there is still a bias toward using even the first timeout fairly late in the game.
ggplot(data.frame(TOs=ElapsedTime[index]), aes(x=TOs)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", binwidth=60) +
  geom_density() +
  labs(title="Uses of Timeout #1 vs. Elapsed Seconds") +
  xlab("Elapsed seconds") + 
  ylab("Frequency of Timeout #1 usage")


# PART 2: Use of timeout #2
# Search all rows where timout #2 was called
timeout2 <- which(grepl("TIMEOUT #2",as.character(dataset$Description))); head(timeout2)

# Take the intersection to get the indices of all rows in the first half where timeout #1 was taken
index <- intersect(half2, timeout2); head(index)

# The average time timeout 2 is used in the second half is ~1640 seconds (~27 minutes) into the half
mean(ElapsedTime[index])

# Here is the histogram of the frequency of timeout 2 usage with elapsed seconds, with the probability
# density overlaid. It shows that teams are again more willing to use their second timeout earlier in
# the game, perhaps because it becomes more concrete how the game is going to play out near the end
# of the game overall. The largest spike in frequency happens two or so minutes before the end of the
# game, which makes sense because there still exists the cushion of timeout #3.
ggplot(data.frame(TOs=ElapsedTime[index]), aes(x=TOs)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", binwidth=60) +
  geom_density() +
  labs(title="Uses of Timeout #2 vs. Elapsed Seconds") +
  xlab("Elapsed seconds") + 
  ylab("Frequency of Timeout #2 usage")


# PART 3: Use of timeout #3
# Search all rows where timout #3 was called
timeout3 <- which(grepl("TIMEOUT #3",as.character(dataset$Description))); head(timeout3)

# Take the intersection to get the indices of all rows in the first half where timeout #3 was called
index <- intersect(half2, timeout3); head(index)

# The average time timeout 3 is used in the second half is ~1740 seconds (~29 minutes) into the half
mean(ElapsedTime[index])

# Here is the histogram of the frequency of timeout 3 usage with elapsed seconds, with the probability
# density overlaid. Surprisingly enough, teams which use their third timeout often use it fairly early
# (compared to in the first half). This might be because teams which are using their third timeout
# are often forced to do so, and they cannot save their timeout. Nevertheless, the average time this
# timeout is used is near the very end of the game, so the histogram is consistent with our expectation
# that the last timeout is often used as an attempt at saving the game near the end.
ggplot(data.frame(TOs=ElapsedTime[index]), aes(x=TOs)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", binwidth=60) +
  geom_density() +
  labs(title="Uses of Timeout #3 vs. Elapsed Seconds") +
  xlab("Elapsed seconds") + 
  ylab("Frequency of Timeout #3 usage")


# SECTION 3: Overtime ------------------------------------------------------------------------------------

# Update the vector to hold the number of seconds since the start of overtime
ElapsedTime <- (10 - dataset$Minute) * 60 + (60 - dataset$Second); head(ElapsedTime)

# Indices of all rows in Q5
over <- which(dataset$Quarter == 5); head(over)


# PART 1: Use of timeout #1
# Search all rows where timout #1 was called
timeout1 <- which(grepl("TIMEOUT #1",as.character(dataset$Description))); head(timeout1)

# Take the intersection to get the indices of all rows in the second half where timeout #1 was taken
index <- intersect(over, timeout1); head(index)

# The average time timeout 1 is used in overtime is ~350 seconds (~5.8 minutes)
mean(ElapsedTime[index])

# Here is the histogram of the frequency of timeout 1 usage with elapsed seconds, with the probability
# density overlaid. It shows that there is not a huge spike in frequency at any point in the quarter,
# and teams are generally willing to use their first timeout at any time during overtime.
ggplot(data.frame(TOs=ElapsedTime[index]), aes(x=TOs)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", binwidth=60) +
  geom_density() +
  labs(title="Uses of Timeout #1 vs. Elapsed Seconds") +
  xlab("Elapsed seconds") + 
  ylab("Frequency of Timeout #1 usage")


# PART 2: Use of timeout #2
# Search all rows where timout #2 was called
timeout2 <- which(grepl("TIMEOUT #2",as.character(dataset$Description))); head(timeout2)

# Take the intersection to get the indices of all rows in the first half where timeout #1 was taken
index <- intersect(over, timeout2); head(index)

# The average time timeout 2 is used in overtime is ~460 seconds (~8 minutes)
mean(ElapsedTime[index])

# Here is the histogram of the frequency of timeout 2 usage with elapsed seconds, with the probability
# density overlaid. It shows that timeout #2 is not often used in overtime, so it is difficult to 
# analyze its use, but again the frequency is relatively constant throughout overtime and teams 
# generally feel freer to use their timeouts whenever they would like during this period. This makes
# sense in some ways, because overtime games are by definition really close and the deciding moment
# in overtime could happen at functionally any point.
ggplot(data.frame(TOs=ElapsedTime[index]), aes(x=TOs)) + 
  geom_histogram(aes(y=..density..), color="black", fill="white", binwidth=60) +
  geom_density() +
  labs(title="Uses of Timeout #2 vs. Elapsed Seconds") +
  xlab("Elapsed seconds") + 
  ylab("Frequency of Timeout #2 usage")


# -----------------------------------------------------------------------------------------------------
# Logistic Regression
# -----------------------------------------------------------------------------------------------------

# For the following logistic regressions, we will use the yard line, days since the start of the season, 
# and the numer of seconds that have passed in the game since the start.

YardLine <- dataset$YardLine; head(YardLine)
ElapsedTime <- (dataset$Quarter - 1) * 15 * 60 + dataset$Minute * 60 + dataset$Second; head(ElapsedTime)
ElapsedDays <- as.numeric(difftime(as.Date(dataset$GameDate, "%m/%d/%y"), as.Date(dataset$GameDate[1], "%m/%d/%y"), units = "days")); head(ElapsedDays)

# -----------------------------------------------------------------------------------------------------

# We will also define a function that takes in a continuous variable and a Bernoulli variable
# and plots the logistic regression curve

logReg <- function(vec1, vec2, xlabel, ylabel)
{
  # Plot the logistic graph of the first vector vs. the second vector
  plot(vec1, vec2, main = "Logistic Regression", xlab = xlabel, ylab = ylabel) 
  
  # To create a function that is bounded between 0 and 1, assume that
  # p = e^(ax+b)/(1 + e^(ax+b)) 
  
  # Start with minus the log of the likelihood function
  LF <- function(alpha, beta)
  {
    -sum( log( exp(beta*vec1 + alpha)/(1+exp(beta*vec1 + alpha)) )*vec2
          + log(1/(1+exp(alpha+beta*vec1)))*(1-vec2) )
  }
  
  # An initial guess is required
  results<-mle(LF, start = list(alpha = 0, beta = 0)) 
  results@coef
  
  # Display the logistic regression curve
  curve(exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)
}

# -----------------------------------------------------------------------------------------------------
# PART 1: Decision to RUSH or PASS the ball

# Find all rows that satisfy IsPass == 1 or IsRush == 1
index <- c(which(dataset$IsPass == 1), which(dataset$IsRush == 1)); head(index)

# Get all entries where the ball is either rushed or passed
yardline <- YardLine[index]; head(yardline)
elapsedtime <- ElapsedTime[index]; head(elapsedtime)
elapseddays <- ElapsedDays[index]; head(elapseddays)
isrush <- dataset$IsRush[index]; head(isrush)
ispass <- dataset$IsPass[index]; head(ispass)

# SECTION A: Relationship to YARDLINE
logReg(yardline, isrush, xlabel="Yard line", ylabel="Proportion of plays that are rushes")

# Analysis: The logistic regression curve is approximately constant at 0.4, with a slight upwards
# incline. This means that the probability of rushing the ball from anywhere on the field is about
# 40%, with the probability of rushing very slightly increasing as the yard line increases. This result
# makes sense, because rushing is an important part of any NFL team's arsenal, though the league is
# primarily pass-oriented (hence the below-half probability). The slight uptick at the end of the field
# is likely explained by the higher likelihood of running safer rush plays near or past the goal line.

# SECTION B: Relationship to ELAPSEDTIME
logReg(elapsedtime, isrush, xlabel="Elapsed time (seconds)", ylabel="Proportion of plays that are rushes")

# Analysis: The curve shows that proportion of plays that are rushes begins at around 0.5, but slowly
# declines throughout the game until eventually bottoming out below 0.4. This result is unsurprising;
# later in the game, safer plays are substituted out in favor of passes which generally have a greater
# likelihood of resulting in significant gains in yardage.

# SECTION C: Relationship to ELAPSEDDAYS
logReg(elapseddays, isrush, xlabel="Elapsed days in the season", ylabel="Proportion of plays that are rushes")

# Analysis: The relationship between elapsed days in the season and proportion of plays that are rushes
# is constant, at a proportion of about 0.4. This is an unexpected result, because one would expect
# teams to generally behave more aggressively (pass more, rush less) later in the season just as the
# previous regression showed that they behave later in individual games. However, it is not totally
# surprising either. One interpretation could be that team decisionmaking at the level of
# individual types of plays is *much* more concerned with the game at hand than with considerations
# about the whole season.

# -----------------------------------------------------------------------------------------------------
# PART 2: Incomplete pass

# Find all rows that satisfy IsPass == 1
index <- which(dataset$IsPass == 1); head(index)

# Get all entries where the ball is passed
yardline <- YardLine[index]; head(yardline)
elapsedtime <- ElapsedTime[index]; head(elapsedtime)
elapseddays <- ElapsedDays[index]; head(elapseddays)
isincomplete <- dataset$IsIncomplete[index]; head(isrush)

# SECTION A: Relationship to YARDLINE
logReg(yardline, isincomplete, xlabel="Yard line", "Proportion of passes incomplete")

# Analysis: The regression shows that the proportion of incomplete passes increases with the yard line.
# This result makes sense because (a) the offense is likely to make riskier passes as they enter the
# range where touchdown attempts make sense, and (b) the defense is likely to become tighter as the
# offense progresses, and will be best at causing incompletions close to the end zone. It is worth
# noting that the proportion of passes incomplete overall is fairly high; across the NFL, about 40%
# of all passes are incomplete. This is fairly high, but at the same time unsurprising. It shows that
# defensive formations are generally decently successful!

# SECTION B: Relationship to ELAPSEDTIME
logReg(elapsedtime, isincomplete, xlabel="Elapsed time (seconds)", ylabel="Proportion of passes incomplete")

# Analysis: Per the graph, a smaller proportion of passes is complete later in a game. Similar factors
# describe this phenomenon as the relationship between yard line and proportion of passes incomplete.
# Namely, both teams make more aggressive plays later in a game (in 'do-or-die' situations), the
# offense by running riskier throws and the defense by tightening their defense (perhaps changing their
# coverage patterns up). It is also possible that, as defensive teams begin to get a greater 'read' on
# offenses, they begin to adapt their strategy and hence derease the proportion of passes incomplete.

# SECTION C: Relationship to ELAPSEDDAYS
logReg(elapseddays, isincomplete, xlabel="Elapsed days in the season", ylabel="Proportion of passes incomplete")

# Analysis: Unsurprisingly, there is little relationship between elapsed days in the season and the
# proportion of passes incomplete. It trends slightly upwards - one might expect that games later in
# the season tend to provoke greater desperation, but not enough to change the overall likelihood
# of passing success. 

# -----------------------------------------------------------------------------------------------------
# PART 3: Is the penalty accepted

# Find all rows that had a penalty
index <- which(dataset$IsPenalty == 1); head(index)

# Get all entries where there was a penalty
yardline <- YardLine[index]; head(yardline)
elapsedtime <- ElapsedTime[index]; head(elapsedtime)
elapseddays <- ElapsedDays[index]; head(elapseddays)
ispenaltyaccepted <- dataset$IsPenaltyAccepted[index]; head(ispenaltyaccepted)


# SECTION A: Relationship to YARDLINE
logReg(yardline, ispenaltyaccepted, xlabel="Yard line", ylabel="Proportion of penalties accepted")

# Analysis: The relationship between yard line and proportion of penalties accepted is constant. It is
# fairly surprising that the proportion of penalties accepted is so high in general - roughly 9 in 10
# penalties are accepted, which seems high considering that teams often gain more positive an outcome
# by denying penalties, especially those which do not gain many yards. It is not surprising that yard
# line does not affect the proportion of penalties accepted; it seems that this decision would be
# mostly determined by whether or not accepting the penalty would benefit the team at the time.

# SECTION B: Relationship to ELAPSEDTIME
logReg(elapsedtime, ispenaltyaccepted, xlabel="Elapsed time (seconds)", ylabel="Proportion of penalties accepted")

# Analysis: This is perhaps the most unexpected result thus far. The odds of accepting a penalty
# increases substantially throughout a game, starting at around 0.6 and capping off near 1. One would
# not expect this to be the case - if there is a penalty, then the team is likely to pick the better
# outcome between the play that occurred and the penalty - but the relationship is strong and evident. 
# One reason this may be true is that regaining downs may be more important than incremental advances
# in yardage near the end of a game, or that penalties near the end of the game are more frivolous
# (so teams have little reason to not accept).

# SECTION C: Relationship to ELAPSEDDAYS
logReg(elapseddays, ispenaltyaccepted, xlabel="Elapsed days in the season", ylabel="Proportion of penalties accepted")

# Analysis: Given the result of the relationship between elapsed time in a game and penalty acceptance,
# it is natural to then also expect that a similar relationship would be true for games throughout the
# season (i.e., teams would be more likely to accept penalties later in the season). However, the
# relationship between the two variables is effectively constant, perhaps because penalty acceptance
# is understood as a decision made in individual games rather than given the context of the entire
# season.

# -----------------------------------------------------------------------------------------------------
# PART 4: Is the challenge reversed

# Find all rows that had a challenge
index <- which(dataset$IsChallenge == 1); head(index)

# Get all entries where there was a challenge
yardline <- YardLine[index]; head(yardline)
elapsedtime <- ElapsedTime[index]; head(elapsedtime)
elapseddays <- ElapsedDays[index]; head(elapseddays)
ischallengereversed <- dataset$IsChallengeReversed[index]; head(ischallengereversed)


# SECTION A: Relationship to YARDLINE
logReg(yardline, ischallengereversed, xlabel="Yard line", ylabel="Proportion of successful challenges")

# Analysis: There is a slight decrease in proportion of challenges reversed as the yard line increases.
# This makes sense, because one can imagine that coaches are unlikely to make challenges that are 
# unlikely to reverse the ruling on the field when few yards have been attained at that point. This
# seems best interpreted as a statement about coaches' risk aversion relative to the yard line.

# SECTION B: Relationship to ELAPSEDTIME
logReg(elapsedtime, ischallengereversed, xlabel="Time elapsed (seconds)", ylabel="Proportion of successful challenges")

# Analysis: The portion of successful challenges is relatively constant against time in the game. This
# is a surprising result because one would expect that later in the game, coaches will make riskier
# challenges, which are less likely to be successful. This turns out not to be the case though; there
# is actually a slight increase in the proportion of successful challenges as time elapsed in a game
# increases. This is a highly unexpected result!

# SECTION C: Relationship to ELAPSEDDAYS
logReg(elapseddays, ischallengereversed, xlabel="Elapsed days in the season", ylabel="Proportion of successful challenges")

# Analysis: There is a very slightly positive relationship between the number of elapsed days in a 
# season and the proportion of successful challenges. This is also a slightly surprising result; one
# would think that coaches would make riskier challenges later in the season (where games may be 
# higher stakes), but in fact, this is not the case. 

# -----------------------------------------------------------------------------------------------------
# PART 5: Two point conversion success

# Find all rows that had a two point conversion
index <- which(dataset$IsTwoPointConversion == 1); head(index)

# Get all entries where there was a two point conversion
yardline <- YardLine[index]; head(yardline)
elapsedtime <- ElapsedTime[index]; head(elapsedtime)
elapseddays <- ElapsedDays[index]; head(elapseddays)
suc2ptconversion <- dataset$IsTwoPointConversionSuccessful[index]; head(suc2ptconversion)

# SECTION A: Relationship to ELAPSEDTIME
logReg(elapsedtime, suc2ptconversion, xlabel="Time elapsed (seconds)", ylabel="Proportion of successful 2pt conversions")

# Analysis: One important thing to note about this particular regression is that there are very few
# two-point conversions earlier in a game, so it is difficult to draw conclusions based on the 
# regression. This is because two-point conversions are generally viewed as risky (about half the 
# success rate of PAT attempts), and so teams typically opt to kick field goals instead. Nevertheless, 
# one might expect that this regression would show that the proportion of successful 2-point attempts
# is higher when they are unforced (i.e., happen earlier in the game, and they are not necessary in 
# order to win the game as they might be near the end). Instead, the data show that the proportion
# of successful 2-point conversions actually decreases with game time.

# SECTION B: Relationship to ELAPSEDDAYS
logReg(elapseddays, suc2ptconversion, xlabel="Elapsed days in the season", ylabel="Proportion of successful 2pt conversions")

# Analysis: Though with limited data points, this regression shows that there is a slight increase in
# the proportion of successful 2-point conversions later in the season. This makes sense, because
# teams might perform better in 'do-or-die' type scenarios in which their qualification to the playoffs
# (or their berth within the playoffs) requires that they win a game with a two-point conversion (since
# it is rare that two-point conversions are completed in less dire circumstances). Players in such
# tense situations may be more likely to successfully complete 2-point conversions, because their 
# seasons may rest on the outcome of the play.

# -----------------------------------------------------------------------------------------------------
# PART 6: Touchdown from a pass

# Find all rows that had a pass
index <- which(dataset$IsPass == 1); head(index)

# Get all entries where there was a pass
yardline <- YardLine[index]; head(yardline)
elapsedtime <- ElapsedTime[index]; head(elapsedtime)
elapseddays <- ElapsedDays[index]; head(elapseddays)
istouchdown <- dataset$IsTouchdown[index]; head(istouchdown)

# SECTION A: Relationship to YARDLINE
logReg(yardline, istouchdown, xlabel="Yard line", ylabel="Proportion of passes that are TDs")

# Analysis: Unsurprisingly, passes thrown from yard lines closer to the end zone are more likely to
# result in touchdowns. Intuitively, one would expect a pass thrown from the 20 or 30-yard line to
# have a close-to-zero likelihood of resulting in a touchdown, and the regression reflects this
# intuition. Instead of a linear increase in proportion, the relationship is closer to exponential. 
# This makes sense, because a pass from 95 yards is *much* more likely to result in a touchdown than a
# pass from the 80 yard line (about double the likelihood, per this regression).

# SECTION B: Relationship to ELAPSEDTIME
logReg(elapsedtime, istouchdown, xlabel="Time elapsed (seconds)", ylabel="Proportion of passes that are TDs")

# Analysis: There is a linear decrease in the proportion of passes that are touchdowns as time elapses
# in a game. This is expected, because (a) passes can be more frivolous later in the games (consider, 
# for example, the hail mary in a desperate case), and (b) plays can happen much quicker as time is
# running out, which can cause a smaller *proportion* of passes to result in touchdowns. 

# SECTION C: Relationship to ELAPSEDDAYS
logReg(elapseddays, istouchdown, xlabel="Elapsed days in the season", ylabel="Proportion of passes that are TDs")

# Analysis: There is practically no relationship between elapsed days in the season and proportion of
# passes that result in touchdowns. It is not unreasonable to expect that there would be an increase
# in effective passes as teams gather data about other teams' defenses and refine their game plans, 
# but it is also reasonable to think that proportion of passes that are TDs are highly game dependent
# and not very sensitive to time in the season. In this sense, the lack of a relationship is perhaps 
# surprising, though not groundbreaking. 

# -----------------------------------------------------------------------------------------------------
# PART 7: Touchdown from a rush

# Find all rows that had a rush
index <- which(dataset$IsRush == 1); head(index)

# Get all entries where there was a rush
yardline <- YardLine[index]; head(yardline)
elapsedtime <- ElapsedTime[index]; head(elapsedtime)
elapseddays <- ElapsedDays[index]; head(elapseddays)
istouchdown <- dataset$IsTouchdown[index]; head(istouchdown)


# SECTION A: Relationship to YARDLINE
logReg(yardline, istouchdown, xlabel="Yard line", ylabel="Proportion of rushes that are TDs")

# Analysis: This exhibits the same relationship as with passing, and for the same reason. In fact, 
# the proportion of rushes that result in a touchdown is actually almost the same for the 100-yard line
# (~0.4), though rushes have a much lower probability of resulting in touchdowns before the ~75 yard
# line (because rushes tend to not result in as many yards gained on average as passes).

# SECTION B: Relationship to ELAPSEDTIME
logReg(elapsedtime, istouchdown, xlabel="Time elapsed (seconds)", ylabel="Proportion of rushes that are TDs")

# Analysis: Unlike for passes, surprisingly the proportion of rushes that result in TDs does not vary
# very much with time elapsed in a game. This could speak to the preference of teams to rely on 
# passing (especially with talented quarterbacks) in desperate situations, especially since passes
# can be more time-effective than rushing later in a game. This is a surprising and welcome insight
# into how passing-vs-rushing decisions are made by offensive coordinators and head coaches.

# SECTION C: Relationship to ELAPSEDDAYS
logReg(elapseddays, istouchdown, xlabel="Elapsed days in the season", ylabel="Proportion of rushes that are TDs")

# Analysis: This relationship is exactly the same as for passing, and for the same reason! No 
# additional insight is pertinent here, for the rushing-specific case.

# -----------------------------------------------------------------------------------------------------
# PART 8: Intercepted pass

# Find all rows that had a pass
index <- which(dataset$IsPass == 1); head(index)

# Get all entries where there was a pass
yardline <- YardLine[index]; head(yardline)
elapsedtime <- ElapsedTime[index]; head(elapsedtime)
elapseddays <- ElapsedDays[index]; head(elapseddays)
isinterception <- dataset$IsInterception[index]; head(isinterception)


# SECTION A: Relationship to YARDLINE
logReg(yardline, isinterception, xlabel="Yard line", ylabel="Proportion of passes intercepted")

# Analysis: There is unsurprisingly no real relationship between yard line and the proportion of passes
# that are intercepted. One reason this may be true is that interceptions are not correlated highly
# with the particular situation a team is in; they are just unlikely events that cannot be explained
# by more than random chance. We continue regressions to potentially disprove this theory below.

# SECTION B: Relationship to ELAPSEDTIME
logReg(elapsedtime, isinterception, xlabel="Time elapsed (seconds)", ylabel="Proportion of passes intercepted")

# Analysis: No relationship was expected between the proportion of passes intercepted and time elapsed
# in a game, but surprisngly enough, fewer passes are intercepted later in a game. This seems
# counterintuitive - wouldn't quarterbacks be less careful later in the game? - but makes sense when
# considering quarterbacks could prioritize control of the ball over risky scoring. Another
# puzzling part of this is that the relationship is roughly linear; why would quarterbacks be less
# prone to interceptions at the start of the third quarter than at the start of the first? This could
# potentially be explained by quarterbacks 'warming up' throughout a game, and being less likely to
# throw interceptions as a result.

# SECTION C: Relationship to ELAPSEDDAYS
logReg(elapseddays, isinterception, xlabel="Elapsed days in the season", ylabel="Proportion of passes intercepted")

# Analysis: The proportion of passes intercepted stays relatively constant with elapsed days in the
# season, unsurprisingly. This can be explained by quarterbacks not playing more or less carefully
# depending on time in the season (i.e, their game style remains fixed regardless of time in the 
# season).

# -----------------------------------------------------------------------------------------------------
# PART 9: Occurrence of a fumble

# Find all rows that had a rush or a pass
index <- c(which(dataset$IsPass == 1), which(dataset$IsRush == 1)); head(index)

# Get all entries where there was a pass or a rush
yardline <- YardLine[index]; head(yardline)
elapsedtime <- ElapsedTime[index]; head(elapsedtime)
elapseddays <- ElapsedDays[index]; head(elapseddays)
isfumble <- dataset$IsFumble[index]; head(isfumble)


# SECTION A: Relationship to YARDLINE
logReg(yardline, isfumble, xlabel="Yard line", ylabel="Proportion of plays fumbled")

# Analysis: There is little to no relationship between yard line and proportion of plays fumbled. 
# See part 9, section A for why this is the case (interception and fumble have the same 
# behavior here).

# SECTION B: Relationship to ELAPSEDTIME
logReg(elapsedtime, isfumble, xlabel="Time elapsed (seconds)", ylabel="Proportion of plays fumbled")

# Analysis: So few plays result in a fumble that it is tough to see any relationship between time
# elapsed in a game and proportion of plays fumbled. It is roughly constant at proportion ~0. If
# the result is indeed constant, then it is curiously different than the result for interceptions (more
# interceptions occur per play earlier in the game). This could be because interceptions are more
# directly controlled by the quarterback, while fumbles are more random events that no one player
# or coach has clear decision control over. 

# SECTION C: Relationship to ELAPSEDDAYS
logReg(elapseddays, isfumble, xlabel="Elapsed days in the season", ylabel="Proportion of plays fumbled")

# Analysis: Portion of plays fumbled is relatively constant near zero with elapsed days in the season.
# This is a similar result to what was seen for interceptions - see part 9, section C for that 
# analysis. 

# -----------------------------------------------------------------------------------------------------
# PART 10: Occurrence of a penalty on a pass or a rush

# Find all rows that had a rush or a pass
index <- c(which(dataset$IsPass == 1), which(dataset$IsRush == 1)); head(index)

# Get all entries where there was a pass or a rush
yardline <- YardLine[index]; head(yardline)
elapsedtime <- ElapsedTime[index]; head(elapsedtime)
elapseddays <- ElapsedDays[index]; head(elapseddays)
ispenalty <- dataset$IsPenalty[index]; head(ispenalty)

# SECTION A: Relationship to YARDLINE
logReg(yardline, ispenalty, xlabel="Yard line", ylabel="Proportion of passes or rushes resulting in penalty")

# Analysis: The proportion of plays resulting in a penalty is fairly constant at about ~0.075 with yard
# line, though it tends upward ever so slightly (higher yard lines have a slightly greater chance of
# penalty). This relationship is decidedly small, which is expected because players are naturally
# averse to penalties and generally have little to gain by committing penalties intentionally, even
# near the end zone (since teams which have penalties against them can choose to decline the penalty).
# The slight trend upward linearly is likely insignificant, but could potentially be attributed to
# penalties out of frustration or increased sense of responsibility to make big plays near the end zone.

# SECTION B: Relationship to ELAPSEDTIME
logReg(elapsedtime, ispenalty, xlabel="Time elapsed (seconds)", ylabel="Proportion of passes or rushes resulting in penalty")

# Analysis: Surprisingly, and perhaps counterintuitively, the proportion of passes or rushes resulting
# in a penalty decreases with elapsed time. One might think that players are more likely to commit
# penalties as games draw to a close, because they might be forced to make big plays in order to 
# seize a close victory, as conjectured in the previous part, but the data do not reflect this. A
# different take on things could be that players are especially averse to committing penalties later
# in the game, because they may further jeopardize a team's (perhaps waning) chance of success.

# SECTION C: Relationship to ELAPSEDDAYS
logReg(elapseddays, ispenalty, xlabel="Elapsed days in the season", ylabel="Proportion of passes or rushes resulting in penalty")

# Analysis: The relationship between elapsed days in the season and proportion of passes or rushes
# resulting in a penalty is relatively constant. This seems to make sense - there is no incentive
# for players to increase or decrease the number of penalties they commit later in the season, and also
# there is little reason to believe that players fundamentally become more or less risk averse on the
# field later in the season. So, the constant relationship between these two variables is expected.

# -----------------------------------------------------------------------------------------------------
# END
# -----------------------------------------------------------------------------------------------------
