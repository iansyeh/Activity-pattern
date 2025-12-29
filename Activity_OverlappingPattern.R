# Activity Pattern
# by Dian Andi Syahputra :)
# This is my very first script, created with the help of ChatGPT
# I hope it is easy to understand, and I would appreciate any corrections or suggestions


# Make sure you have installed the required packages before running the analysis
# (overlap, readxl, or readr), then load them using library()
library(overlap)
library(readxl)

# Insert the file path to your dataset
# Make sure the dataset is located in the specified folder
Trial_Data <- read_excel("Activity.xlsx", sheet = "Activity")  #Input dataset
range(Trial_Data$Appear)
timeRad <- Trial_Data$Appear * 2 * pi #convert time data into 


# General kernel density estimation for activity patterns (density plot)

TK <- timeRad[Trial_Data$`Species name` == 'Tragulus kanchil']
densityPlot(
  TK,
  rug = TRUE,
  main = expression(italic("Tragulus kanchil"))
)
rect(0,0,5,1, density=NA, border = FALSE, col = rgb(0.3,0.3,0.3,1/4))
rect(5,0,7,1, density=NA, border = FALSE, col = rgb(0.3,0.3,0.3,1/2))
rect(17,0,19,1, density=NA, border = FALSE, col = rgb(0.3,0.3,0.3,1/2))
rect(19,0,24,1, density=NA, border = FALSE, col = rgb(0.3,0.3,0.3,1/4))

PTS <- timeRad[Trial_Data$`Species name` == 'Panthera tigris sumatrae']
densityPlot(
  PTS,
  rug = TRUE,
  main = expression(italic("Panthera tigris sumatrae"))
)
rect(0,0,5,1, density=NA, border = FALSE, col = rgb(0.3,0.3,0.3,1/4))
rect(5,0,7,1, density=NA, border = FALSE, col = rgb(0.3,0.3,0.3,1/2))
rect(17,0,19,1, density=NA, border = FALSE, col = rgb(0.3,0.3,0.3,1/2))
rect(19,0,24,1, density=NA, border = FALSE, col = rgb(0.3,0.3,0.3,1/4))


# Analysis of activity overlap


min(length(PTS), length(TK))
OBAest <- overlapEst(PTS, TK, type="Dhat1")
OBAest             # Coefficient of Overlapping
overlapPlot(PTS, TK)
rect(0,0,5,1, density=NA, border = FALSE, col = rgb(0.3,0.3,0.3,1/4))
rect(5,0,7,1, density=NA, border = FALSE, col = rgb(0.3,0.3,0.3,1/2))
rect(17,0,19,1, density=NA, border = FALSE, col = rgb(0.3,0.3,0.3,1/2))
rect(19,0,24,1, density=NA, border = FALSE, col = rgb(0.3,0.3,0.3,1/4))
legend(
  "topleft",
  legend = c("Panthera tigris sumatrae", "Tragulus kanchil"),
  lty = c(1, 2),
  col = c(1, 2),
  bty = "n",
  cex = 0.7,      # 👈 text size (this is the main one)
  lwd = 1.2,      # 👈 thinner legend lines
  seg.len = 1.5     # 👈 shorter line segments
)
OAboot <- resample(PTS, 10000)
dim(OAboot)
OBboot <- resample(TK, 10000)
dim(OBboot)
OBA <- bootEst(OAboot, OBboot, type="Dhat1")
( BSmean <- mean(OBA) )            # Bootstrap mean
bootCI(OBAest,OBA) 
bootCIlogit(OBAest,OBA) 


# Display the Dhat value on the graph

dhat <- overlapEst(PTS, TK, type = "Dhat1")
dhat
usr <- par("usr")

text(
  x = usr[2] - 0.2,
  y = usr[4] * 0.93,
  labels = bquote(hat(Delta)[1] == .(round(dhat, 3))),
  adj = c(1, 1),
  cex = 0.85,
  font = 2
)


# Finished — you’ve done a really good job :)
# Thank you for taking the time to run this analysis
# I hope this script is helpful and easy to follow

