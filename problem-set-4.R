# Joe Ludmir - Problem Set 4 - Professor Montgomery - Due: 3/3/16

library("rvest")
# Accessed the wikipedia page
url <- 'https://en.wikipedia.org/wiki/List_of_United_States_presidential_elections_by_popular_vote_margin'
# Using the rvest package, I found the xpath package on google crhome and created a 
# new table using the specific table needed on the wikipedia page
population <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[2]') %>%
  html_table()
# Made the table from a list into a data frame for easier manipulation.
pres.elects <- as.data.frame(population)
# Removed the first two rows as they were useless and contained no info
pres.elects <- pres.elects[c(-1,-2),]
# Renamed columns to make them easier to understand and more concise.
colnames(pres.elects)[1] <- "Election #"
colnames(pres.elects)[3] <- "Winner"
colnames(pres.elects)[4] <- "Win Party"
colnames(pres.elects)[5] <- "Vote %"
colnames(pres.elects)[6] <- "Margin by %"
colnames(pres.elects)[7] <- "Vote Total"
colnames(pres.elects)[8] <- "Margin by Total"
colnames(pres.elects)[9] <- "Loser"
colnames(pres.elects)[10] <- "Lose Party"
# For the negative percentages in the margin by percent column, the negative sign did
# not convert well onto R, so I deleted all the strange characters before the actual percent total.
for(i in 1:4){
  end <- nchar(pres.elects$`Margin by %`[i])
  pres.elects$`Margin by %`[i] <- substr(pres.elects$`Margin by %`[i], 10, end)
}
# Got rid of the duplicate numbers before all percent totals less than 10, again
# due to the conversion from wikipedia to R.
for(i in 5:29){
  pres.elects$`Margin by %`[i] <- substr(pres.elects$`Margin by %`[i], 7, 11)
}
# Got rid of the percent sign for all of the margin values and made them numeric
pres.elects$`Margin by %` <-  as.numeric(sub("%", "", (pres.elects$`Margin by %` )))
# Changed the sign of the percent totals to negative to match the wikipedia page's numbers.
for(i in 1:4){
  pres.elects$`Margin by %`[i] <- -1*(pres.elects$`Margin by %`[i])
}
# Does the same things as line 24-25 but for the margin totals, not percentages.
for(i in 1:4){
  end.two <- nchar(pres.elects$`Margin by Total`[i])
  pres.elects$`Margin by Total`[i] <- substr(pres.elects$`Margin by Total`[i], 10, end.two)
}
# This for loop takes away the string of eight random numbers before each margin total
# for all but the first 4 entries to align the numbers with those on wikipedia. 
for(i in 5:48){
  end.two <- nchar(pres.elects$`Margin by Total`[i])
  pres.elects$`Margin by Total`[i] <- substr(pres.elects$`Margin by Total`[i], 9, end.two)
}
# Removes commas from vote totals and makes them numbers as opposed to strings
pres.elects$`Margin by Total` <-  as.numeric(gsub(",", "", (pres.elects$`Margin by Total`)))
# Makes the numbers negative that were originally so
for(i in 1:4){
  pres.elects$`Margin by Total`[i] <- -1*(pres.elects$`Margin by Total`[i])
}
# Getting rid of commas and % signs and making several subsets of data numerical
pres.elects$`Vote Total` <-  as.numeric(gsub(",", "", (pres.elects$`Vote Total`)))
pres.elects$`Vote %` <-  as.numeric(sub("%", "", (pres.elects$`Vote %` )))
pres.elects$`Turnout` <-  as.numeric(sub("%", "", (pres.elects$`Turnout` )))
pres.elects$`Election #` <- as.numeric(pres.elects$`Election #`)
pres.elects <- pres.elects[order(pres.elects$Year),]
# First Plot - Comparing Republican and Democratic Trends in Popular Vote Percentage For Winners
# This allows for two plots to be shown simultaneously and to save them to a pdf.
pdf("pset4.pdf", width = 10.5)
this.par <- par(mfrow=c(1,2))
# Creates two sets of x and y points by political party placing 
# the margin by percent won over the course of the years.
y1 <- pres.elects$`Margin by %`[which(pres.elects$`Win Party`== "Rep.")]
x1 <- pres.elects$Year[which(pres.elects$`Win Party`== "Rep.")]
x2 <- pres.elects$Year[which(pres.elects$`Win Party`== "Dem.")]
y2 <- pres.elects$`Margin by %`[which(pres.elects$`Win Party`== "Dem.")]
# Added colors for labels in plots
par(col.lab = "darkmagenta", col.main = "darkmagenta")
# Make plot and labels, resize to make sure both plots can fit
plot(NULL, xlim=c(min(pres.elects$Year), max(pres.elects$Year)), 
     ylim=c(5,15), main = "Trends in Winning Voting Margins by Party", 
     xlab = "Year", ylab = "Margin by % Points", cex.axis=0.8, cex.main = 0.7)
# Create regression lines for republicans, democrats, and for all.
abline(lm(y1~x1), col = "red", lty = 2, lwd = 2.5)
abline(lm(y2~x2), col = "blue", lty = 3, lwd = 2.5)
abline(lm(pres.elects$`Margin by %`~pres.elects$Year), lwd = 2.5)
# Create a legend
legend("topright",c("Regression of Democrat Winners", "Regression of Republican Winners", "All"), 
       fill = c("blue","red","black"), cex=0.5)
# Plot 2 - Do turnout rates affect the margin of vote percentage?
# Same logic as plot 1 but with turnout rates as opposed to time
y3 <- pres.elects$`Margin by %`[which(pres.elects$`Win Party`== "Rep.")]
x3 <- pres.elects$Turnout[which(pres.elects$`Win Party`== "Rep.")]
x4 <- pres.elects$Turnout[which(pres.elects$`Win Party`== "Dem.")]
y4 <- pres.elects$`Margin by %`[which(pres.elects$`Win Party`== "Dem.")]
par(col.lab = "darkmagenta", col.main = "darkmagenta")
plot(NULL, xlim=c(20,90), 
     ylim=c(0,30), main = "Trends Between Vote Turnout and Winning Margin", 
     xlab = "Turnout by % Points", ylab = "Margin by % Points", cex.axis=0.8,
     cex.main = 0.55)
abline(lm(y3~x3), col = "red", lty = 2, lwd = 1.5)
abline(lm(y4~x4), col = "blue", lty = 3, lwd = 1.5)
abline(lm(pres.elects$`Margin by %`~pres.elects$Turnout), lwd = 1.5)
legend("topright",c("Regression Democrat Winners", "Regression of Republican Winners", "All"), 
       fill = c("blue","red","black"), cex=0.45)
dev.off()