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