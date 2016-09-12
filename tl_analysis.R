# Titre          tl_analysis.R
# Author         Yannick Rochat, UNIL\LETTRES\ISH | EPFL\CDH\DHLAB | UNIL\SSP\IMA
# Date           2016/09/12
# Version        0.4
# Description    This script intends to describe and analyse tweets as received by Twitter's data retrieval own tool
# License        GNU General Public License

# How to cite    ...

################
### VERSIONS ###
################

# 0.4 Compatibility with R and Twiter archive format

# 0.3 Dates

# 0.2 Translation from french to english (unfinished)

# 0.1 First version

##########################
### HOW DOES IT WORK ? ###
##########################

# BEGINNER Open this script inside your Twitter archive 
# EXPERT Manually choose your Twitter archive as working directory
# (e.g. drag and drop it on the R logo)

# TO EVALUATE THIS SCRIPT LINE AFTER LINE :
# On Mac, bring the cursor on the line you want to evaluate, don't
# select anything, then press (and keep) the "Apple touch" and press enter.

# If you have selected some text, it will be evaluated instead of
# the single line.

# If you want to evaluate the whole script, select it entirely,
# press "Apple touch" + enter, and pray. This script is intended to work that way.

# I try in the comments to explain what happens, at least the first a function is called
# In case you wonder how something works, for example the rm function used a few lines below,
# please use the command "?" (in our case : "?rm") to access its help file
# If you're wondering how to do something but don't know how to, make a search with "??"
# For example : "??remove"
# If you find nothing or nothing relevant, then google it :-) 

# Cleaning working space

rm(list=ls())

# Defining working directory. On mac, "~" is your home folder and
# "Desktop/Files" means that folder "Files" lies into "Desktop"
# DON'T FORGET TO REPLACE MY OWN WORKING DIRECTORY BY YOURS
# and to copy the Twitter folder "tweets" into it

wd <- getwd()

# Entering your Twitter "tweets" folder

setwd(paste(wd, "/data/js/tweets", sep=""))


############################
### Loading the packages ###
############################

# If you have never installed one of these packages, please first evaluate
# the 5 following lines but without the comment (= "#")

# install.packages("rjson")
# install.packages("wordcloud")
# install.packages("tm")
# install.packages("lubridate")
# install.packages("scales")

require(rjson)			# deals with json format
require(tm)				# text mining
require(lubridate)		# nicest way to deal with dates
require(scales)			# alternative color package


##########################
### LOADING THE TWEETS ###
##########################

# List all .js files
lf <- list.files()

# Reading these .js files.
tw <- sapply(lf, readLines, warn = FALSE)

# Cleaning names
names(tw) <- NULL

# Deleting first "parasite" line
tw <- sapply(tw, function(x) x[-1])

# Suppressing line breaks
tw <- sapply(tw, paste, collapse=" ")

# Converting from JSON to an R list
tw <- lapply(tw, fromJSON)			

# Suppressing month breaks
tw <- unlist(tw, recursive = FALSE)


#############
### DATES ###
#############

# Extracting dates
crea.temp <- sapply(tw, function(x) x$created_at)

# Converting dates from characters to POSIX # example : "2013-09-02 15:42:33 +0000"
crea <- strptime(crea.temp, "%Y-%m-%d %H:%M:%S %z")

# Leaving your Twitter "tweets" folder and going back to your original working directory
# This is were the graphical outputs will be saved
setwd(wd)


########################
### NUMBER OF TWEETS ###
########################

# If you want to preview your tweets and retweets metadata ,
# evaluate without the comment (#)
# head(tw)

# Total number of tweets
length(tw)

# Number of tweets per day computed like that :
# Number of tweets / (last tweet date - oldest tweet date)
difft <- as.numeric(tail(crea,1) - head(crea,1))				

cat("You have sent", length(tw) / difft, "tweets per day over", difft/365, "years.")



################
### RETWEETS ###
################

# Returns empty list or retweet itself
RT <- lapply(tw, function(x) x$retweeted_status)

# Either TRUE if it is a retweet, or FALSE if it isn't
RT.status <- !sapply(RT, is.null)			

# Number of TRUE's, i.e. numbre of retweets
cat("Among your", length(tw), "tweets, there were", sum(RT.status), "retweets.")

# Proportion of RTs
cat("This is", sum(RT.status) / length(RT.status), "% of your corpus.")

# Who you retweet
RT.id <- sapply(tw[RT.status], function(x) x$retweeted_status$user$screen_name)

# Frequency of who you retweet
RT.id.df <- as.data.frame(table(RT.id), stringsAsFactors = FALSE)

# Frequency of who you retweet, ordered
RT.Freq <- RT.id.df$Freq
names(RT.Freq) <- RT.id.df$RT.id

# And visualised
pdf("RT_people.pdf", width=1200/72, height=1200/72)
	par(mar=c(5,12,4,2))							# margins
	barplot(RT.Freq[tail(order(RT.Freq), 39)], 		# data
			horiz = TRUE, 							# graph orientation
			las = 2, 								# names orientation
			main = "Name of retweeted users", 		# title
			cex.main = 3, 							# title size
			cex.axis = 2)							# labels size
abline(v=1:20*10, lty = 3)
dev.off()

cat("Le compte que vous avez le plus retweeté est", names(RT.Freq[tail(order(RT.Freq), 1)]))

################
### MENTIONS ###
################

# Number of mentions appearing in each tweet
mentions <- sapply(tw, function(x) length(x$entities$user_mentions))

# Distribution of mentions
mentions.df <- as.data.frame(table(mentions), stringsAsFactors = FALSE)

# All IDs mentioned (not unique & retweets are omitted via the intersect command)
mentions.id <- unlist(sapply(tw[intersect(which(!RT.status), which(mentions != 0))], function(x) sapply(x$entities$user_mentions, function(y) y$id)))

# ID frequencies
mentions.id.df <- as.data.frame(table(mentions.id), stringsAsFactors = FALSE)

# Number of mentions per tweet
pdf("mentions_per_tweet.pdf")
	barplot(	table(mentions), 									# data
				pch=20, 											# choice of symbol
				log="y", 											# log scale on y-axis
				xlab="Mentions", 
				ylab="Tweets", 
				main = "Number of mentions per tweet")
	legend(	"topright", 											# legend position
			bty = "n", 												# legend frame
			legend = paste(sum(mentions > 0), " tweets,\ncontaining at least\none mention,\n", sum(mentions), " mentions\nin total.", sep=""))
	abline(	h = 10^(1:round(log10(max(table(mentions))),0)), 		# plot horizontal lines
			lty = "dotted", 
			col = "lightgray")
	abline(	h = 5*10^(0:(round(log10(max(table(mentions))),0)-1)), 
			lty = "dotted", 
			col = "lightgray")
dev.off()

# Proportion of tweets with at least one mention
sum(mentions.df[-1,]$Freq / length(tw))

# The tweet(s?) with the maximum number of mentions
tw[[which.max(mentions)]]$text

# Number of unique mentions (people mentioned at least once)
length(unique(mentions.id))

# Proportion of unique mentions to total number of mentions
length(unique(mentions.id)) / length(mentions.id)

# Number of unique mentions per number of tweets
length(unique(mentions.id)) / length(tw)

# Distribution of mentions
mentions.id.hist <- hist(mentions.id.df$Freq, breaks = seq(min(mentions.id.df$Freq), max(mentions.id.df$Freq)+1, 1), plot = FALSE)

# Let's plot it
pdf("mentions_users.pdf")
	plot(	head(mentions.id.hist$breaks, -1), 		
			mentions.id.hist$counts, pch = 20, 
			xlab = "Mentions", 
			ylab = "Users", 
			main = "Distribution of mentions per user", log="xy")
	grid()
	legend(	"topright", 
			bty = "n", 
			legend = paste(sum(mentions.id.hist$counts), " users\nwere mentioned\n", sum(mentions.id.df$Freq), " times\nin total.", sep=""))
dev.off()

# Extracting usernames
mentions.screen_name <- unlist(sapply(tw[intersect(which(!RT.status), which(mentions != 0))], function(x) sapply(x$entities$user_mentions, function(y) y$screen_name)))

# Frequency of each user name
mentions.screen_name.df <- as.data.frame(table(mentions.screen_name), stringsAsFactors = FALSE)

# 20 most cited
tail(mentions.screen_name.df[order(mentions.screen_name.df$Freq),], 20)

# Note : we obtain with mentions.screen_name.df (at least it appears to be my case)
# the same results as mentions.id.df[order(mentions.id.df$Freq),]
identical(sort(mentions.screen_name.df$Freq), mentions.id.df[order(mentions.id.df$Freq),]$Freq)


################
### HASHTAGS ###
################

# Number of hashtags per tweet
hashtags <- sapply(tw, function(x) length(x$entities$hashtags))

# Distribution of number of hashtags per tweet
hashtags.df <- as.data.frame(table(hashtags), stringsAsFactors = FALSE)

# The tweet containing the maximum of hashtags (my best score is 20, yours ?)
tw[[which.max(hashtags)]]$text

# Getting the hashtags
hashtags.id <- unlist(sapply(tw[which(hashtags != 0)], function(x) sapply(x$entities$hashtags, function(y) y$text)))

# Frequency of each hashtag
hashtags.id.df <- as.data.frame(table(tolower(hashtags.id)), stringsAsFactors = FALSE)

# Total number of hashtags
length(hashtags.id)

# Total number of unique hashtags
length(tolower(unique(hashtags.id)))

# Number of hashtags per tweet
pdf("hashtags_per_tweet.pdf")
	plot(	hashtags.df, 
			pch=20, 
			log="xy", 				# log scales on x- & y-axis
			xlab="Hashtags", 
			ylab="Tweets", 
			main = "Number of hashtags per tweet")
	for(i in 1:9) abline(h = i*10^(0:7), col = "lightgray", lty = "dotted")		# 7 (= 10'000'000 tweets) should suffice
	for(i in 1:9) abline(v = i*10^(0:7), col = "lightgray", lty = "dotted")
	legend("topright", bty = "n", legend = c(
		paste("#hashtags = ", sum(as.numeric(hashtags.df[,1]) * hashtags.df[,2]), "\n", sep=""), 	
		paste("#tweets = ", sum(hashtags.df[-1,2]), "\n", sep=""),
		paste(hashtags.df[1,2], " tweets\ndon't own\na single\nhashtag", sep="")))
dev.off()

# Distribution of hashtags
hashtags.id.hist <- hist(hashtags.id.df$Freq, breaks = seq(min(hashtags.id.df$Freq), max(hashtags.id.df$Freq)+1, 1), plot = FALSE)

# histogram
pdf("hashtags_dist.pdf")
	plot(	head(hashtags.id.hist$breaks, -1), 
			hashtags.id.hist$counts, 
			pch = 20, 
			ylab = "Number of hashtags", 
			xlab = "Number of apparitions", 
			main = "Number of apparitions for each hashtag", 
			log="xy")
	for(i in 1:9) abline(h = i*10^(0:7), col = "lightgray", lty = "dotted")
	for(i in 1:9) abline(v = i*10^(0:7), col = "lightgray", lty = "dotted")
	legend("topright", bty = "n", legend = c(
		paste("#hashtags uniques = ", nrow(hashtags.id.df), "\n", sep=""), 
		paste("Total number of tweets = ", length(tw), sep="")))
dev.off()

# 20 used hashtags the most
tail(hashtags.id.df[order(hashtags.id.df$Freq),], 20)


############
### URLs ###
############

# Distribution of urls
urls <- sapply(tw, function(x) length(x$entities$urls))
urls.df <- as.data.frame(table(urls), stringsAsFactors = FALSE)
urls.df

# Proportion of tweets containing at least one URL
sum(urls.df$Freq[-1]) / sum(urls.df$Freq)

# various URLs (entire name)
urls.id <- unlist(sapply(tw[which(urls != 0)], function(x) sapply(x$entities$urls, function(y) y$expanded_url)))

# Function taken from example(grep). It isolates domain name.
URL_parts <- function(x) {
     m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
     parts <- do.call(rbind,
                      lapply(regmatches(x, m), `[`, c(3L, 4L, 6L, 7L)))
     colnames(parts) <- c("protocol","host","port","path")
     parts
}

# Sites in my tweets (all : mentionned or retweeted)
websites <- as.data.frame(URL_parts(urls.id), stringsAsFactors = FALSE)$host	# Some NAs !?
websites.df <- as.data.frame(table(websites), stringsAsFactors = FALSE)			# frequency of each domain
websites.Freq <- websites.df$Freq
names(websites.Freq) <- websites.df$websites
tail(websites.Freq[order(websites.Freq)], 40)									# 40 most frequent URLs

# We visualize the most cited urls
pdf("sites_web.pdf", width=1200/72, height=1200/72)		# 1200 pixels, has to be in inches, there's 72 pix / inch
	par(mar=c(5,12,4,2))
	barplot(	websites.Freq[tail(order(websites.Freq), 40)], 
				horiz = TRUE, 
				cex.names = 1, 
				las = 2, 
				main = "Domain name of cited urls", 
				cex.main = 3, 
				cex.axis = 2)
	abline(v=1:10*50, lty = 3)
dev.off()


########################
### LENGTH OF TWEETS ###
########################

# Extracting the texts
texts <- sapply(tw, function(x) x$text)

# clean problematic characters, for example < and >
texts <- gsub("\\&gt\\;", "", texts)
texts <- gsub("\\&lt\\;", "", texts)
texts <- gsub("\\&amp", "", texts)

# In this §, we don't deal with RTs : they are shortened to 140 characters, 
# therefore some information is missing. But we deal with "RT & edit"'s.
# It is possible to extract the original tweet. Anyway, this would logically 
# have to be done in a new §
# Since all retweets begin with RT (you don't send it on the website, 
# but it's how it's saved), we omit them via "!substr(texts, 1, 2) == "RT""
# you can test this :
# texts[substr(texts, 1, 2) == "RT"]

# Let's classify tweets, RTs & "RT & edit"
classif.RT <- data.frame(	text = texts,																						# texts
							no = !grepl("RT", texts) | (grepl("RT", texts) & !grepl("RT ", texts)),								# no RT with exception for stuff like "RTS" or "CONCERT" etc.
							RT = !sapply(tw, function(x) is.null(x$retweeted_status)),											# pure RT
							plzRT = grepl("please RT|plz RT|RT please", texts),													# "please RT" 
							RTedit = grepl("MT @|RT @|MT@|RT@", texts) & sapply(tw, function(x) is.null(x$retweeted_status)), 	# contains MT or RT, is not a pure RT, is not a "please RT"
							stringsAsFactors=FALSE 																				# stringsAsFactors will save your life
)

# Number of characters in all the tweets except pure RTs
char <- sapply(classif.RT$text[classif.RT$RT == FALSE], nchar)

# It shouldn't appear here. Anyway, let's extract original text from retweets
# and compute number of characters in these foreign texts
RT.text <- sapply(tw, function(x) x$retweeted_status$text)
RT.text.null <- !sapply(RT.text, is.null)
RT.text <- do.call("c", RT.text[RT.text.null])

RT.text <- gsub("\\&gt\\;", "", RT.text)
RT.text <- gsub("\\&lt\\;", "", RT.text)
RT.text <- gsub("\\&amp", "", RT.text)

RT.char <- sapply(RT.text, nchar)

# Number of characters of edited RTs
char2 <- sapply(classif.RT$text[classif.RT$RTedit == TRUE], nchar)

# Distribution of number of characters composing each tweet except pure RTs
char.df <- as.data.frame(table(char), stringsAsFactors = FALSE)
char.df$char <- as.numeric(char.df$char)

# Distribution of number of characters composing pure RTs
RT.char.df <- as.data.frame(table(RT.char), stringsAsFactors = FALSE)
RT.char.df$char <- as.numeric(RT.char.df$RT.char)

# Distribution of number of characters composing edited RTs
char2.df <- as.data.frame(table(char2), stringsAsFactors = FALSE)
char2.df$char2 <- as.numeric(char2.df$char2)

# Let's plot on the same graph 3 distribution (see legend)
pdf("nchar.pdf")
	plot(	char.df, 
			pch = 20, 
			col = "blue", 
			xlab="Characters", 
			ylab="Tweets", 
			main = "Distribution of tweets\nvs. number of characters", 
			cex = .6)
	grid()
	points(	RT.char.df, 
			pch = 20, 
			col = "red", 
			cex = .6)
	points(	char2.df, 
			pch = 20, 
			col = "green", 
			cex = .6)
# splines help seeing the tendancy
	lines(smooth.spline(char.df$char, char.df$Freq, df = 10))
	lines(smooth.spline(char2.df$char2, char2.df$Freq, df = 10))
	lines(smooth.spline(RT.char.df$RT.char, RT.char.df$Freq, df = 10))
	legend(	"topleft", 
			pch = c(20, 20, 20), 
			col = c("blue", "red", "green"), 
			legend = c(paste("Tweets without classic RTs, n = ", sum(char.df$Freq), sep=""), paste("Retweets, n = ", sum(RT.char.df$Freq) , sep=""), paste("Edited retweets, n = ", sum(char2.df$Freq) , sep="")))
dev.off()

# Relative case
char.rel.df <- char.df
char.rel.df$Freq <- char.df$Freq / sum(char.df$Freq)

RT.char.rel.df <- RT.char.df
RT.char.rel.df$Freq <- RT.char.df$Freq / sum(RT.char.df$Freq)

char2.rel.df <- char2.df
char2.rel.df$Freq <- char2.df$Freq / sum(char2.df$Freq)

#graph
pdf("nchar_rel.pdf")
	plot(	char.rel.df, 
			pch = 20, 
			col = "blue", 
			xlab="Characters", 
			ylab="Frequency", 
			main = "Distribution of tweets\nto number of characters", 
			cex = .6, 
			ylim = c(0,max(char.rel.df$Freq, char2.rel.df$Freq, RT.char.rel.df$Freq)))
	grid()
	points(	RT.char.rel.df, 
			pch = 20, 
			col = "red", 
			cex = .6)
	points(	char2.rel.df, 
			pch = 20, 
			col = "green", 
			cex = .6)
	# splines
	lines(smooth.spline(char.rel.df$char, char.rel.df$Freq, df = 10))
	lines(smooth.spline(RT.char.rel.df$RT.char, RT.char.rel.df$Freq, df = 10))
	lines(smooth.spline(char2.rel.df$char2, char2.rel.df$Freq, df = 10))
	legend(	"topleft", 
			pch = c(20, 20, 20), 
			col = c("blue", "red", "green"), 
			legend = c(paste("Tweets without classic RTs, n = ", sum(char.df$Freq), sep=""), paste("Retweets, n = ", sum(RT.char.df$Freq) , sep=""), paste("Edited retweets, n = ", sum(char2.df$Freq) , sep="")))
dev.off()



### TO BE TRANSLATED FROM HERE



########################################################################
### Longueur des tweets de conversation (commençant par une mention) ###
########################################################################

# Ici les tweets de conversation (parfois la mention est précédée d'un point)
char <- sapply(texts[(substr(texts, 1, 1) == "@") | (substr(texts, 2, 2) == "@")], nchar)			
char.df <- as.data.frame(table(char), stringsAsFactors = FALSE)
char.df$char <- as.numeric(char.df$char)

# Longueur des tweets pas en conversation (et pas RT classique)
char2 <- sapply(texts[((!substr(texts, 1, 1) == "@") & (!substr(texts, 2, 2) == "@") & (!substr(texts, 1, 2) == "RT"))], nchar)
char2.df <- as.data.frame(table(char2), stringsAsFactors = FALSE)
char2.df$char2 <- as.numeric(char2.df$char2)

# On graphe tout ça
pdf("nchar_conversation.pdf")
plot(char.df, pch = 20, col = "blue", xlab="Nombre de caractères", ylab="Nombre de tweets", main = "Tweets en fonction du nombre de caractères\n(conversations)", cex = .6)
points(char2.df, pch = 20, col = "red", cex = .6)
lines(smooth.spline(char.df$char, char.df$Freq, df=10))
lines(smooth.spline(char2.df$char2, char2.df$Freq, df=10))
legend("topleft", pch = c(20, 20), col = c("blue", "red"), legend = c(paste("en réponse à qn, n = ", length(char), sep=""), paste("pas en réponse (ni un RT classique), n = ", length(char2), sep="")))
dev.off()

# Nombre de mots par tweet (code inspiré du site MINING TWITTER)
words_list = strsplit(texts, " ")
words_per_tweet = sapply(words_list, length)

pdf("words_per_tweet.pdf")
barplot(table(words_per_tweet), border=NA, main="Distribution des mots dans les tweets", cex.main=1, cex.names = .4, xlab = "Nombre de mots", ylab ="Nombre de tweets")
abline(h=0:10*100, col="lightgray", lty = 3)
legend("topright", bty = "n", legend =c(paste("nombre de tweets = ", length(texts), sep=""), paste("nombre de mots = ", length(unlist(words_list)), sep="")))
dev.off()

# Longueur des mots (code inspiré du site MINING TWITTER)
# wsize_per_tweet = sapply(words_list, function(x) mean(nchar(x)))

# pdf("wsize_per_tweet.pdf")
# barplot(table(round(wsize_per_tweet)), border=NA,
#   xlab = "word length in number of characters",
#   main="Distribution of words length per tweet", cex.main=1)
# abline(h=0:100*500, col="lightgray", lty = 3)
# dev.off()

# Most frequent words CODE EMPRUNTE AU SITE MINING TWITTER
# mfw = sort(table(unlist(words_list)), decreasing=TRUE)
# top20 = head(mfw, 20)
# barplot(top20, border=NA, las=2, main="Top 20 most frequent terms", cex.main=1)

############################
### Analyse des contenus ###
############################

# sentiment analysis
# work in progress !
# faudra trouver des lexiques francophones 

#################################
# Analyse des correspondances ###
#################################

# pas eu le temps !
# https://sites.google.com/site/miningtwitter/questions/talking-about/given-topic

##########################
### Analyse temporelle ###
##########################

# Watch out ! If it doesn't work from here, please read carefully the remark
# at the beginning of the "Dates" paragraph.

# Nombre de tweets par jour
h <- hist(crea, breaks = "day", freq = TRUE, xaxt = "n", plot = FALSE)

pdf(file = "tweet_day_freq.pdf", width=2000/72, height=300/72)
plot(head(h$breaks,-1), h$counts, type = "s", pch = 20, xlab="Année", ylab="Nombre de tweets", main="Nombre de tweets par jour", xaxt="n", bty="n")
lines(smooth.spline(head(h$breaks,-1), h$counts))
axis.POSIXct(1, at = seq(as.Date("2008/01/01"), as.Date("2013/01/01"), by = "year"))
abline(h=0:14 * 10, col="lightgray")
dev.off()

# Nombre de tweets par semaine
h.w <- hist(crea, breaks = "weeks", freq = TRUE, xaxt = "n", plot = FALSE)

pdf(file = "tweet_week_freq.pdf", width=1000/72, height=300/72)
plot(head(h.w$breaks,-1), h.w$counts, type = "s", pch = 20, xlab="Année", ylab="Nombre de tweets", main="Nombre de tweets par semaine", xaxt="n", bty="n")
lines(smooth.spline(head(h.w$breaks,-1), h.w$counts))
axis.POSIXct(1, at = seq(as.Date("2008/01/01"), as.Date("2013/01/01"), by = "year"))
abline(h=1:4 * 100, col="lightgray")
dev.off()

# Nombre de tweets par mois
h.m <- hist(crea, breaks = "months", freq = TRUE, xaxt = "n", plot = FALSE)

pdf(file = "tweet_month_freq.pdf", width=1000/72, height=300/72)
plot(head(h.m$breaks,-1), h.m$counts, type = "s", pch = 20, xlab="Année", ylab="Nombre de tweets", main="Nombre de tweets par mois", xaxt="n", bty="n")
lines(smooth.spline(head(h.m$breaks,-1), h.m$counts))
axis.POSIXct(1, at = seq(as.Date("2008/01/01"), as.Date("2013/01/01"), by = "year"))
abline(h=0:12 * 100, col="lightgray")
dev.off()


###################
### Répartition ###
###################

# lubridate style
# hour(crea)	l'heure du jour
# day(crea)		le jour de l'année
# wday(crea)	le jour de la semaine
# week(crea)	la semaine dans l'année
# month(crea)	le mois dans l'année
# year(crea)	l'année

# Répartition des tweets parmi les jours de la semaine #
jours <- as.data.frame(table(weekdays(crea)), stringsAsFactors = FALSE)[c(2,6,7,5,1,3,4),]$Freq
names(jours) <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")

# Le graphe
pdf("weekdays.pdf", width = 600/72, height = 400/72)
barplot(jours, col = paste("gray", (8-rank(jours)) * 10, sep=""), main = "Répartition de mes tweets parmi les jours de la semaine depuis 2008")
dev.off()

# Répartition des tweets parmi les jours de la semaine en 2011
jours <- as.data.frame(table(weekdays(crea[year(crea) == 2011])), stringsAsFactors = FALSE)[c(2,6,7,5,1,3,4),]$Freq
names(jours) <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")

# Le graphe
pdf("weekdays_2011.pdf", width = 600/72, height = 400/72)
barplot(jours, col = paste("gray", (8-rank(jours)) * 10, sep=""), main = "Répartition de mes tweets parmi les jours de la semaine en 2011")
legend("topright", legend = paste("n = ", sum(jours), sep=""), bty = "n")
dev.off()

# Répartition des tweets parmi les jours de la semaine en 2012
jours <- as.data.frame(table(weekdays(crea[year(crea) == 2012])), stringsAsFactors = FALSE)[c(2,6,7,5,1,3,4),]$Freq
names(jours) <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")

# Le graphe
pdf("weekdays_2012.pdf", width = 600/72, height = 400/72)
barplot(jours, col = paste("gray", (8-rank(jours)) * 10, sep=""), main = "Répartition de mes tweets parmi les jours de la semaine en 2012")
legend("topright", legend = paste("n = ", sum(jours), sep=""), bty = "n")
dev.off()

############################
### Heures de la semaine ###
############################

# Le nombre de tweets par heure et par jour
jour.heure <- as.matrix(table(wday(crea), hour(crea)))[c(7,1:6),]

# La palette de couleurs
pal <- alpha("red", seq(0, .9, length = max(jour.heure)+1))

# Le graphique (heatmap)
pdf("heatmap_allyears.pdf", width=10, height=5)
par(mar=c(3, 6, 3, 2) + 0.1)
image(t(jour.heure), col = pal, xaxt = "n", yaxt = "n", main = "Distribution de mes tweets dans la semaine, depuis 2008")
grid(nx=24, ny = 7, lty = 1, col ="black", lwd = .5)
axis(1, at = seq(0,23,1)/23, labels = seq(0,23,1), tick = FALSE, las = 2)
axis(2, at = rev(seq(0,1,1/6)), labels = c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche") , tick = FALSE, las = 2)
text(rep(seq(0,23,1)/23,7), sapply(seq(0,1,1/6), function(x) rep(x,24)), labels = as.vector(t(jour.heure)), cex = .5)
dev.off()

# On va procéder de même pour chaque année
for (i in 2008:2013)		{
# Le nombre de tweets par heure et par jour pour l'année i
jour.heure <- table(wday(crea[year(crea) == i]), hour(crea[year(crea) == i]))
class(jour.heure) <- "matrix"
mat <- matrix(0, ncol = 24, nrow = 7)
colnames(mat) <- 0:23
rownames(mat) <- c(1:7)

# cet articifice permet de générer la heatmap même si certaines heures ou certains jours sont manquants
jour.heure <- merge(mat, jour.heure, by="row.names", all = TRUE, suffixes = c(".x", ""))[,-1]
jour.heure[is.na(jour.heure)] <- 0
jour.heure <- jour.heure[,-(grep(".x", colnames(jour.heure)))]
jour.heure <- jour.heure[order(as.numeric(colnames(jour.heure)))]
jour.heure <- jour.heure[c(7,1:6),]

# Le graphique (heatmap)
pdf(paste("heatmap_", as.character(i), ".pdf", sep=""), width=10, height=5)
par(mar=c(3, 6, 3, 2) + 0.1)
image(t(jour.heure), col = pal, xaxt = "n", yaxt = "n", main = paste("Distribution de mes tweets dans la semaine, en ", as.character(i), " (n = ", sum(jour.heure), ")", sep=""))
grid(nx=24, ny = 7, lty = 1, col ="black", lwd = .5)
axis(1, at = seq(0,23,1)/23, labels = seq(0,23,1), tick = FALSE, las = 2)
axis(2, at = rev(seq(0,1,1/6)), labels = c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche") , tick = FALSE, las = 2)
text(rep(seq(0,23,1)/23,7), sapply(seq(0,1,1/6), function(x) rep(x,24)), labels = as.vector(t(jour.heure)), cex = .5)
dev.off()
}

################################
### Écarts entre deux tweets ###
################################

# on classe les dates
crea.temp <- crea[order(crea)]

# on se cantonne à l'année 2012
crea.temp <- crea.temp[year(crea.temp)==2012]

# on crée des intervalles de temps
crea.int <- int_diff(crea.temp)

# pour calculer leurs longueurs ensuite (en secondes)
crea.intlength <- int_length(crea.int)

# une description de l'échantillon
summary(crea.intlength)

crea.minutes <- round(crea.intlength / 60)
crea.hours <- round(crea.intlength / 60^2)
crea.days <- round(crea.intlength / (24*60^2))

######################
### Social Network ###
######################

# pas eu le temps !

# Ranking de son entourage
# Reconstruction d'un réseau "social"
