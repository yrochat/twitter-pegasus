# Titre 		:		tl_analysis.R
# Auteur		:		Yannick Rochat, (DHLAB) EPFL, (IMA) UNIL
# Date  		: 		Mars 2013
# Description 	:		Décrire et analyser les tweets contenus dans son archive
# License		:		GNU General Public License

# on nettoie l'environnement de travail
rm(list=ls())

# définir ici le dossier de travail (il est possible de faire un glisser-déposer)
wd <- "~/Dropbox/Pegasus/2013_TwitterDataRetrieval"

# on va d'abord charger les tweets là où ils sont
setwd(paste(wd, "/tweets/data/js/tweets", sep=""))

###############################
### chargement des packages ###
###############################

# Après avoir exécuté la ligne ci-dessous vous n'en aurez plus jamais besoin
install.packages(c("rjson", "wordcloud", "tm", "lubridate", "scales"))
# Alors autant l'effacer :-)

library(rjson)
library(wordcloud)
library(tm)
library(lubridate)
library(scales)

# si le chargement ne fonctionne pas, vous pouvez installer le package via la commande install.packages
# ici par exemple : install.packages("wordcloud")

#############################
### Chargement des tweets ###
#############################

# lister tous les fichiers .js
lf <- list.files()

# lire ces fichiers : ignorer les warnings, qui sont causés par 
# l'absence d'un retour à la ligne à la fin de chaque document
tw <- sapply(lf, readLines)

# retirer les noms des fichiers chargés au passage
names(tw) <- NULL

# enlever la première ligne parasite
tw <- sapply(tw, function(x) x[-1])

# concaténer en une seule entrée
tw <- sapply(tw, paste, collapse=" ")

# convertir de JSON en une liste dans R
tw <- lapply(tw, fromJSON)			

# on supprime la classification par mois pour avoir une liste unique				
tw <- unlist(tw, recursive = FALSE)

# ATTENTION !!!
# ceci est en tout cas valable sur un mac avec le système 10.6.8 et R 2.15.3
# si la commande strptime ne fonctionne pas, utiliser la commande suivante
# pour modifier l'environnement et le rendre compatible avec le format de dates de Twitter :
# Sys.setlocale("LC_TIME", "C")						

#############
### Dates ###
#############

# extraire les dates
crea <- sapply(tw, function(x) x$created_at)

# convertir de characters à POSIXt
crea <- strptime(crea, "%A %b %d %H:%M:%S %z %Y")

# exemple
# Thu Jan 31 23:27:08 +0000 2013
# %A  %b  %d %H:%M:%S +%z   %Y

# on retourne bosser dans notre dossier de travail et pas au fond des fichiers contenant les tweets
setwd(wd)

##########################
### ANALYSE ET VISUELS ###
##########################

# Tous les tweets, retweets en meta-données sont chargés dans tw. Aperçu :
head(tw)

# Nombre de tweets
length(tw)

# Nombre de tweets par jour, en prenant la date la plus vieille et la date la plus récente
difft <- as.numeric(tail(crea,1) - head(crea,1))				
length(tw) / difft

################
### Retweets ###
################

# Nombre de RTs : retourne le tweet si c'est un retweet, et list() si c'est pas le cas
RT <- lapply(tw, function(x) x$retweeted_status)

# TRUE si c'est un RT, FALSE sinon
RT.status <- !sapply(RT, is.null)			

# Un aperçu sur les 1000 premiers tweets
head(RT.status,100)

# Le nombre de TRUE's, c'est-à-dire le nombre de retweets
sum(RT.status)

# Proportion de RTs
sum(RT.status) / length(RT.status)

# Qui je RT
RT.id <- sapply(tw[RT.status], function(x) x$retweeted_status$user$screen_name)

# La fréquence de chaque auteur retweeté
RT.id.df <- as.data.frame(table(RT.id), stringsAsFactors = FALSE)
head(RT.id.df, 10)

# On les réordonne pour créer le graphe
RT.Freq <- RT.id.df$Freq
names(RT.Freq) <- RT.id.df$RT.id
tail(RT.Freq[order(RT.Freq)], 40)

# On représente ici les sites plus cités (sic!)
pdf("RT_people.pdf", width=1200/72, height=1200/72)
par(mar=c(5,12,4,2))
barplot(RT.Freq[tail(order(RT.Freq), 39)], horiz = TRUE, cex.names = 1, las = 2, main = "Noms des utilisateurs retweetés", cex.main = 3, cex.axis = 2)
abline(v=1:20*10, lty = 3)
dev.off()


################
### Mentions ###
################

# Rappel : en travaillant sur tw, on travaille sur tweets et retweets

# Nombre de mentions contenues dans chaque tweet
mentions <- sapply(tw, function(x) length(x$entities$user_mentions))
head(mentions,100)

# La distribution des mentions
mentions.df <- as.data.frame(table(mentions), stringsAsFactors = FALSE)
mentions.df

# Tous les IDs (on retire les retweets avec intersect !)
mentions.id <- unlist(sapply(tw[intersect(which(!RT.status), which(mentions != 0))], function(x) sapply(x$entities$user_mentions, function(y) y$id)))
head(mentions.id,100)

# La fréquence de chaque ID
mentions.id.df <- as.data.frame(table(mentions.id), stringsAsFactors = FALSE)
head(mentions.id.df, 10)

# Nombre de mentions par tweet
pdf("mentions_par_tweet.pdf")
plot(mentions.df, pch=20, log="y", xlab="Mentions", ylab="Tweets", main = "Nombre de mentions par tweet")
legend("topright", bty = "n", legend = paste(sum(mentions.df[-1,]$Freq), " tweets contiennent\nau moins une mention", sep=""))
grid()
dev.off()

# Proportion de tweets avec au moins une mention
sum(mentions.df[-1,]$Freq / length(tw))

# Le tweet avec le maximum de mentions
tw[[which.max(mentions)]]$text

# Nombre de mentions uniques
length(unique(mentions.id))

# Proportion de mentions uniques
length(unique(mentions.id)) / length(mentions.id)

# Nombre de mentions uniques par nombre de tweets
length(unique(mentions.id)) / length(tw)

# Distribution des mentions
mentions.id.hist <- hist(mentions.id.df$Freq, breaks = seq(min(mentions.id.df$Freq), max(mentions.id.df$Freq)+1, 1), plot = FALSE)
pdf("mentions_utilisateurs.pdf")
plot(head(mentions.id.hist$breaks, -1), mentions.id.hist$counts, pch = 20, xlab = "Mentions", ylab = "Utilisateurs", main = "Nombre de mentions des utilisateurs", log="xy")
grid()
legend("topright", bty = "n", legend = paste("n = ", sum(mentions.id.df$Freq), sep=""))
dev.off()

# En faisant le même exercice avec les noms plutôt que les IDs, on arrive aux mêmes résultats
# Bizarrement, les comptes dont le nom d'utilisateur

# Extraction des user names
mentions.screen_name <- unlist(sapply(tw[intersect(which(!RT.status), which(mentions != 0))], function(x) sapply(x$entities$user_mentions, function(y) y$screen_name)))

# Fréquence de chaque user name
mentions.screen_name.df <- as.data.frame(table(mentions.screen_name), stringsAsFactors = FALSE)

# Classement par ordre croissant
tail(mentions.screen_name.df[order(mentions.screen_name.df$Freq),], 20)

# Attention ! Ça donne le même résultat que 
# mentions.id.df[order(mentions.id.df$Freq),]
# ce qui prouve que Twitter n'aurait pas mis à jour les comptes aux noms modifiés !?!? Bizarre … À qui appartiennent ces IDs alors ?

################
### Hashtags ###
################

# Rappel : en travaillant sur tw, on travaille sur tweets et retweets

# Nombre de hashtags par tweet
hashtags <- sapply(tw, function(x) length(x$entities$hashtags))
head(hashtags, 100)

# Distribution du nombre de hashtags par tweet
hashtags.df <- as.data.frame(table(hashtags), stringsAsFactors = FALSE)

# Récupération des hashtags
hashtags.id <- unlist(sapply(tw[which(hashtags != 0)], function(x) sapply(x$entities$hashtags, function(y) y$text)))
head(hashtags.id, 100)

# Fréquence de chaque hashtag
hashtags.id.df <- as.data.frame(table(tolower(hashtags.id)), stringsAsFactors = FALSE)
head(hashtags.id.df, 100)

# Nombre total de hashtags
length(hashtags.id)

# Nombre de hashtags uniques
length(tolower(unique(hashtags.id)))

# Nombre de hashtags par tweet
pdf("hashtags_par_tweet.pdf")
plot(hashtags.df, pch=20, log="xy", xlab="Hashtags", ylab="Tweets", main = "Nombre de hashtags par tweet")
grid()
legend("topright", bty = "n", legend = c(
	paste("#hashtags = ", sum(as.numeric(hashtags.df[,1]) * hashtags.df[,2]), "\n", sep=""), 
	paste("#tweets = ", sum(hashtags.df[-1,2]), "\n", sep=""),
	paste(hashtags.df[1,2], " tweets ne contiennent\npas de hashtag", sep="")))
dev.off()

# Le tweet avec le maximum de hashtags
tw[[which.max(hashtags)]]$text

# Distribution des hashtags
hashtags.id.hist <- hist(hashtags.id.df$Freq, breaks = seq(min(hashtags.id.df$Freq), max(hashtags.id.df$Freq)+1, 1), plot = FALSE)
pdf("hashtags_dist.pdf")
plot(head(hashtags.id.hist$breaks, -1), hashtags.id.hist$counts, pch = 20, ylab = "Nombre de hashtags", xlab = "Nombre d'apparitions", main = "Nombre d'apparitions de chaque hashtag", log="xy")
grid()
legend("topright", bty = "n", legend = c(
	paste("#hashtags uniques = ", nrow(hashtags.id.df), "\n", sep=""), 
	paste("Nombre total de tweets = ", length(tw), sep="")))
dev.off()

# Distribution des hashtags avec le détail
tail(hashtags.id.df[order(hashtags.id.df$Freq),], 20)

# Wordcloud pour visualiser la distribution des hashtags
# scale détermine les tailles du plus grand et du plus petit
# min.freq est un seuil à atteindre pour pouvoir apparaître dans le wordcloud
pdf("wordcloud.pdf")
wordcloud(hashtags.id, scale=c(4,1.5), min.freq = 10)
dev.off()

############
### URLs ###
############

# Nombre d'URLs
urls <- sapply(tw, function(x) length(x$entities$urls))
urls.df <- as.data.frame(table(urls), stringsAsFactors = FALSE)
urls.df

# Proportions de tweets avec des URLs
sum(urls.df$Freq[-1]) / sum(urls.df$Freq)

urls.id <- unlist(sapply(tw[which(urls != 0)], function(x) sapply(x$entities$urls, function(y) y$expanded_url)))

# fonction donnée dans example(grep)
URL_parts <- function(x) {
     m <- regexec("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)", x)
     parts <- do.call(rbind,
                      lapply(regmatches(x, m), `[`, c(3L, 4L, 6L, 7L)))
     colnames(parts) <- c("protocol","host","port","path")
     parts
}

# Les sites que je mentionne ou retweete
websites <- as.data.frame(URL_parts(urls.id), stringsAsFactors = FALSE)$host
websites.df <- as.data.frame(table(websites), stringsAsFactors = FALSE)
websites.Freq <- websites.df$Freq
names(websites.Freq) <- websites.df$websites
tail(websites.Freq[order(websites.Freq)], 40)

# On représente ici les sites plus cités (sic!)
pdf("sites_web.pdf", width=1200/72, height=1200/72)
par(mar=c(5,12,4,2))
barplot(websites.Freq[tail(order(websites.Freq), 40)], horiz = TRUE, cex.names = 1, las = 2, main = "Noms de domaine des URLs cités", cex.main = 3, cex.axis = 2)
abline(v=1:10*50, lty = 3)
dev.off()

###########################
### Longueur des tweets ###
###########################

# DEBUG : char[which.max(char)]
texts <- sapply(tw, function(x) x$text)
head(texts)

# Il faut enlever quelques caractères problématiques
texts <- gsub("\\&gt\\;", "", texts)
texts <- gsub("\\&lt\\;", "", texts)
texts <- gsub("\\&amp", "", texts)

# IL NE FAUT PAS PRENDRE LES RTs PARCE QU'ILS SONT RACCOURCIS A 140 CARACTERES !!!

# Le nombre de caractères de tous les tweets sauf les purs RT
char <- sapply(texts[!substr(texts, 1, 2) == "RT"], nchar)
head(char)

# Distribution du nombre de caractères
char.df <- as.data.frame(table(char), stringsAsFactors = FALSE)
char.df$char <- as.numeric(char.df$char)

# Le nombre de caractères de tous les tweets sauf les purs RT et les RTs manuels 
# (et peut-être quelques tweets malheureux du type "please RT")
char2 <- sapply(texts[-grep("RT ",texts)], nchar)
head(char2)

# Distribution du nombre de caractères
char2.df <- as.data.frame(table(char2), stringsAsFactors = FALSE)
char2.df$char2 <- as.numeric(char2.df$char2)

# Le nombre de caractères de tous les RTs manuels (on exclut ceux qui commencent par "RT")
# (et peut-être quelques tweets malheureux du type "please RT" par contre attention à RTS => "RT ")
char3 <- sapply(texts[intersect(which(!substr(texts,1,2)=="RT"), grep("RT ", texts))], nchar)
head(char3)

# Distribution du nombre de caractères
char3.df <- as.data.frame(table(char3), stringsAsFactors = FALSE)
char3.df$char3 <- as.numeric(char3.df$char3)
png("nchar.png")
# On plot les 3 cas dans un même graphe
pdf("nchar.pdf")
plot(char.df, pch = 20, col = "blue", xlab="Nombre de caractères", ylab="Nombre de tweets", main = "Nombre de tweets (sans RTs classiques)\nen fonction du nombre de caractères", cex = .6)
grid()
points(char2.df, pch = 20, col = "red", cex = .6)
points(char3.df, pch = 20, col = "green", cex = .6)
# on rajoute des splines pour saisir la tendance dans les nuages de points
lines(smooth.spline(char.df$char, char.df$Freq, df = 10))
lines(smooth.spline(char2.df$char2, char2.df$Freq, df = 10))
lines(smooth.spline(char3.df$char3, char3.df$Freq, df = 10))
legend("topleft", pch = c(20, 20, 20), col = c("blue", "red", "green"), legend = c(paste("avec RTs manuels, n = ", sum(char.df$Freq), sep=""), paste("sans RTs manuels, n = ", sum(char2.df$Freq) , sep=""), paste("juste les RTs manuels, n = ", sum(char3.df$Freq) , sep="")))
dev.off()

# On plot les 3 cas dans un même graphe
# Mais cette fois c'est relatif !

char.rel.df <- char.df
char2.rel.df <- char2.df
char3.rel.df <- char3.df
char.rel.df$Freq <- char.df$Freq / sum(char.df$Freq)
char2.rel.df$Freq <- char2.df$Freq / sum(char2.df$Freq)
char3.rel.df$Freq <- char3.df$Freq / sum(char3.df$Freq)

pdf("nchar_rel.pdf")
plot(char.rel.df, pch = 20, col = "blue", xlab="Nombre de caractères", ylab="Proportion de tweets", main = "Distribution du nombre de tweets (sans RTs classiques)\nen fonction du nombre de caractères", cex = .6, ylim = c(0,max(char3.rel.df$Freq)))
grid()
points(char2.rel.df, pch = 20, col = "red", cex = .6)
points(char3.rel.df, pch = 20, col = "green", cex = .6)
# on rajoute des splines pour saisir la tendance dans les nuages de points
lines(smooth.spline(char.rel.df$char, char.rel.df$Freq, df = 10))
lines(smooth.spline(char2.rel.df$char2, char2.rel.df$Freq, df = 10))
lines(smooth.spline(char3.rel.df$char3, char3.rel.df$Freq, df = 10))
legend("topleft", pch = c(20, 20, 20), col = c("blue", "red", "green"), legend = c(paste("avec RTs manuels, n = ", sum(char.df$Freq), sep=""), paste("sans RTs manuels, n = ", sum(char2.df$Freq) , sep=""), paste("juste les RTs manuels, n = ", sum(char3.df$Freq) , sep="")))
dev.off()


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
jour.heure <- as.matrix(table(wday(crea), hour(crea)))[c(1,7:2),]

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

# work in progress

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
