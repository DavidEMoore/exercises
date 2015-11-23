library('XML')
library('RCurl')
library('plyr')
library('ggplot2')
library('gridExtra')
library('ggplot2')
library('rjson')
library('lsa')
library('tm')
library('wordcloud')

setwd('Git_exercise02/exercise02/data')
avi <- xmlTreeParse('AviationData.xml')
class(avi)

#Look at root for xml file
avi.root <- xmlRoot(avi)
class(avi.root)

#Name, size of root
xmlName(avi.root) #Data
xmlSize(avi.root[[1]]) #77,257 events
xmlName(avi.root[[1]]) #Rows

#Initial description of data
#xmlSApply(avi.root[[1]],xmlName)
#xmlSApply(avi.root[[1]],xmlAttrs)
#xmlSApply(avi.root[[1]],xmlSize)

#Look at first subnode
avi.root[[1]][1][1]
#Look at 10 subnodes
for (i in 1:10){
  print(avi.root[[1]][i][1])
}

#Convert to data frame
df.avi <- ldply(xmlToList(avi))
v.avi <- as.vector(df.avi[,1])
m.avi <- matrix(v.avi, ncol=31, byrow=TRUE)
df.avi <- as.data.frame(m.avi)  #organized as a data frame
for(i in 1:ncol(df.avi)){
  df.avi[,i] <- as.character(df.avi[,i]) #everything begins as factor, change to character
}
colnames(df.avi) <- c('EventId','InvestigationType','AccidentNum','EventDate','Location','Country',
                      'Lat','Long','AirportCode','AirportName','InjurySeverity','AircraftDamage',
                      'AircraftCat','RegistrationNum','Make','Model','AmateurBuild','NumEngines',
                      'EngineType','FARDescription','Schedule','PurposeOfFlight','AirCarrier',
                      'TotalFatalInjuries','TotalSeriousInjuries','TotalMinorInjuries','TotalUninjured',
                      'WeatherCondition','BroadPhaseOfFlight','ReportStatus','PubDate') #31 columns

#Exploration
df.avi$Location <- as.factor(df.avi$Location) #change to factors
location.freq <- count(df.avi$Location) #find the frequency of occurences in locations
top.locals <- location.freq[which(location.freq$freq > 100),] #find the most frequent locations

df.avi$InvestigationType <- as.factor(df.avi$InvestigationType)
investigation.type <- count(df.avi$InvestigationType)

df.avi$AirportName <- as.factor(df.avi$AirportName)
airport.name <- count(df.avi$AirportName)
top.airport <- airport.name[which(airport.name$freq > 50),]
top.airport <- top.airport[-c(1,5,6,7,13),]
den.wo.nas <- nrow(df.avi)-31429  #number of rows excluding nas/unavailables/etc.
prcnt.private <- (top.airport$freq[which(top.airport$x=='Private')] + 
                      top.airport$freq[which(top.airport$x=='PRIVATE')] + 
                      top.airport$freq[which(top.airport$x=='Private Airstrip')] + 
                      top.airport$freq[which(top.airport$x=='PRIVATE AIRSTRIP')])/den.wo.nas

df.avi$InjurySeverity <- as.factor(df.avi$InjurySeverity)
injury.type <- count(df.avi$InjurySeverity)
#get a frequency of crashes without fatalities
injury.type$freq <- as.numeric(injury.type$freq)
prcnt.non.fatal <- (injury.type$freq[which(injury.type$x=='Unavailable')] + 
   injury.type$freq[which(injury.type$x=='Incident')] + 
   injury.type$freq[which(injury.type$x=='Non-Fatal')])/nrow(df.avi)

df.avi$Make <- as.factor(df.avi$Make) #what makes have the most incidents
make.freq <- count(df.avi$Make)
top.make <- make.freq[which(make.freq$freq > 1000),]  #makes with the most issues
top.make$freq <- as.numeric(top.make$freq)
prcnt.cessna <- (top.make$freq[which(top.make$x=='Cessna')] + 
                   top.make$freq[which(top.make$x=='CESSNA')])/nrow(df.avi)
prcnt.piper <- (top.make$freq[which(top.make$x=='Piper')] + 
                   top.make$freq[which(top.make$x=='PIPER')])/nrow(df.avi)
prcnt.beech <- (top.make$freq[which(top.make$x=='Beech')] + 
                  top.make$freq[which(top.make$x=='BEECH')])/nrow(df.avi)

df.avi$Model <- as.factor(df.avi$Model) #what models have the most incidents
model.freq <- count(df.avi$Model)
top.model <- model.freq[which(model.freq$freq > 1000),]  #models with the most issues
top.model$freq <- as.numeric(top.model$freq)

#make and model combined
make.model <- count(df.avi[,15:16])
top.make.model <- make.model[which(make.model$freq > 300),]

#Anchorage
inxs.anc <- which(df.avi$Location=='Anchorage, AK' | df.avi$Location=='ANCHORAGE, AK')
df.anc <- df.avi[inxs.anc,c(2,5,11,15,16)]
df.anc$Make <- as.factor(df.anc$Make) #what makes have the most incidents
make.freq.anc <- count(df.anc$Make)
top.make.anc <- make.freq.anc[which(make.freq.anc$freq > 40),]  #makes with the most issues
top.make.anc$freq <- as.numeric(top.make.anc$freq)
prcnt.cessna.anc <- (top.make.anc$freq[which(top.make.anc$x=='Cessna')] + 
                   top.make.anc$freq[which(top.make.anc$x=='CESSNA')])/nrow(df.anc)
prcnt.piper.anc <- (top.make.anc$freq[which(top.make.anc$x=='Piper')] + 
                  top.make.anc$freq[which(top.make.anc$x=='PIPER')])/nrow(df.anc)
df.anc$InjurySeverity <- as.factor(df.anc$InjurySeverity)
injury.type.anc <- count(df.anc$InjurySeverity)
#get a frequency of crashes without fatalities
injury.type.anc$freq <- as.numeric(injury.type.anc$freq)
prcnt.non.fatal.anc <- (injury.type.anc$freq[which(injury.type.anc$x=='Incident')] + 
                      injury.type.anc$freq[which(injury.type.anc$x=='Non-Fatal')])/nrow(df.anc)
df.anc$InvestigationType <- as.factor(df.anc$InvestigationType)
investigation.type.anc <- count(df.anc$InvestigationType)



#histogram of make.model
p <- ggplot(top.make.model,aes(freq))+geom_density(aes(fill=factor(Make)), size=2)
#geom_point(aes(color=factor(Make)),size=4)
p

df.avi$WeatherCondition <- as.factor(df.avi$WeatherCondition) #weather condition
weather.freq <- count(df.avi$WeatherCondition)
top.airport <- airport.name[which(airport.name$freq > 50),]

#json data
json.file <- 'json_tmp/NarrativeData_000.json'
json.data <- fromJSON(file=json.file)
cause <- list()
for(i in 1:length(json.data[[1]])){
  cause[i] <- json.data[[1]][[i]][3]
}

dtm <- textmatrix('json_tmp/',minWordLength=3) #create word matrix
test.space <- lsa(dtm,dims=dimcalc_raw()) #create space
X <- as.textmatrix(test.space) #create X text matrix
all((round(X,2)==dtm) == TRUE) #remove rounding errors
#dimensionality reduction
new.space <- lsa(dtm, dims=2)
Y <- as.textmatrix(new.space)
round(Y,2) #round to two decimal places
#read in the sample again
pdocs <- textmatrix('json_tmp/',vocabulary=rownames(dtm))
Y2 <- fold_in(pdocs,new.space)
all((round(Y,2)==round(Y2,2))==TRUE)
rawCor <- cor(dtm)
lsaCor <- cor(Y)
round(rawCor,2)

#tm package
test.tm <- 'json_tmp/'
corp <- VCorpus(DirSource(test.tm))
#inspect(corp)
#meta(corp[[1]])
#writeLines(as.character(corp[[1]]))
#new.corp <- lapply(corp[1:length(corp)],as.character)

#transform corpus
corp <- tm_map(corp, stripWhitespace) #strip whitespace
corp <- tm_map(corp, content_transformer(tolower))  #lowercase only
corp <- tm_map(corp, removeWords, stopwords('english')) #remove stop words
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
#corp <- tm_map(corp, stemDocument)
dtm <- DocumentTermMatrix(corp)
findFreqTerms(dtm,11000)
#findAssocs(dtm, 'engine', 0.99)
inspect(DocumentTermMatrix(corp,list(dictionary=c('engine','landing','power'))))
dtm.new <- DocumentTermMatrix(corp,list(dictionary=findFreqTerms(dtm,10999)))
dtm.m <- as.matrix(dtm.new)
#make word cloud
dtm.m <- t(dtm.m)
dtm.v <- sort(rowSums(dtm.m),decreasing=TRUE)
dtm.d <- data.frame(word = names(dtm.v),freq=dtm.v)
table(dtm.d$freq)
pal2 <- brewer.pal(8,"Dark2")
pdf("wordcloud_packages.pdf", width=40,height=32)
wordcloud(dtm.d$word,dtm.d$freq, scale=c(18,4),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()
# pdf('test.pdf',width=7,height=5)
# comparison.cloud(dtm.m,scale=c(1,.1),max.words=49,random.order=FALSE,title.size=1)
dev.off()
