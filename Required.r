library(readr)
library(stringr)
library(janeaustenr)
library(devtools)
library(janeaustenr)
library(syuzhet)
library(dplyr)
library(reshape2)
library(ggplot2)
library(ggthemes)


#Reading the Novel's text file as raw character vectors
rawPandP <- read_lines("http://www.gutenberg.org/cache/epub/1342/pg1342.txt",skip=30, n_max = 13032)
head(rawPandP)

#Splitting raw text into paragraphs
PandP <- character()
for (i in seq_along(rawPandP)) {
  if (i%%10 ==1) PandP[ceiling(i/10)] <- str_c(rawPandP[i],
                                               rawPandP[i+1],
                                               rawPandP[i+2],
                                               rawPandP[i+3],
                                               rawPandP[i+4],
                                               rawPandP[i+5],
                                               rawPandP[i+6],
                                               rawPandP[i+7],
                                               rawPandP[i+8],
                                               rawPandP[i+9],sep=""
                                               )
}

#Adding line number and getting the sentiment for the text
PandPnrc <- cbind(linenumber= seq_along(PandP), get_nrc_sentiment(PandP))
head(PandPnrc)

#Finding linenumber of volume breaks
grep("Chapter 1", PandP)
grep("Chapter 24", PandP)
grep("Chapter 43", PandP)

#Creating volumes and setting linenumbers 
PandPnrc$volume <- "Volume I"
PandPnrc[grep("Chapter 24", PandP):length(PandP),'volume'] <- "Volume II"
PandPnrc[grep("Chapter 43", PandP):length(PandP),'volume'] <- "Volume III"
PandPnrc$volume <- as.factor(PandPnrc$volume)
PandPnrc$linenumber[PandPnrc$volume == "Volume II"] <- seq_along(PandP)
PandPnrc$linenumber[PandPnrc$volume == "Volume III"] <- seq_along(PandP)#not clear


#Exploring Positive and negative sentiments
PandPnrc$negative <- -PandPnrc$negative
posneg <- PandPnrc %>% select(linenumber,volume,positive,negative) %>%
          melt(id = c("linenumber","volume"))
names(posneg) <- c("linenumber","volume","sentiment","value")



#annotating the iportant events in the plot by matching the volume and linenumber
annotatetext <- data.frame(x = c(114, 211, 307, 183, 91, 415), y = rep(18.3, 6), 
                           label = c("Jane's illness", "Mr. Collins arrives", 
                                         "Ball at Netherfield", "Mr. Darcy proposes", 
                                         "Lydia elopes", "Mr. Darcy proposes again"),
                                         volume = factor(c("Volume I", "Volume I", 
                                         "Volume I", "Volume II", 
                                         "Volume III", "Volume III"),
                                         levels = c("Volume I", "Volume II", "Volume III")))


annotatearrow <- data.frame(x =c(114, 211, 307, 183, 91, 415), 
                            y1 = rep(17, 6), y2 = c(11.2, 10.7, 11.4, 13.5, 10.5, 11.5),
                            volume = factor(c("Volume I", "Volume I", 
                                              "Volume I", "Volume II", 
                                              "Volume III", "Volume III"),
                                            levels = c("Volume I", "Volume II", "Volume III")))

#plotting positive and negative sentiments
        ggplot(data = posneg, aes(x = linenumber, y = value, color = sentiment)) +
        facet_wrap(~volume, nrow = 3)+
        geom_point(size = 4, alpha = 0.5) + theme_minimal() +
        ylab("Sentient") +
        ggtitle(expression(paste("Positive and Negative Sentiment in ", 
                                 italic("Pride and Prejudice")))) +
        theme(legend.title=element_blank()) +
        theme(axis.title.x=element_blank()) +
        theme(axis.ticks.x=element_blank()) +
        theme(axis.text.x=element_blank()) +
        geom_text(data = annotatetext, aes(x,y,label=label), hjust = 0.5,
                  size = 3, inherit.aes = FALSE) +
        geom_segment(data = annotatearrow, aes(x= x, y = y1, xend = x, yend = y2),
                     arrow = arrow(length = unit(0.05, "npc")), inherit.aes = FALSE) +
        theme(legend.justification=c(1,1), legend.position=c(1, 0.71)) +
        scale_color_manual(values = c("aquamarine3", "midnightblue"))
        
        
#Visualizing the positive and negative emotions with bar charts
        ggplot(data = posneg, aes(x = linenumber, y = value , color = sentiment,fill = sentiment)) +
        facet_wrap(~volume, nrow = 3)+
        geom_bar(stat = "identity", position = "dodge") + theme_minimal() +
        ylab("Sentiment") +
        ggtitle(expression(paste("Positive and Negative Sentiment in ",
                                 italic("Pride and prejudice")))) +
        theme(legend.title=element_blank()) +
        theme(axis.title.x=element_blank()) +
        theme(axis.text.x=element_blank()) +
        theme(axis.ticks.x=element_blank()) +
          theme(legend.justification=c(1,1), legend.position=c(1, 0.71))+
        geom_text(data = annotatetext,aes(x,y,label=label), hjust = 0.5,
        size = 3,inherit.aes = FALSE) +
        geom_segment(data = annotatearrow, aes(x =x, y = y1, xend = x, yend = y2),
                     arrow = arrow(length = unit(0.05, "npc")),inherit.aes = FALSE) +
        scale_fill_manual(values = c("aquamarine3", "midnightblue")) +
        scale_color_manual(values = c("aquamarine3", "midnightblue"))
        

#Visualizaing the overall sentiments in each phase 
        
        PandPsentiment <- data.frame(cbind(linenumber = seq_along(PandP), 
                                           sentiment = get_sentiment(PandP, method = "nrc")))
        PandPsentiment$volume <- "Volume I"
        PandPsentiment[grep("Chapter 24", PandP):length(PandP),'volume'] <- "Volume II"
        PandPsentiment[grep("Chapter 43", PandP):length(PandP),'volume'] <- "Volume III"
        PandPsentiment$volume <- as.factor(PandPsentiment$volume)
        PandPsentiment$linenumber[PandPsentiment$volume == "Volume II"] <- seq_along(PandP)
        PandPsentiment$linenumber[PandPsentiment$volume == "Volume III"] <- seq_along(PandP)
        
ggplot(data = PandPsentiment, aes(x = linenumber, y = sentiment)) +
  geom_bar(stat = "identity",position = "dodge", color = "midnight blue") +
  theme_minimal() +
  facet_wrap(~volume, nrow = 3) +
  ylab("Sentiment") +
  ggtitle(expression(paste("Sentiment in ",italic("Pride and Prejudice")))) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  theme(legend.justification = c(1,1), legend.position = c(1,0.71)) +
  geom_text(data = annotatetext, aes(x = x, y = y, label = label), hjust = 0.5,
            size = 3, inherit.aes = FALSE) +
  geom_segment(data = annotatearrow, aes(x = x, y = y1, xend = x, yend = y2),
               arrow = arrow(length = unit(0.05 ,"npc")))
  
#Applying Fourier transform 
PandPft <- as.numeric(get_transformed_values(PandPsentiment$sentiment,low_pass_size = 3, 
                      scale_vals = TRUE,
                      scale_range = FALSE))
PandPft <- data.frame(cbind(linenumber = seq_along(PandPft), ft = PandPft))


#Ploting the transformed graph
ggplot(data = PandPft ,aes(x =linenumber, y = ft)) +
  geom_bar(stat = "identity", alpha = 0.5, color ="midnightblue", fill = "midnightblue") +
  theme_minimal() +
  ylab("Transformed Sentiment Value") +
  ggtitle(expression(paste("Sentiment in", italic("Pride and Prejudice")))) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  annotate("text", size = 3, x = c(9, 23, 49, 69, 94),
           y = c(-0.2, -0.5, 0.5, 0.2, 0.7),
           label = c("Jane's illness", "Ball at Netherfield",
                           "Mr. Darcy proposes", "Lydia elopes", 
                           "Mr. Darcy proposes again")) +
  annotate("segment", arrow = arrow(length = unit(0.03, "npc")),
           x = c(9, 23, 49, 69, 94), xend = c(9, 23, 49, 69, 94),
           y = c(-0.16, -0.46, 0.44, 0.15, 0.64), 
           yend = c(-0.02, -0.02 , 0.02, 0.02, 0.2))
