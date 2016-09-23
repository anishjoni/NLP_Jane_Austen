library(readr)
library(stringr)
library(janeaustenr)
library(devtools)
library(janeaustenr)
library(syuzhet)

#Reading the Jane austen text file as raw character vectors
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

#Adding line number and 