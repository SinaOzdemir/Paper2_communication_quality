#tweet pos tagger test:
#!!!! this script works best if you first create an Rproject
#!!!! in the directory you wanna work in!!!
# setup -------------------------------------------------------------------

tweets = readRDS(file.choose())
twt_texts = tweets$text


library(devtools)
devtools::install_github("b05102139/twokenizer")

library(twokenizer)

wd = paste0(getwd(),"/")

# better way of downloading the model; could become a function eventually ---------------------------------------------
#wrapper function to download the tagging engin in twokenizer doesn't work
#because of file permissions of the tagger on github. 
#However, tagger works when downloaded from https://code.google.com/archive/p/ark-tweet-nlp/downloads
#below is an alternative method that works

link = "https://storage.googleapis.com/google-code-archive-downloads/v2/code.google.com/ark-tweet-nlp/ark-tweet-nlp-0.3.2.tgz"

utils::download.file(url = link,destfile = paste0(getwd(),"/ark-tweet-nlp-0.3.2.tar.gz"))

utils::untar(tarfile = paste0(getwd(),"/ark-tweet-nlp-0.3.2.tar.gz"),exdir = wd)

tagger_location = paste0(wd,"ark-tweet-nlp-0.3.2")

#downloading other models:
utils::download.file("http://www.cs.cmu.edu/~ark/TweetNLP/model.ritter_ptb_alldata_fixed.20130723", 
                     destfile = paste0(tagger_location, "/penn_model"))

penn_model_location = paste0(tagger_location, "/penn_model")
#irc

utils::download.file("http://www.cs.cmu.edu/~ark/TweetNLP/model.irc.20121211", 
                     destfile = paste0(tagger_location, "/irc_model"))

irc_model_location =paste0(tagger_location, "/irc_model")



# test the base tagger ----------------------------------------------------


twokenizer(tagger_location = tagger_location,
           text = "ikr smh he asked fir yo last name so he can add u on fb lololol")



test_tokens= twokenizer(tagger_location = tagger_location,
                        text = twt_texts[2])

#coarse tagger is too broad with tag names, PENN and IRC would serve better


# Downloading other models ------------------------------------------------


#downloading other models
#penn tree model

utils::download.file("http://www.cs.cmu.edu/~ark/TweetNLP/model.ritter_ptb_alldata_fixed.20130723", 
                     destfile = paste0(tagger_location, "/penn_model"))
penn_model_location = paste0(tagger_location, "/penn_model")
#irc

utils::download.file("http://www.cs.cmu.edu/~ark/TweetNLP/model.irc.20121211", 
                     destfile = paste0(tagger_location, "/irc_model"))

irc_model_location =paste0(tagger_location, "/irc_model")


# testing alternative models ----------------------------------------------

penn_test = twokenizer(tagger_location = tagger_location,text = twt_texts[2],model_location = penn_model_location)

irc_test = twokenizer(tagger_location = tagger_location,text = twt_texts[2],model_location = irc_model_location)

#it all works now!