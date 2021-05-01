tpos_download = function(exploc, type = c("base","irc","penn")){
  
  if (type == "base"){
    message("downloading base tagger")
    tagger_url = "https://storage.googleapis.com/google-code-archive-downloads/v2/code.google.com/ark-tweet-nlp/ark-tweet-nlp-0.3.2.tgz"
    utils::download.file(tagger_url, dir = exploc)
    utils::untar(tarfile = paste0(exploc,"/ark-tweet-nlp-0.3.2.tar.gz", exdir = exploc))
    utils::remove.file(paste0(exploc,"/ark-tweet-nlp-0.3.2.tar.gz"))
    tagger_location = paste0(exploc,"/ark-tweet-nlp-0.3.2")
    message(paste("the Twitter POS tagger is downloaded and exported to",tagger_location,"Use this location to "))
  }
  if (type == "penn"){
    if(isFALSE(dir.exists(paths = tagger_location))){
      stop("Please download the 'base' first!")
    }else{
      penn_location = paste0(tagger_location,"/penn_model")
      penn_link = ""
      utils::download.file(url = penn_link, destfile = penn_location)
      message(paste("Penn TREESTYLE POS model is downloaded.","Please reference the following location in twokenizer's model_location argument"),
              penn_location, sep="\n")
    }
  if(type == "irc"){
    if(isFALSE(dir.exists(paths = tagger_location))){
      stop("Please install the base model first!")
    }else{
      irc_location = paste0(tagger_location,"/irc_model")
      irc_link = ""
      utils::download.file(url = irc_link, destfile = irc_location)
      message(paste("IRC POS model is downloaded.","Please reference the following location in twokenizer's model_location argument"),
              irc_location, sep="\n")
    }
  }
  }
}