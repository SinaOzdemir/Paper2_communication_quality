#interactivity indicators

# setup -------------------------------------------------------------------
source(file = file.path(getwd(),"Scripts","Auxilary_scripts","two_way_interaction_analysis_functions.R"))
data.path<-file.path(getwd(),"data")
api_v1_token <- rtweet::create_token(app = "Functionised_collector",
                                     consumer_key = "c0wODdFRKzNDz23l7O9A6GBig",
                                     consumer_secret = "uwraekdxEX2BaVZSNLw7sjkVbq3wNRByYYiOolHrYaikqZHKUW",
                                     access_token = "1151438784384421888-eas7PxAdeoohUCykI5XeX2Wc2lppjl",
                                     access_secret = "96TaBIVuZXlpICK2QAP7IurCFuYPfHSUbXYUTTSRak3gr",
                                     set_renv = F)

case_data<-data_reader(file_path = data.path,case = "EU")


# data cleaning -----------------------------------------------------------

case_data_clean<- data_cleaner(data = case_data)

# descriptive interactivity -----------------------------------------------

eu_scatter_plot_reply<- descriptive_plots(data = case_data_clean,
                                    case = "the EU",
                                    what = "reply",
                                    unit = "year",
                                    graph_type = "scatter")

eu_scatter_plot_quote<- descriptive_plots(data = case_data_clean,
                                          case = "the EU",
                                          what = "quote",
                                          unit = "year",
                                          graph_type = "scatter")

eu_scatter_plot_retweet<- descriptive_plots(data = case_data_clean,
                                          case = "the EU",
                                          what = "retweet",
                                          unit = "year",
                                          graph_type = "scatter")


(eu_scatter_plot_reply + plot_spacer() + eu_scatter_plot_quote) / (plot_spacer() + eu_scatter_plot_retweet + plot_spacer())

# network graph of interactivity ------------------------------------------------

eu_reply_graph<- network_grapher(data = test_data,case = "the EU", v1_api = api_v1_token)


# test --------------------------------------------------------------------


test_data<- readRDS(file = list.files(file.path(getwd(),"data","EU","rds_clean"),pattern = "*.RDS",full.names = T)[1])


test_cleaned<- data_cleaner(test_data)

eu_reply_desc_plot<- descriptive_plots(data = test_cleaned,
                                             case = "the EU",
                                             what = "reply",
                                             unit = "year",
                                             graph_type = "scatter")

eu_reply_desc_plot + eu_reply_graph

#TODO:
#1) see if the functions are scalable
#2) Fix todo's in the function folder