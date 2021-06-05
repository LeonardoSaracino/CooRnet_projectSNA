#' get_coord_shares
#'
#' Given a dataset of CrowdTangle shares and a time threshold, this function detects networks of entities (pages, accounts and groups) that performed coordinated link sharing behavior
#'
#' @param ct_shares.df the data.frame of link posts resulting from the function get_ctshares
#' @param coordination_interval a threshold in seconds that defines a coordinated share. Given a dataset of CrowdTangle shares, this threshold is automatically estimated by the estimate_coord_interval interval function. Alternatively it can be manually passed to the function in seconds
#' @param percentile_edge_weight defines the percentile of the edge distribution to keep in order to identify a network of coordinated entities. In other terms, this value determines the minimum number of times that two entities had to coordinate in order to be considered part of a network. (default 0.90)
#' @param clean_urls clean the URLs from the tracking parameters (default FALSE)
#' @param keep_ourl_only restrict the analysis to ct shares links matching the original URLs (default=FALSE)
#' @param gtimestamps add timestamps of the fist and last coordinated shares on each node. Slow on large networks (default=FALSE)
#'
#' @return A list (results_list) containing four objects: 1. The input data.table (ct_shares.dt) of shares with an additional boolean variable (coordinated) that identifies coordinated shares, 2. An igraph graph (highly_connected_g) with networks of coordinated entities whose edges also contains a t_coord_share attribute (vector) reporting the timestamps of every time the edge was detected as coordinated sharing, 3. A dataframe with a list of coordinated entities (highly_connected_coordinated_entities) with respective name (the account url), number of shares performed, average subscriber count, platform, account name, if the account name changed, if the account is verified, account handle, degree and component number
#'
#' @examples
#' output <- get_coord_shares(ct_shares.df)
#'
#' output <- get_coord_shares(ct_shares.df = ct_shares.df, coordination_interval = coordination.interval, percentile_edge_weight=0.9, clean_urls=FALSE, keep_ourl_only=FALSE, gtimestamps=FALSE)
#'
#' # Extract the outputs
#' get_outputs(output)
#'
#' # Save the data frame of CrowdTangle shares marked with the “is_coordinated” column
#' write.csv(ct_shares_marked.df, file=“ct_shares_marked.df.csv”)
#'
#' # Save the graph in a Gephi readable format
#' library(igraph)
#' write.graph(highly_connected_g, file="highly_connected_g.graphml", format = "graphml")
#'
#' # Save the data frame with the information about the highly connected coordinated entities
#' write.csv(highly_connected_coordinated_entities, file=“highly_connected_coordinated_entities.csv”)
#'
#' @importFrom stats quantile
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom dplyr mutate mutate select filter
#' @importFrom foreach foreach %dopar%
#' @importFrom doSNOW registerDoSNOW
#' @importFrom tidytable unnest. bind_rows.
#' @importFrom sentimentr sentiment get_sentences
#'
#' @export

get_coord_shares <- function(ct_shares.df, coordination_interval=NULL, percentile_edge_weight=0.90, clean_urls=FALSE, keep_ourl_only=FALSE, gtimestamps=FALSE) {

  options(warn=-1)

  # estimate the coordination interval if not specified by the users
  if(is.null(coordination_interval)){
    coordination_interval <- estimate_coord_interval(ct_shares.df, clean_urls = clean_urls, keep_ourl_only= keep_ourl_only)
    coordination_interval <- coordination_interval[[2]]
  }

  # use the coordination interval resulting from estimate_coord_interval
  if(is.list(coordination_interval)){
    coordination_interval <- coordination_interval[[2]]
  }

  # use the coordination interval set by the user
  if(is.numeric(coordination_interval)){
    if (coordination_interval == 0) {
      stop("The coordination_interval value can't be 0.
           \nPlease choose a value greater than zero or use coordination_interval=NULL to automatically calculate the interval")
    } else {

    coordination_interval <- paste(coordination_interval, "secs")

    if (file.exists("log.txt")) {
      write(paste("\nget_coord_shares script executed on:", format(Sys.time(), format = "%F %R %Z"),
                  "\ncoordination interval set by the user:", coordination_interval), file="log.txt", append=TRUE)
    } else {
      write(paste0("#################### CooRnet #####################\n",
                   "\nScript executed on:", format(Sys.time(), format = "%F %R %Z"),
                  "\ncoordination interval set by the user:", coordination_interval),
            file="log.txt")
      }
    }
  }

  # remove posts where imageText is empty
  ct_shares.df <- subset(ct_shares.df, !is.na(ct_shares.df$imageText))

  # get a list of all the shared imageText
  unique_imageText <- as.data.frame(table(ct_shares.df$imageText))
  names(unique_imageText) <- c("imageText", "ct_shares")
  unique_imageText <- subset(unique_imageText, unique_imageText$ct_shares > 1)
  unique_imageText$imageText <- as.character(unique_imageText$imageText)

  # keep only shares of imageText shared more than one time
  ct_shares.df <- subset(ct_shares.df, ct_shares.df$imageText %in% unique_imageText$imageText)

  # for each unique imageText execute CooRnet code to find coordination
  datalist <- list()

  # progress bar
  total <- nrow(unique_imageText)
  pb <- txtProgressBar(max=total, style=3)

  for (i in 1:nrow(unique_imageText)) {

    utils::setTxtProgressBar(pb, pb$getVal()+1)

    current_imageText <- unique_imageText$imageText[i]
    dat.summary <- subset(ct_shares.df, ct_shares.df$imageText==current_imageText)

    if (length(unique(dat.summary$account.url)) > 1) {
      dat.summary <- dat.summary %>%
        dplyr::mutate(cut = cut(as.POSIXct(date), breaks = "25 secs")) %>%
        dplyr::group_by(cut) %>%
        dplyr::mutate(count=n(),
                      account.url=list(account.url),
                      share_date=list(date),
                      imageText = current_imageText) %>%
        dplyr::select(cut, count, account.url, share_date, imageText) %>%
        dplyr::filter(count > 1) %>%
        unique()

      datalist <- c(list(dat.summary), datalist)
      rm(dat.summary)
    }
  }

  datalist <- tidytable::bind_rows.(datalist)

  if(nrow(datalist)==0){
    stop("there are not enough shares!")
  }

  coordinated_shares <- tidytable::unnest.(datalist)
  coordinated_shares
  rm(datalist)

  # mark the coordinated shares in the data set
  ct_shares.df$is_coordinated <- ifelse(ct_shares.df$imageText %in% coordinated_shares$imageText &
                                          ct_shares.df$date %in% coordinated_shares$share_date &
                                          ct_shares.df$account.url %in% coordinated_shares$account.url, TRUE, FALSE)

  # create a dataframe with no empty message
  no_empty_message <- subset(ct_shares.df, !is.na(ct_shares.df$message))

  # clear message column
  for (i in 1:nrow(no_empty_message)){
    current_message <- no_empty_message$message[i]
    current_message <- gsub("#[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", current_message) # delete hasthag in the messges
    current_message <- gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", current_message) # delete url in the messges
    current_message <- gsub("\\W", " ", current_message)

    no_empty_message$message[i] <- current_message
  }
  rm(current_message, i)

  # clear imageText column
  for (i in 1:nrow(no_empty_message)){
    current_imageText <- no_empty_message$imageText[i]
    current_imageText <- gsub("#[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", current_imageText)
    current_imageText <- gsub("http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", "", current_imageText)
    current_imageText <- gsub("\\W", " ", current_imageText)

    no_empty_message$imageText[i] <- current_imageText
  }
  rm(current_imageText, i)

  # generate parameters for the sentiment analysis for messages
  sentiment <- c()
  average_sentiment <- 0
  counter <- 0

  for (i in 1:nrow(no_empty_message)){
    current_sentiment <- sentiment(get_sentences(no_empty_message$message[i]))
    if(is.na(current_sentiment$sentiment) || is.null(current_sentiment$word_count)){
      current_sentiment$sentiment <- 0
    }
    else{
      average_sentiment <- average_sentiment + current_sentiment$sentiment
      counter <- counter + 1
    }
    sentiment <- c(sentiment, current_sentiment$sentiment)
  }
  average_sentiment <- average_sentiment/counter

  no_empty_message$sentiment <- sentiment

  # generate parameters for the sentiment analysis for imageText
  sentiment_img <- c()

  for (i in 1:nrow(no_empty_message)){
    current_sentiment_img <- sentiment(get_sentences(no_empty_message$imageText[i]))
    sentiment_img <- c(sentiment_img, current_sentiment_img$sentiment)
  }

  no_empty_message$sentiment_img <- sentiment_img

  # define the treshold for creating network based on sentiment analysis
  treshold_sentiment <- (max(abs(no_empty_message$sentiment)) - min(abs(no_empty_message$sentiment)))*average_sentiment

  rm(sentiment, sentiment_img, i, counter, current_sentiment, current_sentiment_img, average_sentiment)

  # orber by sentiment value
  no_empty_message <- no_empty_message[order(no_empty_message$sentiment, decreasing = FALSE),]

  no_empty_message$is_coordinated <- c(FALSE)

  # progress bar
  total <- nrow(no_empty_message)
  pb <- txtProgressBar(max = total, style = 3)

  # extract network from sentiment analysis
  for (i in 1:nrow(no_empty_message)){
    utils::setTxtProgressBar(pb, pb$getVal()+1)

    no_empty_message$is_coordinated[i] <- ifelse((abs(no_empty_message$sentiment[i]) + treshold_sentiment) > abs(no_empty_message$sentiment[i+1])
                                             && (abs(no_empty_message$sentiment_img[i]) + treshold_sentiment) > abs(no_empty_message$sentiment_img[i+1]), TRUE, FALSE)

  }
  # include that data frame in ct_shares.df
  no_empty_message <- subset(no_empty_message, isTRUE(no_empty_message$is_coordinated))
  ct_shares.df$is_coordinated <- ifelse(ct_shares.df$account.url %in% no_empty_message$account.url &&
                                          isTRUE(no_empty_message$is_coordinated), TRUE, FALSE)


#####################################

  # call build_coord_graph
  highly_c_list <- build_coord_graph(ct_shares.df=ct_shares.df, coordinated_shares=coordinated_shares, percentile_edge_weight=percentile_edge_weight, timestamps=gtimestamps)

  highly_connected_g <- highly_c_list[[1]]
  highly_connected_coordinated_entities <- highly_c_list[[2]]
  q <- highly_c_list[[3]]
  rm(highly_c_list)

  uniqueURLs_shared <- unique(ct_shares.df[, c("expanded", "is_coordinated")])

    # write the log
    write(paste("\nnumber of unique URLs shared in coordinated way:", table(uniqueURLs_shared$is_coordinated)[2][[1]], paste0("(", round((table(uniqueURLs_shared$is_coordinated)[2][[1]]/nrow(uniqueURLs_shared)),4)*100, "%)"),
                "\nnumber of unique URLs shared in non-coordinated way:", table(uniqueURLs_shared$is_coordinated)[1][[1]], paste0("(", round((table(uniqueURLs_shared$is_coordinated)[1][[1]]/nrow(uniqueURLs_shared)),4)*100, "%)"),
                "\npercentile_edge_weight:", percentile_edge_weight, paste0("(minimum coordination repetition: ", q, ")"),
                "\nhighly connected coordinated entities:", length(unique(highly_connected_coordinated_entities$name)),
                "\nnumber of component:", length(unique(highly_connected_coordinated_entities$component))),
          file="log.txt", append=TRUE)

    results_list <- list(ct_shares.df, highly_connected_g, highly_connected_coordinated_entities)

    return(results_list)
}

