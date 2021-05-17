ScrapeFirm <- function(url){
  #This function downloads firm info from the value.today website
  minus <- ifelse(grepl(1,url)|grepl(2,url)|grepl(3,url), 122, 118)
  webpage <- read_html(url)
  Firm_Name <- webpage %>% html_nodes("h2 a") %>% html_text()
  Var_Name <- webpage %>% html_nodes("div strong") %>% html_text()
  Var_Name <- Var_Name[-1]
  Raw <- webpage %>% html_nodes("div span") %>% html_text()
  Raw <- Raw[3:(length(Raw)-minus)]
  DataLong <- cbind(Var_Name, Raw)
  DLong <- DataLong %>% 
    as_tibble() %>% 
    mutate(Rank1 = as.numeric(
      ifelse(Var_Name == "Rank in Canada (in Search Criteria): ", 
             Raw, 1)), 
      Rank = 0)
  for(x in 1:length(DLong$Rank1)){
    vec <- DLong$Rank1[1:x]
    DLong$Rank[x] <- as.numeric(max(vec))
  }
  DWide <- DLong %>% 
    select(-Rank1) %>% 
    dcast(Rank ~ Var_Name, value.var="Raw", drop = FALSE)
  Firm_Names <- Firm_Name %>% 
    as_tibble() %>% 
    mutate(Rank = as.numeric(seq(DWide$Rank[1],DWide$Rank[length(DWide$Rank)])))
  DWideNamed <- DWide %>% left_join(Firm_Names, by = "Rank")
  DWideNamed
}


search_main <- function(firm_name){
  #This function searches for a firm name in the CEWS database
  retbus <- grepl(pattern = paste0("^", gsub(pattern = " CORPORATION$| CORP?.$", replacement = "", x = firm_name)), 
                  toupper(cews_list$business_name))
  if(sum(retbus, na.rm = TRUE)==0){
    retop <- grepl(pattern = paste0("^", gsub(pattern = " CORPORATION$| CORP?.$", replacement = "", x = firm_name)), 
                   toupper(cews_list$operating_name))
    CEWS <-  min(sum(retop, na.rm= TRUE), 1)
  }else{
    CEWS <- min(sum(retbus, na.rm = TRUE), 1)
  }
  CEWS
}

search_subsidiaries <- function(recordID){
  #This function takes a record ID in the ICO database and returns whether any 
  #the firm's subsidiaries are in the CEWS database, a vector of all its 
  #subsidiaries and a vector of its subsidiaries who received CEWS
  ret <- list()
  if(is.na(recordID)){
    ret$CEWS <- NA
    ret$CEWS_Subsidiaries <- ""
    ret$Subsidiaries <- ""
    return(ret)
  }else{
    names <- getSubsids(recordID)
    if(length(names)==0){
      ret$CEWS <- NA
      ret$CEWS_Subsidiaries <- ""
      ret$Subsidiaries <- ""
      return(ret)
    }
    bus_name_match <- sapply(X = paste0("^", unique(names)), 
                             FUN = grepl, 
                             tolower(cews_list$business_name)) #matrix of matched subsidiaries in ICO database and CEWS list
    bus_counts <- apply(X = bus_name_match, FUN = sum, MARGIN = 2, 
                        na.rm = TRUE) #vector of # of matches for each subsidiary
    bus_names <- sub("\\^","", names(bus_counts)[bus_counts>0]) #names of subsidiaries in ICO database and CEWS list
    if(sum(bus_counts, na.rm = TRUE)==0){
      op_name_match <- sapply(X = paste0("^", unique(names)), 
                              FUN = grepl, 
                              tolower(cews_list$operating_name))
      op_counts <- apply(op_name_match, sum, MARGIN = 2, na.rm = TRUE)
      op_names <- sub("\\^","",names(op_counts)[op_counts>0])
    }else{
      op_counts <- 0
      op_names <- NULL
    }
    CEWS <- ifelse(sum(bus_counts, op_counts, na.rm = TRUE)>0, 1, 0)
    ret$CEWS <- CEWS
    ret$CEWS_Subsidiaries <- case_when(CEWS==0 ~ "", 
                                       CEWS != 0 ~ paste(unique(c(bus_names, op_names)), collapse = ", "))
    ret$Subsidiaries <- unique(names)
    ret
  }
}

getSubsids <- function(recordID){
  #This function takes an ICO record ID and returns a character vector of the 
  #firm's subsidiaries
  ICO <- ICO %>% mutate(subsid_var = 0)
  i <- recordID + 1
  record <- ICO %>% filter(RECID == recordID)
  reflvl <- record %>% select(LEVEL) %>% as.numeric()
  rqstID <- record %>% select(RQSTID) %>% as.numeric()
  record_loop <- ICO %>% filter(RECID == i)
  while(record_loop %>% select(LEVEL) %>% as.numeric() > reflvl || 
        record_loop %>% select(RECTYPE) %>% as.character() == "I"){
    if(record_loop %>% select(RECTYPE) %>% as.character() != "I"){
      ICO$subsid_var[ICO$RECID==i] <-  1
    }
    i <-  i + 1
    record_loop <- ICO %>% filter(RECID == i)
  }
  names <- ICO %>% 
    filter(RQSTID == rqstID & subsid_var == 1 & COP == 100) %>% 
    select(NAME) %>% 
    lapply(iconv, from = "UTF-8", to = "ASCII") %>% 
    lapply(tolower) %>%
    lapply(gsub, pattern = " inc.?$| ltd.?$| limited$", replacement="")
  if(length(names)==0){
    return(NA)}
  else{
    return(names$NAME)
  }
}

getRecID <- function(firm_name){
  #This function takes a firm name and returns its ICO record number
  tryCatch(
    select_rows(firm_name), 
    warning = function(cond){message(cond)},
    error = function(cond){return(NA)})
}

select_rows <- function(firm_name){
  if(nrow(ICO %>% filter(NAME == firm_name) %>% slice_min(LEVEL)) ==1){
    firm_entry <- ICO %>%
      filter(NAME == firm_name) %>% 
      slice_min(LEVEL)
  }else{
    firm_name_clean <- gsub(firm_name, pattern = 
                              " INC.?$| LTD.?$| LIMITED$",
                            replacement="")
    if(nrow(ICO %>% filter(CLEAN_NAME == firm_name_clean) %>% slice_min(LEVEL)) ==1){
      firm_entry <- ICO %>% 
        filter(CLEAN_NAME == firm_name_clean) %>%
        slice_min(LEVEL)
    }else{
      if(nrow(ICO %>% filter(CLEAN_NAME == firm_name_clean & RECTYPE != "I") %>%
              slice_min(LEVEL)) == 1){
        firm_entry <- ICO %>% 
          filter(CLEAN_NAME == firm_name_clean & RECTYPE != "I") %>%
          slice_min(LEVEL)
      }else{
      selected <- ICO %>%
        filter(grepl(paste0("^", firm_name_clean,"|/ ?",firm_name_clean),
                     CLEAN_NAME)) %>%
        slice_min(LEVEL)
      firm_entry <- select_entry(selected)
      }
    }
  }
  rcrdID <- firm_entry %>% select(RECID) %>% as.numeric()
  rcrdID
}

select_entry <- function(rows){
  if(nrow(rows)==1){
    entry <- rows
  }else{
    if(nrow(rows %>% filter(RECTYPE == "E"))==1){
      entry <- rows %>% filter(RECTYPE == "E")
    }else{
      if(nrow(rows %>% filter(RECTYPE == "E")) == 1){
        entry <- rows %>% filter(COMMENT == "R")
      }else{
        entry <- rows %>% filter(COMMENT != "NULL")
      }
    }
  }
  entry
}
