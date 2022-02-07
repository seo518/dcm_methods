#########################
### To do: put in scope and URLs to JSON file

#### Libraries Init ####
pck <- data.frame(installed.packages(), stringsAsFactors = F)
required.pck <- c("devtools","RDoubleClick","httr","httpuv","rlang","plyr","jsonlite") 
pck <- required.pck[!required.pck %in% pck$Package]
# if(length(pck)>=2){
#   for(p in 1:length(pck)){
#     install.packages(pck)
#   }
#   if(pck=="devtools"){
#     install_github("WillemPaling/RDoubleClick")
#   }
# }

# require(devtools)
# require(RDoubleClick)
require(httr)
require(httpuv)
require(rlang)
require(plyr)
require(jsonlite)
require(tcltk2)
require(tcltk)
require(data.table)



### Change this directory to the desired location >> Create install directory
json.dir <- path.expand("~/dcm_methods/APIJson/")

### Dir checks
if(!dir.exists(json.dir)){
  stop("json.dir does not point to anywhere. Copy and paste location of the json files that is fed to the script.")
}
d <- unlist(strsplit(json.dir,split = ""))
if(d[length(d)]!="/"){
  json.dir <- paste0(json.dir,"/")
  rm(d)
}
if(file.exists(json.dir)){
  stop("json.dir should point to a directory that contains the proper files. It should not be a file.")
}


#### OAuth functions ####

#### Cryptograph 
nonce <- function(length = 10) { 
  paste(sample(c(letters, LETTERS, 0:9), length, replace = TRUE), collapse = "") 
}

oauth_app <- function (appname, key, secret = NULL, redirect_uri = oauth_callback()){
  # appname = "google"
  # key = client.id
  # secret = client.secret
  if (missing(secret)) {
    env_name <- paste0(toupper(appname), "_CONSUMER_SECRET")
    secret <- Sys.getenv(env_name)
    if (secret == "") {
      warning("Couldn't find secret in environment variable ", 
              env_name, call. = FALSE)
      secret <- NULL
    }
    else {
      message("Using secret stored in environment variable ", 
              env_name)
    }
  }
  structure(list(appname = appname, secret = secret, key = key, 
                 redirect_uri = redirect_uri), class = "oauth_app")
}

#### Authentication
#' @Purpose : Initiate OAuth 2.0 Flow
#' 
#' Input:
#' @param : endpoint, use oauth_endpoints('platform') method
#' @param : app definition, object containing client id and secret, use oauth_app('platform','client id', 'client secret') method
#' 
#' Output {Oauth2.0 flow}:
#' @return : localhost page opened on default browser to allow access using Platform authentication method
#' 
init_oauth2.0.1 <- function (endpoint, app, scope = NULL, user_params = NULL, type = NULL, 
                             use_oob = getOption("httr_oob_default"), is_interactive = interactive(), 
                             use_basic_auth = FALSE) {
  # use_basic_auth = FALSE
  # type = NULL
  # user_params = NULL
  # endpoint = oauth_endpoints("google")
  # app = app.def
  # scope = scope
  # is_interactive = T
  # use_basic_auth =  F
  # use_oob =  F
  
  if (!use_oob && !is_installed("httpuv")) {
    message("httpuv not installed, defaulting to out-of-band authentication")
    use_oob <- TRUE
  }
  if (isTRUE(use_oob)) {
    stopifnot(interactive())
    redirect_uri <- "urn:ietf:wg:oauth:2.0:oob"
    state <- NULL
  } else {
    redirect_uri <- oauth_callback()
    state <- nonce()
    print(state)
  }
  scope_arg <- paste(scope, collapse = " ")
  authorize_url <- modify_url(endpoint$authorize, query = compact(list(client_id = app$key, 
                                                                       scope = scope_arg, redirect_uri = redirect_uri, response_type = "code", 
                                                                       state = state)))
  print(authorize_url)
  if (isTRUE(use_oob)) {
    code <- oauth_exchanger(authorize_url)$code
  }
  else {
    code <- oauth_listener(authorize_url, is_interactive)$code
  }
  req_params <- list(client_id = app$key, redirect_uri = redirect_uri, 
                     grant_type = "authorization_code", code = code)
  if (!is.null(user_params)) {
    req_params <- utils::modifyList(user_params, req_params)
  }
  if (isTRUE(use_basic_auth)) {
    req <- POST(endpoint$access, encode = "form", body = req_params, 
                authenticate(app$key, app$secret, type = "basic"))
  }
  else {
    req_params$client_secret <- app$secret
    req <- POST(endpoint$access, encode = "form", body = req_params)
  }
  stop_for_status(req, task = "get an access token")
  content(req, type = type)
}

#### Refresh Token
oauth2.0_refresh <- function(endpoint, app, auth_token, type = NULL) {
  req <- POST(
    url = endpoint$access,
    multipart = FALSE,
    body = list(
      client_id = app$key,
      client_secret = app$secret,
      grant_type = "refresh_token",
      refresh_token = auth_token$refresh_token
    )
  )
  content_out <- content(req, type = type)
  content_out <- c(content_out, auth_token$refresh_token)
}

#### Authenticators ####
cred.loader <- function(loader.name, default.lib = json.dir){
  return(fromJSON(list.files(default.lib, pattern = loader.name, full.names = T)))
}

#### Re-authentication 
token.refresh <- function(platform,default.lib = json.dir, access.token){
  if(length(grep(platform,list.files(default.lib)))==0){
    message("First time running API in this pc.\nInitiating oAuth protocol.\n\n")
    
    auth_token <- init_oauth2.0.1(endpoint = oauth_endpoints("google"),
                                  app = oauth_app(appname = "google", 
                                                  key = client.id, 
                                                  secret = client.secret),
                                  scope = access.token$Creds$Google$scope[grep(platform,access.token$Creds$Google$scope)],
                                  is_interactive = interactive(), use_basic_auth =  F, use_oob =  F)
    
    y <- fromJSON(paste0(default.lib,"google.tokens.json"))
    y[[length(y)+1]] <- auth_token
    names(y)[length(y)] <- platform
    x <- toJSON(y, pretty = T)
    write(x,paste0(default.lib,"google.tokens.json"))
    write("1",paste0(default.lib,platform))
    return(auth_token)
    rm(x)
  } else {
    auth_token <- fromJSON(paste0(default.lib,"google.tokens.json"))
    auth_token <- auth_token[[grep(platform,names(auth_token))]]
    auth_refresh <- oauth2.0_refresh(endpoint = oauth_endpoints("google"),app = oauth_app(appname = "google", key = client.id, secret = client.secret),auth_token = auth_token)
    return(auth_refresh)
  }
}

#### URL Loaders
#### These are manually inputed into the JSON file.
#### The name of the URLs in the json file must be the same name as the function name
#### example: 
#### "listQueries":["https://www.googleapis.com/doubleclickbidmanager/v1/queries"]
URL_API <- function(directory,platform){
  api.url <- fromJSON(directory)
  names(api.url)
  tryCatch({
    x <- api.url[[grep(paste0("^",platform,"$"),names(api.url))]]
    return(x)
  }, error = function(x){
    NULL
  })
}

#### Loader
google.loader <- function(platform){
  access <- cred.loader("Access")
  e <- parent.env(environment())
  e$client.id <- access$Creds$Google$client_id[[1]]
  e$client.secret <- access$Creds$Google$client_secret[[1]]
  e$scope <- grep(platform,access$Creds$Google$scope, value = T)[1]
  e$auth_token <- token.refresh(platform, access.token = access)
  e$api.urls <- URL_API(access$Directory$URL_API_GOOG,platform)
  # auth_token <- cred.loader("google")
  # auth_token <- auth_token[[grep(platform,names(auth_token))]]
}

#### DBM Functions ####
DBM.listQueries <- function(access_token,api_url){
  binary.data <- GET(api_url,
                     config = add_headers("Authorization" = sprintf("Bearer %s", access_token),
                                          "Content-Type" = "application/json"),
                     verbose())
  data <- content(binary.data, "text")
  data <- fromJSON(data)
  df <- data.frame(data$queries$metadata, stringsAsFactors = F)
  df$queryId <- data$queries$queryId
  return(df)
}

DBM.getQueryLatestReport <- function(queryId, access_token, df){
  x <- df[which(df$queryId==queryId),"googleCloudStoragePathForLatestReport"]
  x <- GET(x, verbose())
  x <- content(x, "text")
  tmp.name <- paste0("~/",nonce(20))
  write(x,tmp.name)
  x <- read.csv(tmp.name, stringsAsFactors = F, colClasses = "character")
  file.remove(tmp.name)
  return(x)
}

DBM.getListofQueries <- function(api_url = api.urls, queryName, wildcard = T , id = F, token){
  df <- DBM.listQueries(api_url = api_url$listQueries, access_token = token$access_token)
  if(wildcard){
    df <- df[grep(queryName,df$title),]
    print(nrow(df))
  } else {
    df <- df[grep(queryName,df$title, fixed = T),]
    print(nrow(df))
  }
  queryList <- list()
  for(i in 1:nrow(df)){
    queryList[[i]] <- DBM.getQueryLatestReport(queryId = df$queryId[[i]],df = df, access_token = token$access_token)
  }
  names(queryList) <- df$title
  e <- parent.env(environment())
  e$queryList <- queryList
}



#### DCM Functions ####
DCM.getProfileList <- function(api_url, token){
  x <- GET(api_url,                   
           config = add_headers("Authorization" = sprintf("Bearer %s", token),"Content-Type" = "application/json"),
           verbose())
  x <- content(x, "text")
  x <- fromJSON(x)
  return(x)
}

DCM.listProfileReports <- function(api_url,profileId, token){
  binary.data <- GET(gsub("\\{PROFILE_ID\\}",profileId,api_url),
                     config = add_headers("Authorization" = sprintf("Bearer %s", token),
                                          "Content-Type" = "application/json"),                  
                     verbose())
  data <- content(binary.data, "text")
  data <- fromJSON(data)
  return(data)
}


DCM.getProfileReportLatest <- function(url_api,profileId,reportId, token){
  url_api <- gsub("\\{PROFILE_ID\\}",profileId,api_url)
  url_api <- gsub("\\{REPORT_ID\\}",reportId,api_url)
  
  binary.data <- GET(url_api,
                     config = add_headers("Authorization" = sprintf("Bearer %s", token),
                                          "Content-Type" = "application/json"),                  
                     verbose())
  data <- content(binary.data, "text")
  data <- fromJSON(data)
  
  return(data)
}

DCM.getReport<- function(api_url,token){
  require(data.table)
  x <- GET(api_url,
           config = add_headers("Authorization" = sprintf("Bearer %s", token)),                  
           verbose())
  file_0 <- paste0("~/",nonce(),".csv")
  write(content(x,"text"),file_0)
  x <- fread(file_0)
  file.remove(file_0, what = "text")
  return(x)
}

#' @URL: https://developers.google.com/doubleclick-advertisers/v3.1/accountUserProfiles/list
DCM.listAccountUserProfiles <- function(api_url, profileId, token){
  binary.data <- GET(gsub("\\{PROFILE_ID\\}",profileId,api_url),
                     config = add_headers("Authorization" = sprintf("Bearer %s", token),
                                          "Content-Type" = "application/json"),                  
                     verbose())
  data <- content(binary.data, "text")
  data <- fromJSON(data)
  return(data)
}

#' @URL: https://developers.google.com/doubleclick-advertisers/v3.1/userProfiles/list
DCM.listUserProfiles <- function(api_url, token){
  binary.data <- GET(api_url,
                     config = add_headers("Authorization" = sprintf("Bearer %s", token),
                                          "Content-Type" = "application/json"),                  
                     verbose())
  data <- content(binary.data, "text")
  data <- fromJSON(data)
  return(data)
}

DCM.patchProfile <- function(api_url, token, profileId, id,...){
  api_url <- paste0(gsub("\\{PROFILE_ID\\}",profileId,api_url),"?id=",id)
  print(api_url)
  require(jsonlite)
  body.request <- toJSON(list(...),pretty = T)
  
  cat(toJSON(list(...),pretty = T),"\n")
  
  x <- PATCH(url = api_url, body = body.request,
             config = add_headers("Authorization" = sprintf("Bearer %s", token),
                                  "Content-Type" = "application/json"),
             verbose())
  return(x)
}

#### Google Drive Functions ####
drive.listFiles <- function(api_url, token, blankDf = data.frame(NULL,stringsAsFactors = F) , ...){
  # token = auth_token$access_token
  page.0 <- 1
  hid.objs <- list(...)
  env.objs <- names(list(...))
  empty.string <- NULL
  if(length(hid.objs)>=1){
    for(i in 1:length(hid.objs)){
      empty.string[i] <- hid.objs[[i]]
    }
    api_url <- paste0(api_url,"?",gsub(" ","%20",paste0("&",paste0(env.objs,"=",empty.string, collapse = "&"))))
  } 
  
  data <- GET(api_url,
              config = add_headers("Authorization" = sprintf("Bearer %s", token)),
              verbose())
  data.content <- fromJSON(content(data,"text"))
  nextPage <- data.content$nextPageToken
  blankDf <- rbind(blankDf,data.content$files)
  cat("Page",page.0)
  
  if(length(data.content$nextPageToken)>=1){
    while(length(data.content$nextPageToken)>=1){
      data <- GET(paste0(api_url,"&pageToken=",nextPage),
                  config = add_headers("Authorization" = sprintf("Bearer %s", token)),
                  verbose())
      data.content <- fromJSON(content(data,"text"))
      nextPage <- data.content$nextPageToken
      page.0 <- page.0+1
      cat("Page",page.0)
      blankDf <- rbind(blankDf,data.content$files)
    }
  }
  return(blankDf)
}

drive.emptyTrash <- function(api_url,token){
  DELETE(api_url,
         config = add_headers("Authorization" = sprintf("Bearer %s", token)))$status_code
}

##### Sheets Functions #####
sheets.values.Get <- function(api_url, token, spreadsheetId, range = "A:Z"){.
  
  api_url <- gsub("\\{spreadsheetId\\}",spreadsheetId,api_url)
  api_url <- gsub("\\{range\\}",range,api_url)
  
  data <- GET(api_url,
              config = add_headers("Authorization" = sprintf("Bearer %s", token)),
              verbose())
  
  data.content <- fromJSON(content(data,"text"))
  data.content.df <- data.content$values
  
  
  df <- data.frame(data.content$values, stringsAsFactors = F )
  names(df) <- df[1,]
  df <- df[-1,]
  return(df)
  
}


