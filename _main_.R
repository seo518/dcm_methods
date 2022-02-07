### Generic UI Functions
library(tcltk)
library(tcltk2)
progress.bar <<- tclVar("State Idle")
progress.bar.text <<- "State Idle"

tclArrayVar <- function(x = NULL) {
  # Check argument
  if (!is.null(x) && !is.vector(x) && length(dim(x))!= 2)
    stop("Array must be one-dimensional or two-dimensional, or NULL.")
  
  library(tcltk2)
  
  # Create the Tcl variable and the R Tcl object
  n <- .TkRoot$env$TclVarCount <- .TkRoot$env$TclVarCount + 1
  name <- paste0("::RTcl", n)
  l <- list(env = new.env(), nrow = 0, ncol = 0, ndim = 0)
  assign(name, NULL, envir = l$env)
  reg.finalizer(l$env, function(env) tkcmd("unset", ls(env)))
  class(l) <- "tclArrayVar"
  
  # A NULL array
  if (is.null(x)) {
    .Tcl(paste0("set ", name, "(0,0) \"\""))
    l$nrow <- 0
    l$ncol <- 0
    l$ndim <- 2
    return(l)
  }
  
  # A vector, matrix, or data frame
  if (is.vector(x)) {
    ndim <- 1
    x <- as.data.frame(x)
  } else ndim <- 2
  
  # Populate the Tcl array
  for (i in (1:nrow(x)))
    for (j in (1:ncol(x)))
      .Tcl(paste0("set ", name, "(", i, ",", j,") \"", x[i, j], "\""))
  
  # Process dim names
  if (nrow(x)) {
    if (is.null(rownames(x)))
      rownames(x) <- rep("", nrow(x))
    for (i in 1:nrow(x))
      .Tcl(paste0("set ", name, "(", i, ",", 0, ") \"", 
                  rownames(x)[i], "\""))
  } 
  
  if (ncol(x)) {
    if (is.null(colnames(x)))
      colnames(x) <- rep("", ncol(x))
    for (j in 1:ncol(x))
      .Tcl(paste0("set ", name, "(", 0, ",", j, ") \"", 
                  colnames(x)[j], "\""))
  }
  
  l$nrow <- nrow(x)
  l$ncol <- ncol(x)
  l$ndim <- ndim
  l
}

# edit() generic function is defined in the utils package
edit.tclArrayVar <- function(name, height = 20, width = 10, title) {
  library(tcltk2)
  
  win <- tktoplevel()
  
  tclArrayName <- ls(name$env)
  print(tclArrayName)
  if(missing(title)){
    tkwm.title(win, tclArrayName)
  } else {
    tkwm.title(win, title)
  }
  
  
  table <- tk2table(win,
                    rows = name$nrow + 1, cols = name$ncol + 1,
                    titlerows = 1, titlecols = 1,
                    maxwidth = 1000, maxheight = 1000,
                    drawmode = "fast",
                    height = height + 1, width = width + 1,
                    xscrollcommand = function(...) tkset(xscr, ...),
                    yscrollcommand = function(...) tkset(yscr,...))
  xscr <-tk2scrollbar(win, orient = "horizontal",
                      command = function(...) tkxview(table, ...))
  yscr <- tk2scrollbar(win, orient = "vertical",
                       command = function(...) tkyview(table, ...))
  
  tkgrid(table, yscr)
  tkgrid.configure(yscr, sticky = "nsw")
  tkgrid(xscr, sticky = "new")
  tkgrid.rowconfigure(win, 0, weight = 1)
  tkgrid.columnconfigure(win, 0, weight = 1)
  tkconfigure(table, variable = tclArrayName,
              background = "white", selectmode = "extended",rowseparator = "\"\n\"", colseparator = "\"\t\"", state = "disabled")
  

}

`[.tclArrayVar` <- function(object, i, j = NULL) {
  library(tcltk2)
  
  if (is.null(j) && object$ndim != 1)
    stop("Object is not a one-dimensional tclArrayVar")
  if (!is.null(j) && object$ndim != 2)
    stop("Object is not a two-dimensional tclArrayVar")
  
  if (object$ndim == 1) j <- 1
  tclArrayName <- ls(object$env)
  tclvalue(paste0(tclArrayName, "(", i, ",", j, ")"))
}

`[<-.tclArrayVar` <- function(object, i, j = NULL, value) {
  library(tcltk2)
  
  if (is.null(j) && object$ndim != 1)
    stop("Object is not a one-dimensional tclArrayVar")
  if (!is.null(j) && object$ndim != 2)
    stop("Object is not a two-dimensional tclArrayVar")
  
  if (object$ndim == 1) j <- 1
  tclArrayName <- ls(object$env)
  .Tcl(paste0("set ", tclArrayName, "(", i, ",", j, ") ", value))
  if (i > object$nrow) object$nrow <- i
  object
}


### DCM Security Methods Main Functions
getProfiles <- function(){
  progress.bar.text <<- "Updating local data"
  source(path.expand("~/dcm_methods/RGoogleAPICAT.R"))
  getCols <- fromJSON(path.expand("~/dcm_methods/config.json"))
  google.loader("dfareporting")
  userProfiles <- DCM.listUserProfiles(api_url = api.urls$listUserProfiles, token = auth_token$access_token)
  google.loader("dfatrafficking")
  blankProfiles <- data.frame(NULL, stringsAsFactors = F)
  for(i in 1:nrow(userProfiles$items)){
    accountUserProfiles <- DCM.listAccountUserProfiles(api_url = api.urls$listAccountUserProfiles, token = auth_token$access_token, profileId = userProfiles$items$profileId[[i]])
    
    df <- accountUserProfiles$accountUserProfiles[,grep(paste0(getCols$Config$accountUserProfile.getCols, collapse = "|"),
                                                        colnames(accountUserProfiles$accountUserProfiles))]
    if(length(df)>=1){
      df$accountName <- userProfiles$items[i,"accountName"]
      blankProfiles <- rbind(blankProfiles,df)
    }
    Sys.sleep(1/30)
  }
  progress.bar.text <<- "Updating Finished"
  return(blankProfiles)
}

search.Profiles <- function(email.address, profiles = dcm.profileLists){
  # email.address = "kalin"
  # wildcard = T
  # profiles = dcm.profileLists
  if(email.address==""){
    decision <- ifelse(tclvalue(tkmessageBox(title = "Search Profiles", message = "Pulling all DCM Profiles list. Export file?", type = "yesno", icon = "info"))=="yes",TRUE,FALSE)
    
    search.profile <- profiles

    file.name <- tclvalue(tkgetSaveFile(initialfile = "Profile List.csv",
                                        filetypes = "{ {CSV Comma Delimited File} {.csv} } { {All Files} * }"))
    fwrite(search.profile,file.name)
    
    if(!decision){
      return(search.profile)
    }
  } else {
    email.address <- unlist(strsplit(email.address, split = ","))
    
    search.profile <- data.frame(NULL, stringsAsFactors = F)
    for(r in 1:length(email.address)){
      search.res <- profiles[grep(email.address[r],profiles$email, ignore.case = T),]
      if(nrow(search.res)==0){
        search.res<- profiles[grep(email.address[r],profiles$id, ignore.case = T),]
      }
      search.profile <- rbind(search.profile,search.res)
    }
    progress.bar.text <<- "Search Result Complete"
    return(search.profile)
  }
}

call_search.Profiles <- function(){
  if(exists("dcm.profileLists")){
    df <- search.Profiles(email.address = tclvalue(email.address.entry))
    search_results <<- tclArrayVar(df)
    edit(search_results, title = "Search Results")
  } else {
    dcm.profileLists <<- getProfiles()
    df <- search.Profiles(email.address = tclvalue(email.address.entry))
    search_results <<- tclArrayVar(df)
    edit(search_results, title = "Search Results")
  }
}

update.Profiles <- function(email.address = tclvalue(email.address.entry), active.state){
  progress.bar.text <<- "Patching Profile"
  profile_updateQueue <- search.Profiles(email.address = tclvalue(email.address.entry))
  
  admin_accounts <- search.Profiles(email.address = "groupmadops@gmail.com")
  profile_updateQueue$admin <- admin_accounts[match(profile_updateQueue$accountId,admin_accounts$accountId),"id"]
  
  if(nrow(profile_updateQueue)!=0){
    for(u in 1:nrow(profile_updateQueue)){
      ### TO DO
      ## Get groupmadops id for account
      # google.loader(platform = "dfatrafficking")
      x <- DCM.patchProfile(api_url = api.urls$patchProfile, token = auth_token$access_token, profileId = profile_updateQueue$admin[[u]], id = profile_updateQueue$id[[u]], active = active.state)
      print(x$status_code)
    }
    progress.bar.text <<- "Patching Finished"
  } else {
    tkmessageBox(title = "Error",message = "Cannot find email address", type = "ok")
    progress.bar.text <<- "Patching Error"
  }
}

call_update.Profiles <- function(){
  if(exists("dcm.profileLists")){
    okButton <- function(){
      confirm.update <<- TRUE
      tkdestroy(tt)
      print(confirm.update)
    }
    cancelButton <- function(){
      confirm.update <<- FALSE
      tkdestroy(tt)
      print(confirm.update)
    }
    
    tt <- tktoplevel()
    
    Active <- tclVar("")
    tt$env$rb1 <- tk2radiobutton(tt)
    tt$env$rb2 <- tk2radiobutton(tt)
    tkconfigure(tt$env$rb1, variable = Active, value = "true")
    tkconfigure(tt$env$rb2, variable = Active, value = "false")
    tkgrid(tk2label(tt, text = "Patch Profiles"),columnspan = 2, padx = c(10,10), pady = c(15, 0))
    tkgrid(tk2label(tt, text = "Active:"),columnspan = 2, padx = c(10,10), pady = c(15, 0))
    tkgrid(tk2label(tt, text = "True:"),tt$env$rb1,padx = c(10,10))
    tkgrid(tk2label(tt, text = "False:"),tt$env$rb2,padx = c(10,10),pady = c(0, 15))
    tkwm.title(tt, "Patch Profiles")
    
    tt$env$confirm <- tk2button(tt, text = "Confirm", command = okButton)
    tt$env$cancel <- tk2button(tt, text = "Cancel", command = cancelButton)
    tkgrid(tt$env$confirm,tt$env$cancel)
    
    
    tkfocus(tt)
    
    tkwait.window(tt)
    print(confirm.update)
    if(confirm.update){
      update.Profiles(email.address = tclvalue(email.address.entry), active.state = tclvalue(Active))
      
      # dcm.profileLists <<- getProfiles()
    }
  } else {
    tkmessageBox(title = "Error", message = "Use Search Function first to see whether profile exists", type ="ok", icon = "warning")
  }

}

### Body

tk.window <- tktoplevel()
tktitle(tk.window) <- "DCM - Security"
tabs_list <- c("Search","FAQ")

# Create Tabs
tk.window$env$notebook <- tk2notebook(tk.window, tabs = tabs_list)
tkpack(tk.window$env$notebook, fill = "both", expand = TRUE, pady = c(10,10), padx = c(10,10))

tk.window$env$tab1 <- tk2notetab(tk.window$env$notebook, "Search")
tkgrid(tk2label(tk.window$env$tab1,textvariable=progress.bar, justify = "left"),padx = 10, pady = c(15, 5))
email.address.entry <- tclVar("")
email.search <- tkentry(tk.window$env$tab1, textvariable=email.address.entry, width="25")
tkgrid(tk2label(tk.window$env$tab1,text="Email Address/Account ID:", justify = "left"), email.search, padx = 10, pady = c(15, 5))

tk.window$env$but_search <- tk2button(tk.window$env$tab1, text = "Search",
                               command = call_search.Profiles)
tk.window$env$but_patch <- tk2button(tk.window$env$tab1, text = "Update",
                                      command = call_update.Profiles)
tkgrid(tk.window$env$but_search,tk.window$env$but_patch)

tk.window$env$tab2 <- tk2notetab(tk.window$env$notebook, "FAQ")
tkgrid(tk2label(tk.window$env$tab2,text="Search: search by id or email address. Spaces not allowed\nUpdate: Opens a prompt window with drop down boxes of available parameter changes.\nIt will update all the profiles that are inside the search box", justify = "left"))

