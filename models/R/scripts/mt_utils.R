# model tools - developed by MASHA
mt_ensureGDriveFileExists <- function(googleDriveDirectory, localFilePath, googleFilenamePattern=NULL, overwrite=FALSE) {
  if (is.null(googleFilenamePattern)) googleFilenamePattern <- fs::path_ext_remove(fs::path_file(localFilePath))
  if (file.exists(localFilePath) && !overwrite) {
    return(TRUE)
  }
  if (!dir.exists(fs::path_dir(localFilePath))) {
    warning(glue('Path "{fs::path_dir(localFilePath)}" did not exists so creating it now...'))
    dir.create(fs::path_dir(localFilePath), recursive=T)
  }
  modelsDir <- googledrive::drive_ls(googleDriveDirectory)
  fileCandidates <- modelsDir %>% filter(str_detect(name, googleFilenamePattern))
  if (nrow(fileCandidates)==0) {
    stop("Google drive file not found!")
  }
  fileID <- fileCandidates %>% pull(id) %>% first()
  if (nrow(fileCandidates) > 1) {
    fileID <- NULL
    while (is.null(fileID)) {
    msg <- glue::glue("Multiple matches found:\n{paste0('  ',1:nrow(fileCandidates), ') ', fileCandidates$name, collapse='\n')}\n")
    print(msg)
    result <- suppressWarnings(as.numeric(str_trim(readline(prompt = "Selection: "))))
    if (between(result, 1, nrow(fileCandidates))) {
      # Found a valid selection
      fileID <- fileCandidates[[result,'id']]
    } else {
      print("Invalid selection. Use numbers only.")
      fileID <- NULL
    }
  }
  }
  if (!is.null(fileID)) {
    googledrive::drive_download(file=fileID, path=localFilePath, overwrite = overwrite)
    return(TRUE)
  } else {
    warning(glue("Could not find any candidate files matching googleFilenamePattern '{googleFilenamePattern}' in '{googleDriveDirectory}'."))
    return(FALSE)
  }
}

mt_updateAll <- function(overwrite=TRUE) {
  mt_updateBigX('D', overwrite = overwrite)
  mt_updateBigX('T', overwrite = overwrite)
  mt_updateBigX('P', overwrite = overwrite)
  mt_updateParameters(overwrite = overwrite)
  mt_updateShinyInputs(overwrite = overwrite)
}

mt_updateBigX <- function(modelLetter,
                          overwrite=TRUE) {
  mt_ensureGDriveFileExists(googleDriveDirectory = 'DTP - internal/Models', 
   localFilePath = glue::glue('cache/big{modelLetter}.xlsx'), 
   googleFilenamePattern=glue::glue('big{modelLetter}'),
   overwrite = overwrite)
}

mt_updateShinyInputs <- function(overwrite=TRUE) {
  print("In case of ambiguity, you should pick shinyInputs.rds")
  mt_ensureGDriveFileExists(googleDriveDirectory = 'DTP - internal/ShinyInputs', 
                            localFilePath = './shinyInputs.rds', 
                            googleFilenamePattern='shinyInputs',
                            overwrite = overwrite)
}

mt_updateParameters <- function(overwrite = TRUE) {
  mt_ensureGDriveFileExists(googleDriveDirectory = 'DTP - internal/Models/Model parameters', 
                           localFilePath = glue::glue('models/parameters/parametersDTP.xlsx'), 
                           googleFilenamePattern=glue::glue('DTP_Parameters'),
                           overwrite = overwrite)
}

mt_makeInitialConditionsCode <- function(mtModel, popc, toClipboard=F) {
  warning("We decided to make initial conditions be a function of iso3 in DTP")
  result <- mtModel$lsModel$InitialConditions %>% 
    pivot_longer(!age_group) %>%
    complete(age_group=unique(age_group), name=as_factor(mtModel$tbCompartments$State), fill = list(value=0)) %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    mutate(pop=popc, across(.fns = ~.x*pop)) %>%
    pivot_longer(!c(age_group,pop)) %>% 
    pull(value)
  if (toClipboard) {
    clipr::write_clip(result)
  }
  result
}

mt_checkAllCompartmentsAreDefined <- function(mtModel) {
  compartmentStateNames <- mtModel$tbCompartments$State
  transitionStateNames <- c(mtModel$tbTransitions$From, mtModel$tbTransitions$To) %>%
    unique() %>% 
    str_replace('\\[.+','')
  transitionStateNames <- transitionStateNames[!str_starts(transitionStateNames,'^Null[SD]$')]
  neededInCompartments <- transitionStateNames %>% setdiff(compartmentStateNames)
  neededInTransitions <- compartmentStateNames %>% setdiff(transitionStateNames)
  errors <- F
  if(length(neededInCompartments)>0) {
    errors <- T
    warning(as.character(glue::glue("Based on defined transitions, you should add these to compartments: {paste0(neededInCompartments,collapse=',')}")))
  }
  if(length(neededInTransitions)>0) {
    errors <- T
    warning(as.character(glue::glue("Please add transitions using these compartments: {paste0(neededInTransitions,collapse=',')}")))
  }
}

mt_findSymbols <- function(mtModel, localSymbols) {
  mt_findSymbolsRecursively <- function(txt, localSymbols) {
    parts <- as.list(rlang::parse_expr(txt))
    result <- c()
    for (part in parts) {
      if(is.symbol(part)) {
        if(!(deparse(part) %in% localSymbols)) {
          result <- c(result, part)
        } 
      } else {
        if(!(is.numeric(part) | is.character(part)))
          result <- c(result, mt_findSymbolsRecursively(deparse(part), localSymbols))
      }
    }
    result <- unique(result)
    blockList <- c('(','*','/','+','-','^')
    result[!sapply(result,deparse) %in% blockList]
  }
  mtModel$tbTransitions %>% 
    rowwise() %>% 
    mutate(badSyms=paste0(mt_findSymbolsRecursively(TransExpression, localSymbols),collapse=',')) %>% 
    select(id,From,To,TransExpression,badSyms)
}

mt_makeTransitionCreationCode <- function(mtModel, modelLetter, indent=8, toClipboard=F) {
  spaces <- paste0(rep(' ', indent),collapse='')
  result <- mtModel$tbTransitions %>% 
    rowwise() %>% 
    mutate(u1=ifelse(str_detect(From, 'Null'),To,From) %>% str_replace('\\[[nxt]+\\]',''),
           u2=ifelse(str_detect(To, 'Null'),From,To) %>% str_replace('\\[[nxt]+\\]',''),
           n1=From %>% str_extract('\\[(.+)\\]') %>% str_extract('[^\\[\\]]+'),
           n2=To %>% str_extract('\\[(.+)\\]') %>% str_extract('[^\\[\\]]+'),
           v1=ifelse(str_detect(From, 'Null'),'0','-1'),
           v2=ifelse(str_detect(To, 'Null'),'0','+1')) %>% 
    mutate(u1=ifelse(str_detect(u1, 'Null'), first(mtModel$tbCompartments$State), u1),
           u2=ifelse(str_detect(u2, 'Null'), first(mtModel$tbCompartments$State), u2))
  if (any(is.na(result$n1)|is.na(result$n2))) warning('One or more compartments did not contain [n] or [nxt]. You should review your transitions list. Replacing them with [n] so the model has a chance to run ;)')
  result <- result %>%
    ungroup() %>% 
    mutate(n1=ifelse(is.na(n1), 'n', n1),
           n2=ifelse(is.na(n2), 'n', n2)) %>% 
    left_join(select(mtModel$tbCompartments, srcCompartmentID=id, srcCompartment=State), by=c(u1='srcCompartment')) %>% 
    left_join(select(mtModel$tbCompartments, dstCompartmentID=id, dstCompartment=State), by=c(u2='dstCompartment')) %>% 
    mutate(transitions=glue::glue(
      "{spaces}transitions_{modelLetter}[traind_{modelLetter}[{id},n],] <- c(varind_{modelLetter}[{srcCompartmentID},{n1}],  {v1}, varind_{modelLetter}[{dstCompartmentID},{n2}], {v2})"
    )) %>% 
    pull(transitions) %>% 
    paste0(collapse='\n')
  if (toClipboard) {
    clipr::write_clip(result)
  }
  result
}

mt_makeTransitionRatesCode <- function(mtModel, indent=4, toClipboard=F) {
  spaces <- paste0(rep(' ', indent),collapse='')
  result <- mtModel$tbTransitions %>% 
    mutate(comma=ifelse(TransExpression!=last(TransExpression),',','')) %>% 
    rowwise() %>% 
    mutate(spacedTransitionCode=paste0(spaces,'  ',TransExpression,comma,'  # ',TransitionComment,collapse=glue::glue('\n'))) %>%
    ungroup() %>% 
    pull(spacedTransitionCode) %>% 
    paste0(collapse='\n') %>% 
    glue(
      '{spaces}tranrate <- array(c(\n',.,'\n{spaces}), dim=c(N, {nrow(mtModel$tbTransitions)}))\n{spaces}tranrate <- c(t(tranrate))'
    )
  if (toClipboard) {
    clipr::write_clip(result)
  }
  result
}

mt_getTibblesInList <- function(modelFilename) {
  lsModel <- read_excel_allsheets(modelFilename, tibble=T)
  tbCompartments <- lsModel$Compartments %>%
    rowid_to_column(var='id')
  
  tbTransitions <- lsModel$Transitions %>%
    mutate(TransitionComment=ifelse(is.na(TransitionComment), '', TransitionComment)) %>% 
    rowid_to_column(var='id')
  
  alivepop <- tbCompartments %>%
    filter(isAlive) %>%
    pull(id)
  
  # varind
  varind<-matrix(seq(nrow(tbCompartments)*N), ncol = N)
  rownames(varind) <- tbCompartments$State
  # traind
  traind<-matrix(seq(nrow(tbTransitions)*N), ncol = N)
  rownames(traind) <- tbTransitions$TransitionName
  
  list(lsModel=lsModel,
       tbCompartments=tbCompartments,
       tbTransitions=tbTransitions,
       alivepop=alivepop,
       varind=varind,
       traind=traind)
}
