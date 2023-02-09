#' @title WOE Transformation
#' @description Function that calculates the WOE for each bin and the information value for each variable.
#' @param data Dataframe of binned variables.
#' @param id ID variable.
#' @param target A binary target variable.
#' @param varcol Vector of variables to have WOE transformation.
#' @param IVfilter  Threshold of variables' Information Value
#'
#' @return A list of three items.
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' #skip for now
WOEProfet_new <- function (data, id, target, varcol, IVfilter)
{
  colnames(data) <- gsub("\\.","_",colnames(data))

  if (is.character(target) == TRUE) {
    tar_char <- gsub("\\.","_", target)
    target <- match(gsub("\\.","_", target), colnames(data))
  } else if (is.numeric(target) == TRUE) {
    tar_char <- colnames(data)[target]
  }

  if (is.character(id) == TRUE) {
    id_char <- gsub("\\.","_", id)
    id <- match(gsub("\\.","_", id), colnames(data))
  } else if (is.numeric(id) == TRUE) {
    id_char <- colnames(data)[id]
  }

  if (missing(varcol)) {
    varcol <- (1:ncol(data))[c(-id, -target)]
  }
  if (is.character(varcol) == TRUE) {
    varcol <- match(gsub("\\.","_", varcol), colnames(data))
  }
  ##Option 1: if no IVfilter entry, the default is no filter
  if (missing(IVfilter)) {
    IVfilter <- 0
  }
  ##Option 2: if no IVfilter entry, then return an error message and stop
  # if (missing(IVfilter)) {
  #   stop("IVfilter is a required argument")
  # }

  ## takes id, bad, and one predictor variable
  ## output a dataframe of id, predictor var, and the WOE of the predictor var
  WOEFun1 <- function(data, idcol, targetcol, varcol) {
    data2 <- data[, c(idcol, targetcol, varcol)]
    data2$Bins <- as.character(data2[, 3])
    NumBad <- stats::aggregate(data2[, 2] ~ Bins, data = data2, FUN = sum)
    NumGood <- stats::aggregate((ifelse(data2[, 2] == 1, 0, 1)) ~
                           Bins, data = data2, FUN = sum)
    IVF1_i <- (NumBad[, 2]/sum(NumBad[, 2])) - (NumGood[,
                                                        2]/sum(NumGood[, 2]))
    IVF2_i <- log((NumBad[, 2]/sum(NumBad[, 2]))/(NumGood[,
                                                          2]/sum(NumGood[, 2])))
    IVF3_i <- log(((NumBad[, 2] + 0.5)/(sum(NumBad[, 2]) +
                                          0.5))/((NumGood[, 2] + 0.5)/(sum(NumGood[, 2]) +
                                                                         0.5)))
    IVF23_i <- ifelse(IVF2_i == -Inf | IVF2_i == Inf, IVF3_i,
                      IVF2_i) #Weight of Evidence
    IVF_i <- IVF1_i * IVF23_i #Information values
    Temp <- data.frame(cbind(NumBad[, 1], IVF23_i))
    Temp <- plyr::rename(Temp, c(V1 = "Bins", IVF23_i = "woe"))
    Temp2 <- plyr::join(data2, Temp, by = "Bins", type = "left",
                        match = "all")
    WOE <- data.frame(Temp2)
    colnames(WOE)[5] <- paste(colnames(WOE)[3], "WOE", sep = "_")
    WOE[, 5] <- as.numeric(as.character(WOE[, 5]))
    WOE2 <- WOE[, c(1, 3, 5)]
    return(c(WOE2))
  }
  #This function returns the binned variable & its Information value (sum)
  IVFun1 <- function(data, idcol, targetcol, varcol) {
    data2 <- data[, c(idcol, targetcol, varcol)]
    data2$Bins <- as.character(data2[, 3])
    NumBad <- stats::aggregate(data2[, 2] ~ Bins, data = data2, FUN = sum) #i = # of Bins
    NumGood <- stats::aggregate((ifelse(data2[, 2] == 1, 0, 1)) ~
                           Bins, data = data2, FUN = sum)
    IVF1_i <- (NumBad[, 2]/sum(NumBad[, 2])) - (NumGood[,
                                                        2]/sum(NumGood[, 2]))
    IVF2_i <- log((NumBad[, 2]/sum(NumBad[, 2]))/(NumGood[,
                                                          2]/sum(NumGood[, 2])))
    IVF3_i <- log(((NumBad[, 2] + 0.5)/(sum(NumBad[, 2]) +
                                          0.5))/((NumGood[, 2] + 0.5)/(sum(NumGood[, 2]) +
                                                                         0.5)))
    IVF23_i <- ifelse(IVF2_i == -Inf | IVF2_i == Inf, IVF3_i,
                      IVF2_i)
    IVF_i <- IVF1_i * IVF23_i
    IVF <- sum(IVF_i)
    IVF2 <- c(colnames(data2[3]), IVF)
    return(c(IVF2))
  }

  WOEFun2 <- function(varcol) {
    WOEFun1(data, idcol = id, targetcol = target, varcol)
  }
  z <- lapply(varcol, WOEFun2)
  Merged = Reduce(function(x, y) merge(x, y, all.x = T, by = id_char),
                  z) #note that z  is a list of 1 and Merged is a list of 3
  Merged2 = merge(data[, c(id_char, tar_char)], Merged, by = id_char)
  Merged3 = Merged2[, c(1, 2, seq(4, ncol(Merged2), by = 2))] #Delete? We could filter Merged2 WOE cols to get it, see line 115
  IVFun2 <- function(varcol) {
    IVFun1(data = data, idcol = id, targetcol = target, varcol)
  }
  IV_and_Var <- lapply(varcol, IVFun2)
  IV = NULL
  IV_and_Var2 <- data.frame(Variable = sapply(IV_and_Var, "[",
                                              1), IV = as.numeric(sapply(IV_and_Var, "[", 2)))
  #Note that sapply(IV_and_Var, "[", 1) gets the first item in the list
  IV_and_Var2$Variable = as.character(IV_and_Var2$Variable)
  #library(dplyr) #can we use dplyr library?
#  `%>%` <- magrittr::`%>%`
  IV_and_Var2 <- IV_and_Var2 %>% dplyr::filter(IV >= IVfilter) %>% dplyr::arrange(dplyr::desc(IV)) # filter IV values to greater than 0.02

  filter_string <- IV_and_Var2[,1]
  Merged2_sub <- Merged2 %>% dplyr::select(c(dplyr::all_of(id_char), dplyr::all_of(tar_char)), dplyr::contains(c(filter_string)))
  Merged3_sub <- Merged2_sub %>% dplyr::select(c(dplyr::all_of(id_char), dplyr::all_of(tar_char)), dplyr::contains("_Bins_WOE"))
  Merged2_tmp <- Merged2 %>% dplyr::rename(ID = dplyr::all_of(id_char), Target = dplyr::all_of(tar_char))

  VarWOE = function(var) {
    sql_state = paste("Select", var, ",", paste(var, "WOE", sep = "_"), ",", "avg(Target) as TargetRate," , "count(ID) as Freq",
                      "from", deparse(substitute(Merged2_tmp)),
                      "group by", var, ",", paste(var, "WOE", sep = "_"),
                      sep = " ")
    chk = sqldf::sqldf(sql_state)
    return(chk)
  }

  woeList = lapply(filter_string, VarWOE)

  # Check2 <- data.frame(aggregate(data[, 2] ~ data[, 3] + data[,
  #                                                          1], data = data, FUN = mean))

  names(woeList) = gsub("_Bins", "", filter_string)
  item <- list(BinWOE = Merged2_sub, WOE = Merged3_sub, IV = IV_and_Var2,
               vars = woeList)
  return(item)
}
