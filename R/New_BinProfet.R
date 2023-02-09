#' @title Coarse Binning Variable(s)
#' @description Function that bins selected variable(s) and returns a dataframe with binned values. Uses greedy binning algorithm to perform coarse binning of selected variable(s).
#' @param data Dataframe of that contains ID, binary target and variables to be binned.
#' @param id ID variable. See 'Details'.
#' @param target The binary target/response variable for WOE. See 'Details'.
#' @param varcol Vector of variables to be binned.
#' @param min.cat Minimum number of bins.
#' @param num.bins Target number of bins. Overridden by the number of levels if varcol is factor.
#' @param min.pts.bin Minimum number of observations in a bin.
#' @param bracket Indicating if the intervals should be closed on the right or left. Options include left and right.
#' @param special.values A vector of values that should have their own bin. See 'Details'.
#'
#' @return A dataframe with original ID and target columns and binned variable column(s).
#' @export
#'
#' @examples
#' mydata <- ISLR::Default
#' head(mydata)

#' mydata$ID <- seq(1:nrow(mydata)) ## make an ID variable
#' mydata$default <- ifelse(mydata$default=="Yes", 1, 0) ## target coded with 1, 0

#' ## bin balance and income
#' binned1 <- New_BinProfet(mydata, id="ID", target="default",
#'                     varcol = c("balance",  "income"), num.bins = 5)
#' head(binned1)

New_BinProfet <-  function (data, id, target, varcol, min.cat = 4, num.bins = 10,
                            min.pts.bin = 25, bracket = "left", special.values = NULL)
{
  if (is.character(target) == TRUE) {
    target <- match(target, colnames(data))
  }
  if (is.character(id) == TRUE) {
    id_char <- id
    id <- match(id, colnames(data))
  } else if (is.numeric(id) == TRUE) {
    id_char <- colnames(data)[id]
  }
  if (missing(varcol)) {
    varcol <- (1:ncol(data))[c(-id, -target)]
  }
  if (is.character(varcol) == TRUE) {
    varcol <- match(varcol, colnames(data))
  }
  BinFun1 <- function(data, varcol, badcol, idcol, min.cat,
                      num.bins, min.pts.bin, bracket, special.values) {
    options(scipen = 9999999) #telling R to avoid all scientific notation ??
    if (is.numeric(data[, varcol]) == FALSE) { #For Non Numeric variables
      NAS <- data[which(is.na(data[, varcol])), c(idcol,
                                                  varcol)] #get NA rows with id and var col
      Norm <- data[which(!is.na(data[, varcol])), c(idcol,
                                                    varcol)] #get not NA rows with id and var col
      if (nrow(NAS) > 0) {
        NAS$Bins <- "Missing" #assign to NA rows
        Norm$Bins <- as.character(Norm[, 2]) #make sure the else are character
        final <- rbind(NAS, Norm)
      }
      else if (nrow(NAS) == 0) {
        Norm$Bins <- as.character(Norm[, 2])
        final <- Norm
      }
    }
    else if (is.numeric(data[, varcol]) == TRUE) { #numerical variables
      NAS <- data[which(is.na(data[, varcol])), c(idcol,
                                                  varcol)]
      spec <- data[which(data[, varcol] %in% special.values), #Special values
                   c(idcol, varcol)]
      Norm <- data[which(!is.na(data[, varcol]) & !(data[,
                                                         varcol] %in% special.values)), c(idcol, varcol)]
      if (nrow(NAS) > 0 & nrow(spec) > 0) {#if both NA and Special values exist
        NAS$Bins <- "Missing"
        spec$Bins <- as.character(spec[, 2])
        if (length(table(Norm[, 2])) < min.cat) { #if the number of categories are smaller than mincat
          Norm$Bins <- as.character(Norm[, 2])        #then the var is just saved as character
        }
        else if (length(table(Norm[, 2])) >= min.cat) {
          findbins <- binr::bins.greedy(Norm[, 2], nbins = num.bins, #:: allows to specify the specific function when multiple packages have the same function name
                                        minpts = min.pts.bin)
          if (bracket == "right") {
            Norm$Bins = as.character(cut(Norm[, 2], breaks = c(-Inf,
                                                               findbins$binhi[1:length(unique(findbins$binhi)) -
                                                                                1], Inf)))
          }
          else if (bracket == "left") {
            Norm$Bins = as.character(cut(Norm[, 2], breaks = c(-Inf,
                                                               findbins$binlo[1:length(unique(findbins$binlo))],
                                                               Inf), right = FALSE))
          }
        }
        final <- rbind(NAS, spec, Norm)
      }
      else if (nrow(NAS) > 0 & nrow(spec) == 0) {#when there are missing values but no special values
        NAS$Bins <- "Missing"
        if (length(table(Norm[, 2])) < min.cat) {
          Norm$Bins <- as.character(Norm[, 2])
        }
        else if (length(table(Norm[, 2])) >= min.cat) {
          findbins <- binr::bins.greedy(Norm[, 2], nbins = num.bins,
                                        minpts = min.pts.bin)
          if (bracket == "right") {
            Norm$Bins = as.character(cut(Norm[, 2], breaks = c(-Inf,
                                                               findbins$binhi[1:length(unique(findbins$binhi)) -
                                                                                1], Inf)))
          }
          else if (bracket == "left") {
            Norm$Bins = as.character(cut(Norm[, 2], breaks = c(-Inf,
                                                               findbins$binlo[1:length(unique(findbins$binlo))],
                                                               Inf), right = FALSE))
          }
        }
        final <- rbind(NAS, Norm)
      }
      else if (nrow(NAS) == 0 & nrow(spec) > 0) {#when there are sepcial values but no NAs
        spec$Bins <- as.character(spec[, 2])
        if (length(table(Norm[, 2])) < min.cat) {
          Norm$Bins <- as.character(Norm[, 2])
        }
        else if (length(table(Norm[, 2])) >= min.cat) {
          findbins <- binr::bins.greedy(Norm[, 2], nbins = num.bins,
                                        minpts = min.pts.bin)
          if (bracket == "right") {
            Norm$Bins = as.character(cut(Norm[, 2], breaks = c(-Inf,
                                                               findbins$binhi[1:length(unique(findbins$binhi)) -
                                                                                1], Inf)))
          }
          else if (bracket == "left") {
            Norm$Bins = as.character(cut(Norm[, 2], breaks = c(-Inf,
                                                               findbins$binlo[1:length(unique(findbins$binlo))],
                                                               Inf), right = FALSE))
          }
        }
        final <- rbind(spec, Norm)
      }
      else if (nrow(NAS) == 0 & nrow(spec) == 0) {#When no NAs and no special values
        if (nrow(spec) > 0) {
          spec$Bins <- as.character(spec[, 2])
        }
        if (length(table(Norm[, 2])) < min.cat) {
          Norm$Bins <- as.character(Norm[, 2])
        }
        else if (length(table(Norm[, 2])) >= min.cat) {
          findbins <- binr::bins.greedy(Norm[, 2], nbins = num.bins,
                                        minpts = min.pts.bin)
          if (bracket == "right") {
            Norm$Bins = as.character(cut(Norm[, 2], breaks = c(-Inf,
                                                               findbins$binhi[1:length(unique(findbins$binhi)) -
                                                                                1], Inf)))
          }
          else if (bracket == "left") {
            Norm$Bins = as.character(cut(Norm[, 2], breaks = c(-Inf,
                                                               findbins$binlo[1:length(unique(findbins$binlo))],
                                                               Inf), right = FALSE))
          }
        }
        final <- Norm
      }
    }
    Bins <- data.frame(final, stringsAsFactors = TRUE)
    Bins[, 3] <- as.factor(Bins[, 3])
    colnames(Bins)[3] <- paste(colnames(Bins)[2], "Bins",
                               sep = "_")
    return(c(Bins))
  }
  BinFun2 <- function(varcol) {
    BinFun1(data, varcol, badcol = target, idcol = id, min.cat,
            num.bins, min.pts.bin, bracket, special.values)
  }
  z <- lapply(varcol, BinFun2) #a big list of 1 item
  Merged = Reduce(function(x, y) merge(x, y, all.x = T, by = id_char, sort=F),
                  z) #turned z into a list of 3 items: id, var, var_bin
  Merged2 = merge(data[, c(id, target)], Merged, by = id_char, sort = F)
  Merged2 = Merged2[, c(1, 2, seq(4, ncol(Merged2), by = 2))]
  findLo = function(vec) {
    if (sapply(strsplit(vec, ","), length) == 1) {
      return(NA)
    }
    tmp = strsplit(vec, ",")
    lo = as.numeric(tmp[[1]][1])
    return(lo)
  }
  reorder_levels = function(vars) {
    x = gsub("\\[|\\]|\\(|\\)", "", levels(vars))
    vec = sapply(x, findLo)
    NewBins = factor(vars, levels(vars)[order(vec)])
    return(NewBins)
  }


  Merged2[, 3:ncol(Merged2)] = as.data.frame(lapply(Merged2[,
                                                            3:ncol(Merged2), drop = FALSE], reorder_levels))
  return(Merged2)

}
