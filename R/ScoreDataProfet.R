#' @title Score a Validation Data Set
#' @description Function that scores the validation set using the scorecard from the ScorecardProfet object created by
#' the training set.
#' @param data The validation data set, which should be binned in the same way as the scorecard in the card argument.
#' @param card A ScorecardProfet object. The object should be created by using the training set split from the same dataframe
#' as the validation set.
#' @param id ID variable.
#' @param target A binary target variable.
#' @param varcol Variables on the validation set to be used for scoring. WE MIGHT DETELTE THIS, NOT REALLY NEEDED
#'
#'
#' @return A dataframe of scored validation set.
#' @export
#'
#' @examples
#' #skip for now
ScoreDataProfet <- function (data, card, id, target, varcol)
{
  score = card[[1]]
  `%>%` <- magrittr::`%>%`
  if (is.character(target) == TRUE) {
    tar_char <- target
    target <- match(target, colnames(data))
  } else if (is.numeric(target) == TRUE) {
    tar_char <- colnames(data)[target]
  }

  if (is.character(id) == TRUE) {
    id_char <- id
    id <- match(id, colnames(data))
  } else if (is.numeric(id) == TRUE) {
    id_char <- colnames(data)[id]
  }
  if (missing(varcol)) {
    #varcol <- colnames(data)[c(-id, -target)]
    varcol <- unique(score$Attribute)
  }
  if (is.numeric(varcol) == TRUE) {
    varcol <- colnames(data)[varcol]
    var <- match(gsub("\\.","_", varcol), colnames(data)) #maybe move this up below varcol
  }

  Fun1 <- function(data, id, target, varcol, score)
  {
    if (!is.numeric(data[, varcol])) { #For Non Numeric variables
      tb = score[score$Attribute == varcol,] #subset for each variable

      #test = merge(data[,c(id,var)], tb, by.x = varcol, by.y = "Bins", all.x = TRUE)
      test1 <- dplyr::left_join(tb, data[,c(id_char,varcol)]
                         ,by=c("Bins" = varcol))
      #test2<- right_join(tb, data[,c(id_char,varcol)]
      #                   ,by=c("Bins" = varcol))
      df <- test1 %>% dplyr::select(dplyr::all_of(id_char), "Bins", "Points")
      colnames(df)[c(2,3)] <- c(varcol, paste(varcol, "Points", sep = "_"))
      return(df)
    }


    ### use this one for now
    else if (is.numeric(data[,varcol]) == TRUE) {
      print("The binned variables should not be numeric")
      stop()
    }
  }
  Fun2 <- function(varcol) {
    Fun1(data, id, target, varcol, score)
  }
  z <- lapply(varcol, Fun2)
  Merged = Reduce(function(x, y) merge(x, y, all.x = T, by = id_char), z)
  Merged2 = merge(data[, c(id_char, tar_char)], Merged, by = id_char, sort = F)
  Merged2$Score = rowSums(Merged2[, seq(4, ncol(Merged2), by = 2)], na.rm = TRUE)
  #might need to add an argument for only one variable situation
  return(Merged2)
}
