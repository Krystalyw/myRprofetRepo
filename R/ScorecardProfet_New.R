#' @title Scorecard Builder
#' @description Function that fits a logistic regression models and scores points for each bin and calculates observations' total score.
#' @param object A WOEProfet object containing dataframes with binned and WOE values.
#' @param id ID variable.
#' @param target A binary target variable.
#' @param varcol Vector of WOE variables to be used in the logistic regression model.
#' @param PDO Points to Double Odds.
#' @param BaseOdds Base Odds.
#' @param BasePts Base Points.
#' @param reverse Logical. If true, higher points corresponds to a lower probability of being target.
#'
#' @return A list of a dataframe of scorecard, a scored input dataframe, and a summary of the logistic model used to built the scorecard.
#' @export
#'
#' @examples
#' #skip it for now
ScorecardProfet_New <- function (object, id, target, varcol, PDO = 100, BaseOdds = 10,
                                 BasePts = 1000, reverse = FALSE)
{
  data = object[[2]]
  if (is.character(target) == TRUE) {
    tar_num <- match(target, colnames(data))
  }
  else if (is.numeric(target) == TRUE) {
    tar_num <- target
    target <- colnames(data)[target]
  }
  if (is.character(id) == TRUE) {
    id <- match(id, colnames(data))
  }
  if (missing(varcol)) {
    varcol <- colnames(data)[c(-id, -tar_num)]
  }
  if (is.numeric(varcol) == TRUE) {
    varcol <- colnames(data)[varcol]
  }
  if (reverse == TRUE) {
    data[, varcol] = -data[, varcol]
  }
  factor = PDO/log(2)
  offset = BasePts - (factor * log(BaseOdds))
  form = stats::as.formula(paste(target, paste(varcol, collapse = " + "),
                          sep = " ~ "))
  logmod <- stats::glm(form, data = data, family = "binomial")
  logmod.summary = summary(logmod)
  modelcoef = as.numeric(logmod$coefficients)
  modelterms = as.factor(labels(logmod$coefficients))
  a = modelcoef[1]
  n = length(modelcoef) - 1
  datBinWOE = object[[1]]
  calc_points <- function(datBinWOE, varcol, modelcoef, modelterms,
                          a, n, factor, offset) {
    pat = gsub("_WOE", "", varcol[1])
    score_bins = data.frame(datBinWOE[, colnames(datBinWOE)[which(stringr::str_detect(colnames(datBinWOE),
                                                                                      pat))]])
    score_bins$score = round(-(datBinWOE[, varcol] * modelcoef[which(modelterms ==
                                                                       varcol)] + a/n) * factor + offset/n, 0)
    colnames(score_bins)[3] = paste(varcol, "Points", sep = "_")
    return(score_bins)
  }
  card_wrapper <- function(varcol) {
    calc_points(datBinWOE, varcol, modelcoef, modelterms,
                a, n, factor, offset)
  }
  z = lapply(varcol, card_wrapper)
  tmp = data.frame(ID = datBinWOE[, id], default = datBinWOE[,
                                                             target], as.data.frame(z))
  colnames(tmp)[1:2] = colnames(datBinWOE)[1:2]
  tmp$Score = rowSums(tmp[, seq(5, ncol(tmp), by = 3)], na.rm = TRUE)
  lvl_order <- function(varcol) {
    tmp_lvls = as.numeric(tmp[, gsub("_WOE", "", varcol)])
    return(tmp_lvls)
  }
  lvls = lapply(varcol, lvl_order)
  lvls2 = as.data.frame(lvls)
  name_lvls <- function(varcol) {
    cnames = paste(gsub("_WOE", "", varcol), "Level", sep = "_")
    return(cnames)
  }
  colnames(lvls2) = sapply(varcol, name_lvls)
  tmp2 = data.frame(tmp, lvls2)
  scard <- function(varcol) {
    attribute = gsub("_Bins_WOE", "", varcol)
    sql_state = paste(paste("Select '", attribute, "' as Attribute,", sep = ""),
                      paste(attribute, "Bins", sep = "_"), "as Bins,",
                      paste(attribute, "Bins_WOE", sep = "_"), "as WOE,",
                      paste(attribute, "Bins_WOE_Points", sep = "_"), "as Points from",
                      deparse(substitute(tmp2)), "group by", paste(attribute,
                                                                   "Bins_Level", sep = "_"))
    return(sqldf::sqldf(sql_state))
  }
  chk = lapply(varcol, scard)
  chk2 = do.call("rbind", chk)
  tmp3 = list(Scorecard = chk2, Results = tmp, GLMSummary = logmod.summary)
  return(tmp3)
}
