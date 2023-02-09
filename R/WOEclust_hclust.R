#' @title Hierarchical Variable Clustering
#' @description Function that implements hierarchical clustering on the variables to be used as a form of variable selection.
#' @param object A WOEProfet object containing dataframes with binned and WOE values.
#' @param id ID variable.
#' @param target A binary target variable.
#' @param num_clusts Number of desired clusters.
#' @param method Clustering method to be used. This should be one of "ward.D", "ward.D2", "single", "average", "mcquitty", "median",or "centroid".
#'
#' @return A dataframe indicates the assigned clusters for the predictor variables.
#' @export
#'
#' @examples
#' mydata <- ISLR::Default
#' mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
#' mydata$default<-ifelse(mydata$default=="Yes",1,0) ## Creating numeric binary target variable

WOEclust_hclust <- function (object, id, target, num_clusts, method = "ward.D")
{
  dat = object[[2]]
  if (is.character(id) == TRUE) {
    id = match(id, colnames(dat))
  }
  if (is.character(target) == TRUE) {
    target = match(target, colnames(dat))
  }
  woe_trans = t(dat[, -c(id, target)])
  dist.probes = stats::dist(woe_trans)
  probes.complete = stats::hclust(dist.probes, method = method)
  groups = stats::cutree(probes.complete, k = num_clusts)
  Memberships <- data.frame(groups)
  Memberships <- cbind(rownames(Memberships), Memberships)
  rownames(Memberships) <- NULL
  names(Memberships) <- c("Variable", "Group")
  Memberships$Variable = as.character(Memberships$Variable)
  infoVal = object[[3]]
  Memberships$IV = round(infoVal[, 2], 4)
  Memberships = Memberships[order(Memberships$Group, -Memberships$IV),
  ]
  Memberships$Variable = as.character(Memberships$Variable)
  return(Memberships)
}
