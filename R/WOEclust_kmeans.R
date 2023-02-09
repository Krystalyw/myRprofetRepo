#' @title K-means Variable Clustering
#' @description Function that implements kmeans variable clusteting to be used as a form of variable selection.
#' @param object A WOEProfet object containing dataframes with binned and WOE values.
#' @param id ID variable.
#' @param target A binary target variable.
#' @param num_clusts Number of desired clusters.
#'
#' @return A dataframe indicates the assigned clusters for the predictor variables.
#' @export
#'
#' @examples
#' mydata <- ISLR::Default
#' mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
# 'mydata$default<-ifelse(mydata$default=="Yes",1,0) ## Creating numeric binary target variable

#' ## create two new variables from bivariate normal
#' sigma <- matrix(c(45000,-3000,-3000, 55000), nrow = 2)
#' set.seed(10)
#' newvars <- MASS::mvrnorm(nrow(mydata),
#'                         mu=c(1000,200), Sigma=sigma)

WOEclust_kmeans <- function (object, id, target, num_clusts)
{
  dat = object[[2]]
  if (is.character(id) == TRUE) {
    id = match(id, colnames(dat))
  }
  if (is.character(target) == TRUE) {
    target = match(target, colnames(dat))
  }
  dat2 <- dat[, -c(1, 2)]
  Groups <- ClustOfVar::kmeansvar(X.quanti = dat2[, 1:ncol(dat2)],
                                  X.quali = NULL, num_clusts, iter.max = 150, nstart = 1,
                                  matsim = FALSE)
  Memberships <- data.frame(Groups$cluster)
  Memberships <- cbind(rownames(Memberships), Memberships)
  rownames(Memberships) <- NULL
  names(Memberships) <- c("Variable", "Group")
  varcol = colnames(dat2) #?
  varcol = match(varcol, colnames(dat)) #?
  infoVal = object[[3]]
  Memberships$IV = round(infoVal[, 2], 4)
  Memberships = Memberships[order(Memberships$Group, -Memberships$IV), ]
  Memberships$Variable = as.character(Memberships$Variable)
  return(Memberships)
}
