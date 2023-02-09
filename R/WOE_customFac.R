#' @title Custom Binning Factor Variables
#' @description Function that bins a factor variable based on user inputted factor levels, plots the information on the new bins,
#' and returns a list contains a dataframe of the newly binned values and id column and more items.
#' @param data Dataframe containing the target variable and desired factor variables to be binned.
#' @param var A specific factor attribute to be binned.
#' @param id The unique id variable in the dataframe. Must be specified.
#' @param target A binary target variable. Must be specified.
#' @param new_levels A vector the same length as the number of levels for the categorical variable containing the new factor levels. Must be specified.
#' @param color A hexadecimal value representing a specific color.
#' @param plot Logical. The default is TRUE which generates the plots.
#'
#' @return A list contains a dataframe of the newly binned values and other information.
#' @export
#'
#' @examples
#' #ship for now
WOE_customFac <- function (data, var, id, target, new_levels, color = "#0066CC", plot = TRUE)
{
  if (is.numeric(var) == TRUE) {
    var = colnames(data)[var]
  }
  if (is.numeric(id) == TRUE) {
    id = colnames(data)[id]
  }
  if (missing(color)) {
    color = "#0066CC"
  }
  if (missing(target)) {
    print("ERROR: Target variable input is missing.")
    return()
  }
  var_bins = data[, var]
  old_levels = levels(var_bins)

  fact = data.frame(A = as.character(old_levels), B = as.character(new_levels))

  counter = unique(fact[, 2])
  lookup = NULL
  for (i in counter) {
    tmp = subset(fact, fact$B == i)
    lookup = rbind(lookup, c(i, paste(tmp$A, collapse = "/"))) #combine levels at this step (if exists)
  }
  lookup = data.frame(lookup)

  if(is.numeric(new_levels) == TRUE){
    new_levels = as.character(lookup[match(new_levels, lookup[, 1]), 2])
    levels(var_bins) = new_levels

    var_bins_sort <- data.frame(A=as.numeric(counter), B=levels(var_bins))
    var_bins_sort <- var_bins_sort[order(var_bins_sort$A),]
    new_levels <- var_bins_sort[,2]
    var_bins <- factor(var_bins, levels = new_levels)

  } else if (is.numeric(new_levels) == FALSE){
    new_levels = as.character(lookup[match(new_levels, lookup[, 1]), 1]) #for char
    levels(var_bins) = new_levels
  }

  var_name = paste(var, "_Bins", sep = "")
  dat = data.frame(data[, c(id,target)], var_bins)

  NumBad <- stats::aggregate(dat[, 2] ~ dat[, 3], data = dat, FUN = sum)
  NumGood <- stats::aggregate((ifelse(dat[, 2] == 1, 0, 1)) ~ dat[,
                                                           3], data = dat, FUN = sum)
  IVF1_i <- (NumBad[, 2]/sum(NumBad[, 2])) - (NumGood[, 2]/sum(NumGood[,
                                                                       2]))
  IVF2_i <- log((NumBad[, 2]/sum(NumBad[, 2]))/(NumGood[, 2]/sum(NumGood[,
                                                                         2])))
  IVF3_i <- log(((NumBad[, 2] + 0.5)/(sum(NumBad[, 2]) + 0.5))/((NumGood[,
                                                                         2] + 0.5)/(sum(NumGood[, 2]) + 0.5)))
  IVF23_i <- ifelse(IVF2_i == -Inf | IVF2_i == Inf, IVF3_i,
                    IVF2_i)
  IVF_i <- IVF1_i * IVF23_i
  IVF <- round(sum(IVF_i), 4)
  Temp <- data.frame(NumBad[, 1], IVF23_i)
  colnames(Temp) = c("Bins", "woe")
  #  dat$Bins = dat[, 3]
  colnames(dat)[3] = "Bins"
  #  Temp2 <- merge(dat, Temp, by = "Bins", all.x = TRUE)
  Temp2 <- plyr::join(dat, Temp, by = 'Bins', match = 'all')
  dat = Temp2[, c(3, 2, 4)] #Bins, target, woe
  WeOfEv = dat[, 3]
  FinBins = dat[, 1]
  dat[, 4] <- ifelse(dat[, 2] == 1, 0, 1)
  Check2 <- data.frame(stats::aggregate(dat[, 2] ~ dat[, 3] + dat[,
                                                           1], data = dat, FUN = mean))
  WoE = Bins = TargetRate = Freq = NULL
  names(Check2) <- c("WoE", "Bins", "TargetRate")
  Check3 <- data.frame(table(dat[, 1]))
  names(Check3) <- c("Bins", "Freq")
  Check4 <- merge(Check2, Check3, by = "Bins")
  a <- ggplot2::ggplot() + ggplot2::geom_bar(data = Check4,
                                             ggplot2::aes(x = Bins, y = WoE), fill = color,
                                             stat = "identity") + ggplot2::theme(legend.position = "none",
                                                                                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = "", y = "WoE")
  b <- ggplot2::ggplot() + ggplot2::geom_bar(data = Check4,
                                             ggplot2::aes(x = Bins, y = Freq), fill = color,
                                             stat = "identity") + ggplot2::theme(legend.position = "none",
                                                                                 axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = "", y = "Bin Frequency")
  c <- ggplot2::ggplot() + ggplot2::geom_bar(data = Check4,
                                             ggplot2::aes(x = Bins, y = TargetRate),
                                             fill = color, stat = "identity") + ggplot2::theme(legend.position = "none",
                                                                                               axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::labs(x = "", y = "Target Rate")
  if (plot == TRUE) {
    e <- gridExtra::grid.arrange(a, c, b, ncol = 3, nrow = 1,
                                 top = paste(var_name, "\n IV=", IVF))
  }

  newbin <- data.frame(data[,id], var_bins)
  names(newbin) <- c(id, paste(var, "_Bins", sep = ""))
  item <- list(NewBin = newbin,
               BinWOE = Temp2,
               IV = IVF,
               vars = Check4)
  return(item)
}
