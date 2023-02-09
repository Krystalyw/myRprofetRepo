#' @title Visualizing WOE and Target Rates
#' @description Function generating three plots: WOE value for each bin, target rate for each bin, and the frequency for each bin.
#' @param data Dataframe containing binned values and a binary target variable.
#' @param target A numeric binary target variable.
#' @param var  The desired WOE binned attribute to visualize.
#' @param color A hexadecimal value representing a specific color.
#'
#' @return three plots.
#' @export
#'
#' @examples
#' mydata <- ISLR::Default

#'mydata$ID = seq(1:nrow(mydata)) ## make the ID variable
#'mydata$default<-ifelse(mydata$default=="Yes",1,0) ## Creating numeric binary target variable

#'binned <- New_BinProfet(mydata, id= "ID", target= "default", num.bins = 5) ## Binning variables

#'WOEplotter_New(binned, target= "default", var= "income_Bins")

WOEplotter_New <- function (data, target, var, color = "#0066CC")
{
  if (is.character(var) == FALSE) {
    var = colnames(data)[var]
  }
  if (is.character(target) == FALSE) {
    target = colnames(data)[target]
  }
  dat = data[, c(target, var)]
  NumBad <- stats::aggregate(dat[, 1] ~ dat[, 2], data = dat, FUN = sum)
  NumGood <- stats::aggregate((ifelse(dat[, 1] == 1, 0, 1)) ~ dat[,
                                                           2], data = dat, FUN = sum)
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
  dat$Bins = dat[, 2]
  Temp2 <- merge(dat, Temp, by = "Bins", all.x = TRUE)
  dat = Temp2[, c(1, 2, 4)]
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
  e <- gridExtra::grid.arrange(a, c, b, ncol = 3, nrow = 1,
                               top = paste(var, "\n IV=", IVF))
}
