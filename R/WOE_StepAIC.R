#' @title Stepwise Variable Selection
#' @description Function that implements stepwise model selection by AIC to be used as a form of variable selection.
#' @param object A WOEProfet object containing dataframes with binned and WOE values.
#' @param id ID variable.
#' @param target A binary target variable.
#' @param IVfilter Number of desired clusters.
#' @param direction The mode of stepwise search direction. This should be one of "both", "backward", or "forward", with a default of "both".
#'
#' @return A list of four items.
#' @export
#'
#' @examples
#' ##skip for now

WOE_StepAIC <- function(object, id, target, IVfilter, direction = "both")
{
  data = object[[2]]
  BinWOE = object[[1]]
  IV = object[[3]]
  if (is.character(target) == FALSE) {
    target = colnames(data)[target]
  }
  if (missing(IVfilter)) {
    IVfilter = 0
  }
  Variable = NULL
  IV <- IV %>% dplyr::filter(IV >= IVfilter) %>% dplyr::mutate(Variable = paste(Variable, "_WOE", sep=""))

  #fm_data <- data %>% dplyr::select(1,2,target,IV$Variable)
  # full_md <- glm(bad ~.,
  #                   data = fm_data[,-1], family = 'binomial')
  form = stats::as.formula(paste(target, paste(IV$Variable, collapse = " + "),
                          sep = " ~ "))
  logmod_f <- stats::glm(form, data = data, family = "binomial")
  step_mod <- MASS::stepAIC(logmod_f, direction = direction, trace = 0)
  vars <- colnames(step_mod$model[,-1])
  vars_b <- gsub("_WOE", "", vars)
  vars_b_w <- c(rbind(vars_b, vars))
  data_out <- data %>% dplyr::select(1,2,dplyr::all_of(vars))
  BinWOE <- BinWOE %>% dplyr::select(1,2,dplyr::all_of(vars_b_w))
  item <- list(BinWOE = BinWOE, WOE = data_out, Model = step_mod, IV = IV)
  return(item)

  # form1 = as.formula(paste(target, paste(vars, collapse = " + "),
  #                         sep = " ~ "))
  # logmod_r <- glm(form1, data=data, family = "binomial")
}
