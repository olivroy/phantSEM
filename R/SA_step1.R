#' Sensitivity Analysis Function Step 1
#' @description
#' `SA_step1()` is used to identify the phantom variables and generate names for their covariance parameters. The output of this function will be used in SA_step2().
#'
#' @param lavoutput The lavaan output object output from lavaan functions sem() or lavaan() when fitting your observed model.
#' @param mod_obs A lavaan syntax for the observed model.
#' @param mod_phant A lavaan syntax for the phantom variable model.
#' @returns a list containing the names of all phantom covariance parameters.
#' @import lavaan
#' @importFrom lavaan sem
#' @export
#' @examples
#' # covariance matrix
#' covmatrix <- matrix(c(
#'   0.25, 0.95, 0.43,
#'   0.95, 8.87, 2.66,
#'   0.43, 2.66, 10.86
#' ), nrow = 3, byrow = TRUE)
#' colnames(covmatrix) <- c("X", "M2", "Y2")
#'
#' # lavann syntax for observed model
#' observed <- " M2 ~ X
#'              Y2 ~ M2+X "
#'
#' # lavaan output
#' obs_output <- lavaan::sem(model = observed, sample.cov = covmatrix, sample.nobs = 200)
#'
#' # lavaan syntax for phantom variable model
#' phantom <- " M2 ~ M1 + Y1 + a*X
#'                Y2 ~ M1 + Y1 + b*M2 + cp*X "
#'
#' Step1 <- SA_step1(
#'   lavoutput = obs_output,
#'   mod_obs = observed,
#'   mod_phant = phantom
#' )
#'
SA_step1 <- function(lavoutput, # lavaan object
                     mod_obs, # lavaan syntax for observe model
                     mod_phant # model with phantom variables
) {
  fit <- lavoutput
  C <- fit@SampleStats@cov[[1]]
  colnames(C) <- fit@Data@ov.names[[1]]
  var_obs <- diag(C)

  obsvar <- unique(c(lavaan::lavParseModelString(mod_obs)$lhs, lavaan::lavParseModelString(mod_obs)$rhs))
  SAvar <- unique(c(lavaan::lavParseModelString(mod_phant)$lhs, lavaan::lavParseModelString(mod_phant)$rhs))

  # check for nested
  stopifnot(
    "Your phantom model must include all variables in your observed model" =
      obsvar %in% SAvar
  )

  # find the variables that are new in the phantom model -- those are the phantom vars
  phantom_names <- setdiff(SAvar, obsvar)

  # create expanded matrix
  newmat <- matrix(NA, nrow = length(SAvar), ncol = length(SAvar))
  newmat[1:length(obsvar), 1:length(obsvar)] <- stats::cov2cor(C)

  # newnames <- c(fit@Data@ov.names[[1]],phantom_names)
  # simpnames <- gsub(mediator,"M",oldnames[[2]])

  # add var names to expanded matrix
  colnames(newmat) <- c(fit@Data@ov.names[[1]], phantom_names)
  rownames(newmat) <- colnames(newmat)
  # put variance of phantom variables along diagonal
  var_phant <- c(var_obs, c(rep(1, length(phantom_names))))

  matrix_template <- newmat

  # put names of phantom variances/covariances into expanded matrix
  parname <- c()
  for (i in 1:nrow(newmat)) {
    for (j in 1:ncol(newmat)) {
      if (is.na(newmat[i, j]) & i == j) {
        tn <- (paste0("Var", rownames(newmat)[i]))
        newmat[i, j] <- tn
        parname <- c(parname, tn)
      } else if (is.na(newmat[i, j]) & i > j) {
        tn <- (paste0("Cov", rownames(newmat)[i], colnames(newmat)[j]))
        newmat[i, j] <- tn
        parname <- c(parname, tn)
      } else if (is.na(newmat[i, j]) & i < j) {
        tn <- (paste0("Cov", rownames(newmat)[j], colnames(newmat)[i]))
        newmat[i, j] <- tn
        parname <- c(parname, tn)
      }
    }
  }

  namemat <- newmat
  cov_names <- colnames(newmat)
  namemat <- outer(cov_names, cov_names, function(x, y) {
    ifelse(x == y, paste0("Var", x),
      ifelse(x < y, paste0("Cov", x, y),
        paste0("Cov", x, y)
      )
    )
  })

  obsname <- setdiff(unique(parname), namemat[lower.tri(namemat)])


  cov_map <- data.frame(
    covname = c(namemat),
    val = c(matrix_template),
    matrix(which(namemat > 0, arr.ind = TRUE), ncol = 2)
  )

  message(paste("Here are the phantom covariance matrix parameters (copy and paste and add values/names for step2):\n
              "))
  q <- paste0('"', sort(unique(parname)), '"', collapse = "= ,\n")
  cptext <- paste0("phantom_assignment <- list(", q, "= )")
  message(cptext)
  # print(paste0(sort(unique(parname)),collapse='","'),quote=FALSE) #old
  #message("\n Choose the names of the phantom covariances that you want to fix to single values and put in a vector. These will be used for the fixed_names argument in the SA_step2 function.  The phantom covariance parameters that you want to vary should be put in a list and used as the test_names argument. ")
  message("Here are the observed covariance matrix parameters:\n")
  obscov <- paste0(sort(setdiff(namemat[lower.tri(namemat)], unique(parname))),collapse=",")
  message(obscov,"\n")
  #print(sort(setdiff(namemat[lower.tri(namemat)], unique(parname))))
  message("Choose which values you want to use for your phantom covariances.")
  return(list(
    matrix_template = matrix_template,
    Phantom_covs = parname,
    named_matrix = namemat,
    obs_matrix_phant_names = newmat,
    mod_phant = mod_phant,
    variances = var_phant,
    cov_map = cov_map,
    step2syntax=cptext
  ))
}
