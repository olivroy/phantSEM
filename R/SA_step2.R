#' Step 2 of sensitivity analysis function
#'
#' `SA_step2()` is used to assign values to the phantom covariances. There are three options for assigning values to the phantom covariances: 1. Fix phantom covariance to a numeric value (i.e., 0 or 1), 2. Fix phantom covariance to be equal to another covariance, or 3. Test different values for the phantom covariance.
#' @param phantom_assignment A list of all phantom parameter names (copied from `SA_step1()` output) which assigns them to be equal to ONE of the following: 1) an observed parameter name, 2) a single numeric value, 3) a sequence of values, or 4) another phantom variable that has been set equal to 1-3.
#' @param step1 The output object created in `SA_step1()`
#' @returns A list containing test covariance matrices that the phantom model will be fit to.
#' @import corpcor
#' @import tidyr
#' @import dplyr
#' @import lavaan
#' @importFrom lavaan sem
#' @export
#'
#' @examples
# covariance matrix
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
#' phantom_assignment <- list(
#'   "CovM1X" = 0,
#'   "CovY1M1" = "CovY2M2",
#'   "CovY1X" = 0,
#'   "VarM1" = 1,
#'   "VarY1" = 1,
#'   "CovM1M2" = seq(0, .6, .1),
#'   "CovY1Y2" = "CovM1M2",
#'   "CovY1M2" = seq(-.6, .6, .1),
#'   "CovM1Y2" = "CovY1M2"
#' )
#' Step2 <- SA_step2(
#'   phantom_assignment = phantom_assignment,
#'   step1 = Step1
#' )
#'
SA_step2 <- function(phantom_assignment, # list of all phantom parameter names which tells the function what they should be equal to
                     step1 # previous step
) {
  pa <- phantom_assignment
  matrix_template <- step1[[1]]
  parname <- step1[[2]]
  namemat <- step1[[3]]
  newmat <- step1[[4]]
  mod_phant <- step1[[5]]
  var_phant <- step1[[6]]
  cov_map <- step1[[7]]



  # covariance names fixed to single numeric value
  fvaltable <- data.frame(matrix(ncol = 2, nrow = 0))
  for (i in seq_along(pa)) {
    if (is.numeric(pa[[i]]) && length(pa[[i]]) == 1) {
      fvaltable <- rbind(fvaltable, c(names(pa)[i], pa[[i]]))
    }
  }
  colnames(fvaltable) <- c("name", "value")



  # covariance names set to another named value
  charvaltable <- unlist(pa[sapply(pa, is.character)])
  charvaltable <- stats::setNames(charvaltable, NULL)


  # this returns the rows that have an observed parameter value
  char_obs <- subset(cov_map, (cov_map$covname %in% charvaltable & !is.na(cov_map$val)))
  # covariances equal to other values
  eqseq <- subset(cov_map, cov_map$covname %in% charvaltable & is.na(cov_map$val))[, 1]

  # covariances that are going to be varied
  seqvaltable <- lapply(pa, function(x) if (is.numeric(x) & length(x) > 1) x)
  seqvaltable <- seqvaltable[!sapply(seqvaltable, is.null)]


  # covariances that are fixed to observed covariances -- by name
  fnametable <- data.frame(matrix(ncol = 2, nrow = 0))
  for (i in seq_along(pa)) {
    if (is.character(pa[[i]]) && length(pa[[i]]) == 1 && !(pa[[i]] %in% eqseq)) {
      fnametable <- rbind(fnametable, c(names(pa)[i], pa[[i]]))
    }
  }
  colnames(fnametable) <- c("name", "value")

  # this is the fixed name and values
  fixed <- rbind(fnametable, fvaltable)

  # covariances that are fixed to other phantom covariances
  phantomnametable <- data.frame(matrix(ncol = 2, nrow = 0))
  for (i in seq_along(pa)) {
    if (is.character(pa[[i]]) && length(pa[[i]]) == 1 && (pa[[i]] %in% eqseq)) {
      phantomnametable <- rbind(phantomnametable, c(names(pa)[i], pa[[i]]))
    }
  }
  colnames(phantomnametable) <- c("name", "value")


  # this creates list of parameter names and values that will be varied
  testnamelist <- list(NA)
  testnametable <- data.frame(matrix(nrow = 0, ncol = 1))
  # counter
  j <- 0
  for (i in seq_along(pa)) {
    if (is.numeric(pa[[i]]) && length(pa[[i]]) > 1) {
      j <- j + 1
      #print(j)
      testnametable <- rbind(testnametable, names(pa)[i])
      testnamelist[[j]] <- list(name = names(pa)[i], values = pa[[i]])
    }
  }
  colnames(testnametable) <- c("value")

  # find remaining phantom parameters that will be set to default test values
  defaultname <- setdiff(unique(parname), names(pa))
if (length(defaultname>0)){
  defaultnamelist <- list(NA)
  defaultnametable <- data.frame(matrix(nrow = 0, ncol = 1))
  # counter
  j <- j
  for (i in length(defaultname)) {
    j <- j + 1
   # print(j)
    testnametable <- rbind(testnametable, defaultname[i])
    testnamelist[[j]] <- list(name = defaultname[i], values = seq(-.3, .3, .1))
  }} else{}


  # make sure the phantom variable name is correct --removed
 # for (i in 1:nrow(phantomnametable)) {
#    print(phantomnametable[i, 2] %in% testnametable[, 1])
 # }

  # make sure that when there are phantom variables equal to other phantom variables, the phantom variable name is correct
  stopifnot(
    "The phantom variable name that another phantom variable was set equal to is not spelled correctly. Please compare to output from SA_step1" =
      all(phantomnametable[, 2] %in% testnametable[, 1])
  )


  ## Unique Use Cases
  # Case 1 -- all phantom variables are fixed to numeric values OR
  # Case 2 -- all Phantom Variables fixed to observed variables
  if (length(unique(parname)) == nrow(fvaltable) | length(unique(parname)) == nrow(fnametable)) {
    #### assigning names that were previously function arguments ####
    fixed_names <- fixed[, 1]
    fixed_values <- fixed[, 2]
    fixed <- fixed_names
    ref <- fixed_values


    corlist <-
      rep(list((list(
        matrix(NA, nrow = nrow(newmat), ncol = ncol(newmat)), c(NA)
      ))), 1)

    corlist[[1]] <- matrix_template

    covlist <-
      rep(list((list(
        matrix(NA, nrow = nrow(newmat), ncol = ncol(newmat)), c(NA)
      ))), 1)

    covlist[[1]] <- cor2cov_lavaan(R = corlist[[1]], sds = sqrt(var_phant))

    combos <- list(NA)
  } else {
    # Create a new list
    new_testnamelist <- list()
    # Loop over the list of lists
    for (i in seq_along(testnamelist)) {
      # Get the name and values of the current list
      name <- testnamelist[[i]]$name
      values <- testnamelist[[i]]$values

      # Find the rows in the data frame that matches the value
      rows <- phantomnametable[phantomnametable$value == name, ]

      # Add the name(s) from the data frame to the current list
      new_testnamelist[[i]] <- list(name = c(name, rows$name), values = values)
    }
    #### assigning names that were previously function arguments ####
    fixed_names <- fixed[, 1]
    fixed_values <- fixed[, 2]
    test_names <- lapply(new_testnamelist, "[[", 1)
    test_values <- lapply(new_testnamelist, "[[", 2)
    fixed <- fixed_names
    ref <- fixed_values



    # reference name indices
    ind_ref <- sapply(fixed_values, function(x) {
      which(namemat == x, arr.ind = TRUE)
    })
    vals <- c(rep(0, length(ind_ref)))


    for (i in 1:length(ind_ref)) {
      # print(length(ind[[i]]))
      # if(length(ind[[i]])<1)
      if (is.na(ind_ref[[i]][2])) {
        #print(is.na(ind_ref[[i]][2]))
        vals[i] <- as.numeric(fixed_values[i])
        #print(vals[i])
      } else {
        vals[i] <- (matrix_template[ind_ref[[i]]])
      }
    }

    # put reference values into covariance matrix
    for (i in 1:length(fixed)) {
      index <- which(newmat == fixed[i], arr.ind = TRUE)[1, ]
      matrix_template[index[[1]], index[[2]]] <- as.numeric(vals[i])
      matrix_template[index[[2]], index[[1]]] <- as.numeric(vals[i])
      # matrix <- sub(fixed[i],as.numeric(vals[i]),newmat)
    }

    # variables that are NA
    naind <- which((is.na(matrix_template) & lower.tri(matrix_template)), arr.ind = TRUE)

    # names of remaining parameters that don't have values
    name_na <- namemat[naind]

    namemat[naind] %in% fixed_names

    # check if there are any remaining NA values
    phant_default <- matrix(NA, nrow = 0, ncol = 1)
    for (j in 1:nrow(naind)) {
      if (name_na[j] %in% unlist(test_names)) {

      } else {
        #print(name_na[j])
        phant_default <- rbind(phant_default, name_na[j])
      }
      # print(name_na[j] %in% unlist(test_names))
    }


    # combos <- reduce(test_values,crossing)
    combos <- expand.grid(test_values)

    ## this code is causing issues
    # if (length(test_names)==1){ #if there is one vector of test names then the columns need to correspond to those params
    #  colnames(combos) <- unlist(test_names)
    # }
    ##

    # else { #this code is causing issues
    colnames(combos) <- lapply(test_names, paste0, collapse = ",") # combines names of params
    # } #this code is causing issues
    combos <- as.data.frame(combos)
    corlist <-
      rep(list((list(
        matrix(NA, nrow = nrow(newmat), ncol = ncol(newmat)), c(NA)
      ))), nrow(combos))



    for (i in 1:nrow(combos)) {
      tmat <- matrix_template
      if (length(test_names) == 1) {
        tn <- unlist(test_names)
        for (j in 1:length(tn)) {
          tmat[(which(namemat == tn[j], arr.ind = TRUE))] <- combos[i, ] # combos would only be a vector here so do not index column
        }
      } else {
        for (j in 1:length(test_names)) {
          unlist(test_names)
          for (k in 1:length(test_names[[j]])) {
            tmat[(which(namemat == test_names[[j]][k], arr.ind = TRUE))] <- combos[i, j]
          }
        }
      }
      tmat[upper.tri(tmat)] <- t(tmat)[upper.tri(tmat)]
      corlist[[i]][[1]] <- tmat
      corlist[[i]][[2]] <- corpcor::is.positive.definite(tmat)
    }

    # } else if (!is.null(test_values) & is.null(test_names)) { message("Error: You must provide lists for BOTH (test_values) and (test_names), or leave these null.  You have provided testvalues but have not specified parameters in test_names. ")
    # } else if (is.null(test_values) & !is.null(test_names)) {message("Error: You must provide lists for BOTH (test_values) and (test_names), or leave these null. You have provided parameter names (test_names) but have not specified the values you want to test (test_values).")}




    covlist <-
      rep(list((list(
        matrix(NA, nrow = nrow(newmat), ncol = ncol(newmat)), c(NA)
      ))), nrow(combos))

    for (i in 1:nrow(combos)) {
      covlist[[i]] <- cor2cov_lavaan(R = corlist[[i]][[1]], sds = sqrt(var_phant))
    }
  }


  return(list(
    mod_phant = mod_phant,
    var_phant = var_phant,
    combos = combos,
    correlation_matrix_list = corlist,
    covariance_matrix_list = covlist
  ))
}
