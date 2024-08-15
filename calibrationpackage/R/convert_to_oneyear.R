#' function to convert 3 yr NESARC TPs to 1 yr TPs
#' this outputs a TP
#' @param
#' @keywords microsimulation markov model
#' @export
#' @examples
#' convert_to_one_year
convert_to_one_year <- function(probs){
  library(Matrix)

  percentage_error <- function(A, B) {
    error <- sqrt(sum((B - (A %*% A %*% A))^2)) / sqrt(sum(B^2))
    return(error)
  }

  # Function to approximate the cube root of a matrix
  approximate_cube_root <- function(matrix) {
    eig <- eigen(matrix)
    eigenvalues <- eig$values
    eigenvectors <- eig$vectors

    if (any(Im(eigenvalues) != 0)) {
      stop("Matrix has complex eigenvalues, unsuitable for cube root computation.")
    }

    cube_root_eigenvalues <- eigenvalues^(1/3)
    cube_root_matrix <- eigenvectors %*% diag(cube_root_eigenvalues) %*% solve(eigenvectors)

    cube_root_matrix <- Re(cube_root_matrix)

    # Ensure the matrix is stochastic
    cube_root_matrix[cube_root_matrix < 0] <- 0
    cube_root_matrix <- sweep(cube_root_matrix, 1, rowSums(cube_root_matrix), FUN = "/")

    return(cube_root_matrix)
  }

  # Function to perform hill climbing optimization
  hill_climber <- function(initial_matrix, target_matrix) {
    current_matrix <- initial_matrix
    best_error <- percentage_error(current_matrix, target_matrix)

    improvement <- TRUE
    while (improvement) {
      improvement <- FALSE
      for (r in 1:nrow(current_matrix)) {
        for (c in 1:ncol(current_matrix)) {
          for (change in c(-0.001, 0.001)) {
            temp_matrix <- current_matrix
            temp_matrix[r, c] <- temp_matrix[r, c] + change

            if (temp_matrix[r, c] < 0) {
              temp_matrix[r, c] <- 0
            } else if (temp_matrix[r, c] > 1) {
              temp_matrix[r, c] <- 1
            }

            # Normalize row to ensure it's stochastic
            temp_matrix[r, ] <- temp_matrix[r, ] / sum(temp_matrix[r, ])

            new_error <- percentage_error(temp_matrix, target_matrix)

            if (new_error < best_error) {
              best_error <- new_error
              current_matrix <- temp_matrix
              improvement <- TRUE
            }
          }
        }
      }
    }
    return(list(final_matrix = current_matrix, final_error = best_error))
  }

  samples <- unique(problist$samplenum)
  cat <- unique(problist$cat)

  new_probs <- list()

  for(i in unique(samples)){
    for(j in unique(cat)){
      probs_matrix <- problist %>% filter(samplenum==i & cat==j)
      statefrom = probs_matrix$alc4.factor_1
      probs_matrix <- probs_matrix[1:4,4:7]
      stateto <- colnames(probs_matrix)
      initial_approximation <- approximate_cube_root(probs_matrix)
      result <- hill_climber(initial_approximation, probs_matrix)
      colnames(result$final_matrix) <- stateto
      new_probs[[paste(i,j)]] <- data.frame(samplenum=i,
                                            cat=j,
                                            statefrom=statefrom,
                                            result$final_matrix)
    }
  }

  new_probs <- do.call(rbind,new_probs)

  # # Step 1: Initial approximation using eigendecomposition
  # initial_approximation <- approximate_cube_root(probs_matrix)
  #
  # # Step 2: Apply the hill climber algorithm to refine the approximation
  # result <- hill_climber(initial_approximation, probs_matrix)

  return(probs)
  }
