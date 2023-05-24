

# a function
vectorToStripeDiag <- function(vector_m) {
  
  dim = length(vector_m)
  M = matrix(0, dim, dim)
  for (i in 1:dim) {
    for (j in 1:dim) {
      M[j,i]=vector_m[abs(j-i)+1]
    }
  }
  
  return(M)
  
} 

# compute tv function
compute_tv_matrix <- function(teacher_data, M) {
  
  year_index <- min(teacher_data$get.1) - 1
  
  mm <- tidyr::crossing(
    teacher_data |> dplyr::select(get.1) |> dplyr::distinct(),
    teacher_data |> dplyr::rename(syear = get.1)) |>
    dplyr::filter(!is.na(class_mean), get.1 != syear) |>
    dplyr::mutate(
      years = syear - year_index
    ) |>
    # necessary to make the factor capture every year
    dplyr::bind_rows(
      tibble::tibble(years = 1 : ncol(M))
    ) |>
    dplyr::mutate(years = as.factor(years)) |>
    dplyr::filter(!is.na(get.1)) |>
    dplyr::mutate(
      year_i = get.1 - year_index,
      scores = class_mean |> as.matrix(),
      A = model.matrix(~years + 0)
      # A = model.matrix(~.$years + 0)
    ) |>
    tidyr::nest(
      .by = c(get, get.1)
    )
  
  n <- mm |>
    dplyr::mutate(
      tv = lapply(data, function(df) 
        (M[df$year_i[1], ] %*% t(df$A)) %*% # phi
          (solve(df$A %*% M %*% t(df$A) + diag(1 / df$weight, nrow = nrow(df$A), ncol = nrow(df$A))) %*% # inverse
             df$scores)) # scores
    ) |>
    dplyr::select(get, get.1, tv) |>
    tidyr::unnest(tv) |>
    dplyr::mutate(tv = as.numeric(tv))
  
  n
  
}
