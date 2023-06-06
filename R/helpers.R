
is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)

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
compute_tv_matrix <- function(teacher_data, M, type = "base") {
  
  year_index <- min(teacher_data$get.1) - 1
  
  mm <- tidyr::crossing(
    teacher_data |> dplyr::select(get.1) |> dplyr::distinct(),
    teacher_data |> dplyr::rename(syear = get.1)
  ) |>
    dplyr::filter(!is.na(class_mean), get.1 != syear) |>
    dplyr::select(-n_tested) |>
    dplyr::mutate(
      years = syear - year_index,
      year_i = get.1 - year_index
    )
  
  if (type == "f") {
    mm <- mm |> dplyr::filter(get.1 + 1 != syear) # leave next year out
  }
  if (type == "l") {
    mm <- mm |> dplyr::filter(get.1 - 1 != syear) # leave prior year out
  }
  
  mm <- mm |>
    tidyr::nest(
      .by = c(get, get.1, .group)
    )
  
  suppressMessages(
    n <- dplyr::bind_cols(
      cpp_tv(mm$data, M),
      mm
    ) |>
      dplyr::select(1:3) |>
      rlang::set_names("tv", "get", "get.1") |>
      dplyr::select(get, get.1, tv)
  )

  
  n
  
}
