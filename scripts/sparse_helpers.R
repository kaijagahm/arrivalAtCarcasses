sparsify_A <- function(data_list) {
  A <- data_list$A
  d <- dim(A)
  nz <- which(A != 0, arr.ind = TRUE)
  nz <- nz[order(nz[,1], nz[,2], nz[,3], nz[,4], nz[,5]), ]
  vals <- A[nz]
  rm(A); data_list$A <- NULL; gc()
  
  row_id <- (((nz[,1] - 1) * d[2] + (nz[,2] - 1)) * d[3] + (nz[,3] - 1)) * d[4] + nz[,4]
  counts <- tabulate(row_id, nbins = prod(d[1:4]))
  
  data_list$A_nnz     <- length(vals)
  data_list$A_val     <- as.numeric(vals)
  data_list$A_col     <- as.integer(nz[,5])
  data_list$A_row_ptr <- as.integer(c(1, cumsum(counts) + 1))
  data_list
}

patch_model_sparse <- function(model_code) {
  code <- as.character(model_code)
  
  old_decl <- "array[N_networks, K, T_max] matrix[P, P] A;  // network matrices"
  pat <- "dot_product\\(A\\[network, trial, time_step\\]\\[id, \\],\\s*Z\\[trial\\]\\[time_step, \\]\\)"
  
  has_decl <- grepl(old_decl, code, fixed = TRUE)
  has_dot  <- grepl(pat, code)
  
  # Asocial models: no network matrix, nothing to patch
  if (!has_decl && !has_dot) {
    message("No network matrix A found; model is asocial, returning unchanged.")
    return(code)
  }
  
  # Partial match means the generated code has changed shape; fail loudly
  if (xor(has_decl, has_dot)) {
    stop("Found only one of: A declaration / A dot_product calls. ",
         "Model template may have changed (whitespace or comment text?).")
  }
  
  new_decl <- paste(
    "int<lower=0> A_nnz;",
    "  vector[A_nnz] A_val;",
    "  array[A_nnz] int<lower=1, upper=P> A_col;",
    "  array[N_networks * K * T_max * P + 1] int<lower=1> A_row_ptr;",
    sep = "\n")
  code <- sub(old_decl, new_decl, code, fixed = TRUE)
  
  fn <- "
  real A_dot(vector A_val, array[] int A_col, array[] int A_row_ptr,
             int K, int T_max, int P,
             int network, int trial, int time_step, int id, row_vector z) {
    int r = (((network - 1) * K + (trial - 1)) * T_max + (time_step - 1)) * P + id;
    real s = 0;
    for (k in A_row_ptr[r]:(A_row_ptr[r + 1] - 1)) s += A_val[k] * z[A_col[k]];
    return s;
  }
"
  if (grepl("functions\\s*\\{", code)) {
    code <- sub("functions\\s*\\{", paste0("functions {", fn), code)
  } else {
    code <- paste0("functions {", fn, "}\n", code)
  }
  
  n_found <- lengths(regmatches(code, gregexpr(pat, code)))
  message("Replaced ", n_found, " dot_product calls (expect 6)")
  code <- gsub(pat,
               "A_dot(A_val, A_col, A_row_ptr, K, T_max, P, network, trial, time_step, id, Z[trial][time_step, ])",
               code)
  code
}
