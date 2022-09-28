print_column_names <- function(df) {
  for (i in 1:ncol(df)) {
    print(paste(i," - ", colnames(df[i])))
  }
}