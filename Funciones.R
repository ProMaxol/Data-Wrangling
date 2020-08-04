generate_df <- function(x){
  df <-  data.frame(
    a = sample(letters, size = 10, replace = T)
    ,b = sample(1:10, size = 10, replace = T)
    ,c = sample(1:10, size = 10, replace = T)
  )
  return(df)
}
df_1 <-  generate_df()