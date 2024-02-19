nullToNA <- function(x) {
x[sapply(x, is.null)] <- NA
return(x)
}


## Round of 2
round_of_two <- function(a){
  return(round(a,2))
}


add_collapse_content <- function(x, id) {
  tagList(
    tags$button(
      "data-toggle" = "collapse",
      "data-target" = paste0("#", id),
      "More Info"
    ),
    div(
      "id" = id,
      "class" = "collapse",
      x
    )
  )  %>% as.character()
}


price_Change <- function(title, price){
  if (price > 0) 
  {
    return(paste0('<span class="b">',title,' : ',round(as.numeric(price),2),'&nbsp<i></i></span>&nbsp'))
  } else 
  {
    return(paste0('<span class="c">',title,' : ',round(as.numeric(price),2),'&nbsp<i></i></span>&nbsp'))
  }
}

# main_data$pc <- do.call(paste, c(main_data[Stock_price_Change_Columns], sep = '_'))

paste_function <- function(df, df_col = c(a, b)){
  for (i in df_col){
    return(substr(paste0(df,'$',df_col,collapse = ",", sep = ",'_'"), 1,nchar(paste0(df,'$',df_col,collapse = ",", sep = ",'_'"))-4))
  }
}

