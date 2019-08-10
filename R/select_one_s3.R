#' @export
select_one <- function(x, choices = NULL) {
  as_select_one(x, choices)
}

#' @export
as_select_one<-function(x, choices = NULL){
  choices<-as.character(choices)

  choices<- c(choices,levels(unlist(x)),unlist(x)) %>% unique

  x<- factor(x,levels = choices)

  class(x)<-c('select_one',class(x)) %>% unique
  return(x)

}


#' @export
c.select_one <- function(x, ...) {
  as_select_one(NextMethod())
}

#' @export
`[.select_one` <- function(x, i) {
  as_select_one(NextMethod())
}

#' @export
format.select_one <- function(x, ...) {
as.character(x)
}


pillar_shaft.select_one<- function(x, ...) {
  out <- format(x)
  out[is.na(x)] <- NA
  pillar::new_pillar_shaft_simple(out, align = "left", na_indent = 5)
}




#' @export
print.select_one <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' @importFrom pillar type_sum
#' @export
type_sum.select_one <- function(x) {
  "s_one"
}

#' @export
is.select_one<-function(x){
  select_one %in% class(x)
}



