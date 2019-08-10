#'
#'
#' label.odk_select_multiple<-function(x){
#'   labels<-attributes(x)$labels
#'   lapply(x, function(y){
#'     y_labeled<-as.character(y)
#'     y_labeled[y %in% names(labels)]<-labels[y_labeled[y %in% names(labels)]]
#'     y_labeled
#'   }) %>% as_select_multiple(choices = labels)
#' }
#'
#'
#' #' @param x a character vector with concatenated select_multiple choices (for example 'c("choice_A choices_B", "choice_C")`)
#' #' @param choices list of options; equivalent to factor levels (in case some options were never selected but we want to track them regardless)
#' #' @param labels named vector with choice labels. the vector name is the value in `x`, the vector value is the label.
#' #' @param sep the delimeter used to separate the choices in each element of `x` ("choice_A choice_B" vs. "choice_A; choice_B"). uses regex.
#' #' @export
#' select_multiple <- function(x = character(), choices = NULL, labels = NULL, sep = " ") {
#'   # if(class(x)=='matrix' & typeof(x)=='logical'){
#'   # # return(gather_select_multiple(x))
#'   # }
#'   if(!is.list(x)){x<-vctrs::vec_cast(x,character())}
#'   new_select_multiple(x, choices, labels = labels, sep)
#' }
#'
#' #' @param x a character vector with concatenated select_multiple choices (for example 'c("choice_A choices_B", "choice_C")`)
#' #' @param choices list of options; equivalent to factor levels (in case some options were never selected but we want to track them regardless)
#' #' @param labels named vector with choice labels. the vector name is the value in `x`, the vector value is the label.
#' #' @param sep the delimeter used to separate the choices in each element of `x` ("choice_A choice_B" vs. "choice_A; choice_B"). uses regex.
#' new_select_multiple<-function(x = character(), choices = NULL, labels = NULL, sep = " "){
#'   if(!is.list(x)){vctrs::vec_assert(x, character())}
#'
#'   choices<-as.character(choices)
#'
#'   if(is.list(x)){
#'     x_split<-x
#'     # get choices from supplied choices, all factor levels and values:
#'     choices<- c(choices,levels(unlist(x)),unlist(x)) %>% unique
#'   }else{
#'     x<-as.character(x)
#'     # prepare factor levels
#'     x_split<-strsplit(x,split = sep)
#'     choices<-c(choices,as.character(unlist(x_split))) %>% unique
#'   }
#'   # convert to list of factor vectors
#'   x_split<-lapply(x_split,function(x){
#'     factor(x,levels = choices)
#'   })
#'   attributes(x_split)$choices<-choices
#'   # class(x_split)<-c('select_multiple')
#'   vctrs::new_vctr(x_split, class = "odk_select_multiple", labels = labels)
#'
#' }
#'
#'
#' format.odk_select_multiple<-function(x, ...) {
#'
#'   x<-purrr::map_chr(x,function(x){
#'     x<-as.character(x)
#'
#'     paste0(
#'       # number of selected items
#'       crayon::silver(crayon::italic(paste0(" (",length(x),") "))),
#'       # concatenated choices
#'       paste0(x, collapse = crayon::silver(crayon::italic(" & ")))
#'     )
#'
#'   })
#'
#'
#' x
#' }
#'
#' # basic type functions
#'
#' #' check if vector is of class odk_select_multiple
#' #' @param x a vector
#' #' @return TRUE if it is
#' #' @export
#' is_select_multiple<-function(x){
#'   inherits(x,'odk_select_multiple')
#' }
#'
#' as_select_multiple<-select_multiple
#'
#' # pretty printing
#'
#'
#' print.odk_select_multiple<-function(x, ...) {
#'   cat(format(x), sep = "\n")
#'   invisible(x)
#' }
#' library(vctrs)
#'
#'
#' vec_ptype_abbr.odk_select_multiple <- function(x, ...) {
#'   "s_mult"
#' }
#'
#' pillar_shaft.odk_select_multiple<- function(x, ...) {
#'   out <- format(x)
#'   out[is.na(x)] <- NA
#'   pillar::new_pillar_shaft_simple(out, align = "left", na_indent = 5)
#' }
#'
#'
#'
#'
#'
#' #' @importFrom pillar type_sum
#' #' @export
#' type_sum.odk_select_multiple <- function(x) {
#'   "s_mult"
#' }
#'
#' #' @export
#' as.logical.odk_select_multiple<-function(x){
#'   spread_select_multiple(x)
#' }

#' @export
mutate_select_multiple<-function(.data,...){
  mutation <- enquos(...)
  .data<-.data %>% rowwise %>% mutate(!!! mutation)
  class(.data)<-class(.data)[class(.data)!="rowwise_df"]
  .data
}

# fct_collapse.odk_select_multiple<-function(x,...){
#   as_select_multiple(lapply(x,fct_collapse,...))
# }
