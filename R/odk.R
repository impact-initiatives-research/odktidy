#' create ODK object
#' @param x a data.frame or tibble with the recorded surveys
#' @param questions the survey sheet in the kobo questionnaire as a data.frame
#' @param choices the choices sheet in the kobo questionnaire as a data.frame
#' @param ... (ignored)
#' @export
as_odk<-function(x,questions,choices,...){


  questionnaire<-koboquest::load_questionnaire(x,questions,choices)

  #
  q_types<-sapply(names(x),purrr::possibly(questionnaire$question_type,NA))
  not_identified<-which(is.na(q_types))
  not_identified_is_sm_choice<-sapply(names(q_types)[not_identified],questionnaire$question_is_sm_choice)
  q_types[not_identified[not_identified_is_sm_choice]]<-"sm_choice"

  x<-x[,-which(q_types=="sm_choice")]
  q_types<-q_types[-which(q_types=="sm_choice")]
  sm_main_cols<-which(q_types=="select_multiple")
  sm_main_cols_choices<-lapply(names(sm_main_cols),questionnaire$question_get_choices)
  sm_main_cols_labels <-purrr::map2(sm_main_cols_choices, names(sm_main_cols),questionnaire$question_get_choice_labels)

  sm_main_cols_labels<-purrr::map2(sm_main_cols_choices,sm_main_cols_labels,function(choices,labels){
    names(labels) = choices
    return(labels)
  })

  x[,sm_main_cols]<-purrr::pmap(list(x = x[,sm_main_cols],
                                     choices = sm_main_cols_choices,
                                     labels = sm_main_cols_labels),
                                as_select_multiple)


  so_cols<-which(sapply(names(x),questionnaire$question_is_select_one))
  so_cols_choices<-lapply(names(so_cols),questionnaire$question_get_choices)
  x[,so_cols]<-purrr::map2(x[,so_cols],so_cols_choices,as_select_one)

  odk<-tibble::new_tibble(x,questionnaire = questionnaire, subclass = 'odk_tbl')
}



is_skipped<-function(odk, var){
  var<-deparse(substitute(var))
  attributes(odk)$questionnaire$question_is_skipped(odk,var)
}
