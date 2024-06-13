#' Print bar charts for categorical variables to Power Point document
#'
#' @param cr_tabs_g dataset; set of crosstabulations in a long format (generated with `get_crosstabs()` or `get_weighted_crosstabs()`, "wide" parameter should be set to FALSE) with variables' and choices' labels added (could be done with `get_crosstabs_formatted()`)
#' @param ppt_file character; name of the target ppt file (with an extension)
#' @param ppt_layout character; name of a layout in a ppt file (suggested option - title and content)
#' @param ppt_theme character; name of a theme in a ppt file
#' @param indicator_col character; name of a variable, containing indicators
#' @param question_label character; name of a variable, containing questions' labels
#' @param choice_label character; name of a variable, containing choices' labels
#' @param unit_col character; name of a variable, containing a unit of disaggregation (independent variable)
#' @param value_col character; name of a variable, containing numeric values of proportions
#' @param type_col character; name of a variable, containing a question type (select_one or select_multiple)
#'
#' @return prints to the ppt file as a side effect
#' @export
#'
#' @examples
get_ppt_graphs_categorical <- function(cr_tabs_g, ppt_file, ppt_layout, ppt_theme, indicator_col = "indicator", question_label = "label::English", choice_label = "choice_label", unit_col = "unit", value_col = "prop", type_col = "type"){

  ms_blank <- mschart_theme(
    grid_major_line = fp_border(width = 0),
    grid_minor_line = fp_border(width = 0),
    axis_text = fp_text(font.family = "Open Sans", color = "#007DBC"),
    legend_text = fp_text(font.family = "Open Sans", color = "#007DBC")
  )


  cr_tabs_g <- cr_tabs_g |>
    filter(!is.na(!!sym(paste0(type_col))))

  indicators <- unlist(as.vector(unique(cr_tabs_g[paste0(indicator_col)])))

  ind <- indicators[1] #for testing the loop
  for(ind in indicators){

    temp_table <- cr_tabs_g |>
      filter(indicator == ind) |>
      rename(choice_label := paste0(question_label),
             question_lable := paste0(question_label))

    sl <- officer::read_pptx(ppt_file)
    sl <- officer::add_slide(sl, layout = ppt_layout, master = ppt_theme)

    if(temp_table[1, paste0(type_col)] == "select_one"){
      ms_g <- as_bar_stack(ms_barchart(temp_table, x = "unit", y = paste0(value_col), group = "choice_label"), dir = "horizontal", percent = T)
    }

    else if(temp_table[1, paste0(type_col)] == "select_multiple"){
      ms_g <- ms_barchart(temp_table, x = "unit", y = "prop", group = "choice_label")
    }

    ms_g <- set_theme(ms_g, ms_blank)
    sl <- officer::ph_with(sl, value = temp_table$question_lable[1], location = ph_location_type("title"))
    sl <- officer::ph_with(sl, value = ms_g, location = ph_location_type("body"))
    print(sl, target = ppt_file)
  }
  print(paste0("Graphs are put into the file ", ppt_file))
}


