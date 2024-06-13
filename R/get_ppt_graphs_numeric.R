#' Print pseudo-boxplots for numeric variables to a Power Point document
#'
#' @param num_tabs dataframe; set of summary statistics for numeric variables in a long format, with variable labels added (can be generated with `get_numerics_summaries()`, "wide" parameter is set to FALSE and followed by `get_numerics_formatted()`)
#' @param ppt_file character; name of the target ppt file (with an extension)
#' @param ppt_layout character; name of a layout in a ppt file (suggested option - title and content)
#' @param ppt_theme character; name of a theme in a ppt file
#' @param indicator_col character; name of a variable, containing indicators
#' @param question_label character; name of a variable, containing questions' labels
#' @param stat character; name of a variable, containing an indication of statistics
#' @param unit_col character; name of a variable, containing a unit of disaggregation (independent variable)
#' @param value_col character; name of a variable, containing numeric values of proportions
#' @param exclude_stats character; vector of statistics' names that should be excluded from plotting
#'
#' @return prints to the ppt file as a side effect
#' @export
#'
#' @examples
get_ppt_graphs_numeric <- function(num_tabs, ppt_file, ppt_layout, ppt_theme, indicator_col = "indicator", question_label = "indicator", stat = "stat", unit_col = "unit", value_col = "value", exclude_stats = c("observations count", "mean")){
  ms_blank <- mschart_theme(
    grid_major_line = fp_border(width = 0),
    grid_minor_line = fp_border(width = 0),
    axis_text = fp_text(font.family = "Open Sans", color = "#007DBC"),
    legend_text = fp_text(font.family = "Open Sans", color = "#007DBC")
  )

num_tabs <- num_tabs |>
  mutate(stat = case_when(
    stat == "min" ~ "1_min",
    stat == "q1" ~ "2_q1",
    stat == "median" ~ "3_median",
    stat == "q3" ~ "4_q3",
    stat == "max" ~ "5_max"
  ))

  indicators <- unlist(as.vector(unique(num_tabs[paste0(indicator_col)])))

  ind <- indicators[1] #for testing the loop
  for(ind in indicators){

    temp_table <- num_tabs |>
      filter(indicator == ind) |>
      rename(stat := paste0(stat)) |>
      filter(!stat %in% exclude_stats)

    sl <- officer::read_pptx(ppt_file)
    sl <- officer::add_slide(sl, layout = ppt_layout, master = ppt_theme)

    ms_g <- as_bar_stack(ms_barchart(temp_table, x = "unit", y = paste0(value_col), group = "stat"), dir = "horizontal")

    ms_g <- chart_data_fill(ms_g, values = c(`1_min` = "#FFFFFE",
                                             `2_q1` = "#C2DBEF",
                                             `3_median` = "#4892CE",
                                             `4_q3` = "#4892CE",
                                             `5_max` = "#C2DBEF"))

    ms_g <- set_theme(ms_g, ms_blank)
    sl <- officer::ph_with(sl, value = temp_table$indicator[1], location = ph_location_type("title"))
    sl <- officer::ph_with(sl, value = ms_g, location = ph_location_type("body"))
    print(sl, target = ppt_file)

  }

  print(paste0("Graphs are put into the file ", ppt_file))
}




