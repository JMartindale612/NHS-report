# render

# https://www.youtube.com/watch?v=mFEEkOYvAZg&ab_channel=BrunoRodrigues
# He renders an R markdown report using a for loop for each country in the list

for(i in countries){
  rmarkdown::render(input = "example_Report.Rmd",
                    output_file = paste0("report_", i, "_", Sys.Date(), ".docx"),
                    rmarkdown::word_document(),
                    params = list(country = i, year = 1990))
}