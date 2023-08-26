

render(
  input = here::here("szkielet", "performace_raport_nowySpisTresci.rmd"),                         # path to the template
  output_file = paste("Performance_report","_", MiesiacAnalizyTekst, RokAnalizy,".pdf", sep=""),  # name the output
  output_dir = here::here("output")                                                               # folder in which to put the output file
)


