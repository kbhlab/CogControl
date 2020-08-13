equation_model7pre <- function(language, time_bin, trial_num) {
  if(language == 0) {
    return(0.08 + 0.39*language + 6.47*time_bin + 1.12*trial_num + 0.43*language*time_bin + 1.04*language*trial_num + 0.91*time_bin*trial_num + 1.11*language*time_bin*trial_num)
  } else if(language == 1) {
    return(0.08*0.39*language + 6.47*0.43*language*time_bin + 1.12*1.04*language*trial_num + 0.91*1.11*language*time_bin*trial_num)
  } else {
  print("error: language must be 0 (monolingual) or 1 (bilingual)")
  }
}

equation_model7pre(1, 0, 7) 
