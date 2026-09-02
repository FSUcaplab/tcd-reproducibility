# Step 01 - Table 1: participant characteristics.

# Locate the analysis modules, then hand off to start_step().
.file <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.here <- if (length(.file)) dirname(normalizePath(sub("^--file=", "", .file[1]), winslash = "/", mustWork = FALSE)) else file.path(getwd(), "Analysis_R")
source(file.path(.here, "R", "_setup.R"))

step <- start_step("02_table1_characteristics")

write_outputs(list(Table1_Characteristics = build_table1(step$df)), step$config)
