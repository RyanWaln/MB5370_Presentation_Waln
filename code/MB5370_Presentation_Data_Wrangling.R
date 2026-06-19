---
  #title: "MB5370 Presentation Waln"
  #author: "Ryan Waln"
  #date: "`r format(Sys.time(), '%d %B, %Y')`"
  #output: html_document
  ---
  ## Introduction
  
  # This file is meant to format the AIMs coral data for the class project 
  
  # Houskeeping
  rm(list=ls())
objects()  


## Rules for tidy data:

#   Each variable must have its own column.

#   Each observation must have its own row.

#   Each value must have its own cell.

#   ALWAYS: Put each dataset in a tibble and each variable in a column.


# Have github start tracking files
usethis::use_github() 
