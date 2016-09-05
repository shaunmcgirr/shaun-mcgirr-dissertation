## Separate script to handle output of mixed models
## Based on http://svmiller.com/blog/2015/02/quasi-automating-the-inclusion-of-random-effects-in-rs-stargazer-package/

# Helper function
insertrow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

# Output the models without random effects
Tables <- stargazer(final_model_disqualifications_baseline,
                    final_model_disqualifications_random_agency,
                    final_model_disqualifications_random_agency_fixed_region,
                    final_model_disqualifications_random_agency_random_region,
          column.labels = c("Baseline", "Agency RE", "Age RE/Reg FE", "Age RE/Reg RE"),
          title = "Mixed-effects models of agency corruption across all regions",
          # omit = "factor",
          # omit.labels = "Region fixed effects",
          # se = list(NA, NA, NA, NA),
          dep.var.labels = c("Corruption (mean proportion disqualified by agency)"),
          multicolumn = TRUE,
          # order = c(5, 6, 7, 1, 2, 3),
          #covariate.labels = c("Agency spend (log base 10)", "Mean listing duration", "Mean bidders applied", "Mean supplier favoritism", "\\textbf{Median product commonness}", "\\textbf{Median auction efficiency}", "\\textbf{Commonness x Efficiency}"),
          # df = F,
          omit.stat = c("adj.rsq", "f", "ser", "ll", "aic", "bic"), #
          notes.label = "Standard errors in parentheses",
          # notes = "Note: Some procedures use no initial price, so `bunching' is ruled out",
          # out = "../dissertation-text/tables/agencies_in_regions_models.tex",
          label = "agencies-in-regions-models",
          # float.env = "sidewaystable",
          # single.row = T,
          initial.zero = FALSE,
          style = "apsr")

Tables <- as.data.frame(Tables)
Tables$Tables <- as.character(Tables$Tables)
# Tables

# Add lines to help with formatting
r <- 7
Tables <- insertrow(Tables, "\\resizebox*{\\textwidth}{!}{%", r) # Starts command to scale table to page
r <- length(Tables$Tables)
Tables <- insertrow(Tables, "} % Closing out page scaling", r) # Starts command to scale table to page

# Replace header with a row saying "Fixed effects" (too confusing?)

# Find where you want to put in the random effect. In our case, this is right after the last fixed effect. Line: 25.
# r <- 25
r <- Tables %>%
  add_rownames(var = "RowName") %>%
  filter(grepl("Constant & ", Tables)) %>%
  select(RowName) %>%
  as.numeric() + 2
  
# Create some standard rows to add.
randomeffect <- "{\\bf Random Effects} & & \\\\"
hline <- "\\hline"
newline <- "\\\\"
agency_header <- "{\\it Agency level predictors} & & \\\\"
region_header <- "{\\it Region level predictors} & & \\\\"


Tables <- insertrow(Tables, hline, r)
Tables <- insertrow(Tables,randomeffect,r+1)
Tables <- insertrow(Tables,hline,r+2)
# Type Tables into the terminal to see where it put it.
Tables

# Number the models for easier referencing
M1 <- final_model_disqualifications_baseline
M2 <- final_model_disqualifications_random_agency
M3 <- final_model_disqualifications_random_agency_fixed_region
M4 <- final_model_disqualifications_random_agency_random_region

# Which terms in these models are agency- vs region-level?
agency_level_re <- c("Commonness", "Efficiency", "Commonness:Efficiency")
# region_level_re # Better define as difference

# Know your random effect groupings. In this case, they're "replicate" and "recipe:replicate". 
# Know their order too. Recipe:replicate comes first. Let's get the number of groupings in each random effect.
  # num.recipe.replicate <- sapply(ranef(M1),nrow)[1]
  # num.replicate <- sapply(ranef(M1),nrow)[2]
num.region <- sapply(ranef(M1),nrow)[1]
# Note: if this varies with the different models you're running, grab them (i.e. sub out M1 with M2, M3, or whatever)

# Let's get the standard deviation of the random effect now.
  # stddev.M1.recipe.replicate <- attributes(VarCorr(M1)$"recipe:replicate")$stddev
stddev.M1.region <- attributes(VarCorr(M1)$"Region")$stddev
  # stddev.M2.recipe.replicate <- attributes(VarCorr(M2)$"recipe:replicate")$stddev
stddev.M2.region <- attributes(VarCorr(M2)$"Region")$stddev
stddev.M2.region.alone <- attributes(VarCorr(M2)$"Region")$stddev[1]
stddev.M2.region.rounded <- round(stddev.M2.region, 3)
  # 
stddev.M3.region <- attributes(VarCorr(M3)$"Region")$stddev
stddev.M3.region.alone <- attributes(VarCorr(M3)$"Region")$stddev[1]
stddev.M3.region.rounded <- round(stddev.M3.region, 3)
  #
stddev.M4.region.alone <- attributes(VarCorr(M4)$"Region")$stddev[1]
# stddev.M4.region.all <- attributes(VarCorr(M4)$"Region")$stddev
stddev.M4.region.agencylevel <- with(attributes(VarCorr(M4)$"Region"), stddev[names(stddev) %in% agency_level_re])
stddev.M4.region.regionlevel <- with(attributes(VarCorr(M4)$"Region"), stddev[!names(stddev) %in% agency_level_re & names(stddev) != "(Intercept)"])
# rm(stddev.M4.region.all) # For safety!
stddev.M4.region.agencylevel <- round(stddev.M4.region.agencylevel, 3)
stddev.M4.region.regionlevel <- round(stddev.M4.region.regionlevel, 3)

# Assemble agency-level first
stddev.region.predictors.M1 <- cbind(names(stddev.M2.region[2:4]), "")
stddev.region.predictors.M2 <- cbind(names(stddev.M2.region[2:4]), unname(stddev.M2.region.rounded)[2:4])
stddev.region.predictors.M3 <- cbind(names(stddev.M3.region[2:4]), unname(stddev.M3.region.rounded)[2:4])
stddev.region.predictors.M4.a <- cbind(names(stddev.M4.region.agencylevel[1:3]), unname(stddev.M4.region.agencylevel)[1:3])
stddev.region.predictors.M4.r <- cbind(names(stddev.M4.region.regionlevel), unname(stddev.M4.region.regionlevel))
# paste(stddev.region.predictors.M2[,1], " & ", stddev.region.predictors.M2[,2], "&", stddev.region.predictors.M2[,2], "\\\\")

# Start creating some rows to add soon.
  # number.of.recipe.replicate <- paste("\\# of Recipe:Replicate & ", num.recipe.replicate, "&", num.recipe.replicate, "\\\\")
  # stddev.recipe.replicate <- paste("Recipe:Replicate Standard Deviation & ", round(stddev.M1.recipe.replicate, 3), "&", round(stddev.M2.recipe.replicate, 3), "\\\\")
number.of.region <-paste("\\# of Regions & ", num.region, "&", num.region, "&", num.region, "&", num.region, "\\\\")
stddev.region <- paste("- Regions std. dev. & ", round(stddev.M1.region, 3), "&", round(stddev.M2.region.alone, 3), "&", round(stddev.M3.region.alone, 3), "&", round(stddev.M4.region.alone, 3), "\\\\")
stddev.region.a.p1 <- paste("- ", stddev.region.predictors.M2[1,1], "& &", stddev.region.predictors.M2[1,2], "&", stddev.region.predictors.M3[1,2], "&", stddev.region.predictors.M4.a[1,2], "\\\\")
stddev.region.a.p2 <- paste("- ", stddev.region.predictors.M2[2,1], "& &", stddev.region.predictors.M2[2,2], "&", stddev.region.predictors.M3[2,2], "&", stddev.region.predictors.M4.a[2,2], "\\\\")
stddev.region.a.p3 <- paste("- ", stddev.region.predictors.M2[3,1], "& &", stddev.region.predictors.M2[3,2], "&", stddev.region.predictors.M3[3,2], "&", stddev.region.predictors.M4.a[3,2], "\\\\")
#stddev.region.p4 <- paste(stddev.region.predictors.M2[4,1], " & ", stddev.region.predictors.M2[4,2], "&", stddev.region.predictors.M3[4,2], "&", stddev.region.predictors.M4.a[4,2], "\\\\")
# Logic of above: take the name from M2, then empty column, then M2 estimates (col 2) etc
stddev.region.r.p1 <- paste("- ", stddev.region.predictors.M4.r[1,1], "& & & &", stddev.region.predictors.M4.r[1,2], "\\\\")
stddev.region.r.p2 <- paste("- ", stddev.region.predictors.M4.r[2,1], "& & & &", stddev.region.predictors.M4.r[2,2], "\\\\")


# Let's add them now.
Tables <- insertrow(Tables,number.of.region,r+3)
Tables <- insertrow(Tables,stddev.region,r+4)
# Tables <- insertrow(Tables,newline,r+5)
Tables <- insertrow(Tables,agency_header,r+5)
Tables <- insertrow(Tables,stddev.region.a.p1,r+6)
Tables <- insertrow(Tables,stddev.region.a.p2,r+7)
Tables <- insertrow(Tables,stddev.region.a.p3,r+8)
# Tables <- insertrow(Tables,newline,r+10)
Tables <- insertrow(Tables,region_header,r+9)
Tables <- insertrow(Tables,stddev.region.r.p1,r+10)
Tables <- insertrow(Tables,stddev.region.r.p2,r+11)

# 
write.table(Tables,file="6-Present/mixed_effects_table.tex",sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)
write.table(Tables,file="../dissertation-text/tables/mixed_effects_table.tex",sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)

