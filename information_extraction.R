# Title: Code to accompany the paper:
#
#    "Beyond Lexical Frequencies: Using R for Text
#     Analysis in the Digital Humanities"
#
# Date: 2017-10-30
# Authors: Taylor Arnold <taylor.arnold@acm.org>
# Data URL:

# Load the packages
library(cleanNLP)
cleanNLP::init_spaCy("en")
library(stringi)
stringi::stri_locale_set("en_GB")
library(readtext)
library(dplyr)

# Load the corpus (assumes you have "wiki_eu_en_GB"
# in the current working directory)
corpus <- readtext::readtext("wiki_eu_en_GB")
corpus$text <- stringi::stri_replace_all(corpus$text, "",
                         regex = "\\[[0-9]+\\]")
corpus$text <- stringi::stri_replace_all(corpus$text, "",
                         fixed = "[citation needed]")
corpus$lang <- stringi::stri_sub(corpus$doc_id, 9, 13)
corpus$country <- basename(corpus$doc_id)
corpus$country <- stringi::stri_sub(corpus$country, 1, -5)
corpus$country <- stringi::stri_replace_all(corpus$country, " ", fixed = "_")

# Annotate the corpus with spaCy
anno <- cleanNLP::tif_annotation(corpus)
print(anno)

# Find sentences containing the word "captial"
anno_capital <- dplyr::filter(anno, lemma == "capital")

# Create match rules for finding capital cities
df_match <- dplyr::data_frame(
              upos = "PROPN",
              lemma_source = c("be", "be", "be", "make",
                               "make", "make", "capital",
                                "city"),
              relation = c("dobj", "attr", "nsubj", "dobj",
                           "attr", "nsubj", "appos",
                           "appos"),
              rank = 1:8)

# Now find tokens according to the match rule
cc <- dplyr::semi_join(anno, anno_capital,
                       by = c("doc_id", "sid"))
cc <- dplyr::inner_join(cc, df_match)
cc <- dplyr::filter(cc, entity != country)
cc <- dplyr::filter(cc, lemma != country)
cc <- dplyr::filter(cc, !(entity_type %in% c("NORP", "ORG")))
cc <- dplyr::filter(cc, word_source != "was")
cc <- dplyr::arrange(cc, country, rank)
cc <- dplyr::select(cc, entity, country)
cc <- cc[!duplicated(cc$country),]

# Print out the results
sprintf("%s => %s", cc$country, cc$entity)


# Tested with the following sessionInfo()
#
# > sessionInfo()
# R version 3.4.1 (2017-06-30)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Sierra 10.12.6
#
# Matrix products: default
# BLAS: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRblas.0.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRlapack.dylib
#
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base
#
# other attached packages:
# [1] dplyr_0.7.4     readtext_0.50   stringi_1.1.5   cleanNLP_1.10.4
#
# loaded via a namespace (and not attached):
#  [1] Rcpp_0.12.13        assertthat_0.2.0    R6_2.2.2
#  [4] jsonlite_1.5        magrittr_1.5        httr_1.3.1
#  [7] rlang_0.1.2         data.table_1.10.4-2 bindrcpp_0.2
# [10] reticulate_1.2      tools_3.4.1         glue_1.1.1
# [13] compiler_3.4.1      pkgconfig_2.0.1     bindr_0.1
# [16] tibble_1.3.4


