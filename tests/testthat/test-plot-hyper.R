context("plot_hyper")

pacman::p_load(sperrorest)

# plot_hyper_rf Fri Jun  9 11:55:51 2017 ------------------------------

test_that("plot_hyper_rf works correctly with 'mtry' < 12", {
   data(ecuador) # Muenchow et al. (2012), see ?ecuador
   fo <- dem ~ slides + slope + hcurv + vcurv + log.carea + cslope

   out <- sptune_rf(fo, ecuador, accelerate = 16, nfold = 5,
   partition_fun = "partition_kmeans", rf_fun = "randomForest")

   out_plot <- plot_hyper_rf(out)

   expect_length(out_plot, 10)
})

test_that("plot_hyper_rf works correctly with 'mtry' > 12", {
   fo <- croptype ~ b82 + b83 + b84 + b85 + b86 + b87 + ndvi01 +
         ndvi02 + ndvi03 + ndvi04 + b25 + b44 + b47
   data(maipo)
   out <- sptune_rf(fo, maipo, accelerate = 32, nfold = 5,
                    coords = c("utmx", "utmy"),
                    partition_fun = "partition_kmeans",
                    rf_fun = "randomForest")

   expect_message(plot_hyper_rf(out))
})
