

model <-

model <- readRDS(
      "3_Model_Output/Model_Foreign/Habitual/foreign_habitual_p5.rds"
    )


irf_draws <- compute_impulse_responses(
    model,
    horizon = 30
  )



dim(irf_draws)
  irf_draws










  # 1. Dimensions
dim(irf_draws)

# 2. Dimension names
dimnames(irf_draws)

# 3. Structure
str(irf_draws)

# 4. First response to first shock across horizons
irf_draws[1, 1, , 1]

# 5. First response to second shock across horizons
irf_draws[1, 2, , 1]

# 6. First shock's response across all variables at impact
irf_draws[, 1, 1, 1]

# 7. Second shock's response across all variables at impact
irf_draws[, 2, 1, 1]


names(model)


class(model)
str(model$posterior, max.level = 2)
names(model$posterior)
str(model$last_draw, max.level = 2)




dm <- model$last_draw$get_data_matrices()

class(dm)
names(dm)
str(dm, max.level = 2)



ident <- model$last_draw$get_identification()

class(ident)
names(ident)
str(ident, max.level = 2)