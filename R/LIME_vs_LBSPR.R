# set.seed(42)
#
# library(tidyverse)
# library(LIME)
# library(LBSPR)
#
# vbk = 0.21
# linf = 64.58
# lwa = 0.0245
# lwb = 2.79
# S50 = 30
# selex_input = "length"
# M50 = 34
# maturity_input = "length"
# binwidth = 1
# CVlen = 0.2
# SigmaR = 0
# M = 0.2
# rho = 0
#
#
# lh <-
#   create_lh_list(
#     vbk = vbk,
#     linf = linf,
#     lwa = lwa,
#     lwb = lwb,
#     S50 = S50,
#     selex_input = selex_input,
#     M50 = M50,
#     maturity_input = maturity_input,
#     binwidth = binwidth,
#     CVlen = CVlen,
#     SigmaR = SigmaR,
#     M = M,
#     rho = rho,
#     F1 = 0.2,
#     SigmaF = 0
#   )
#
# simmed_fish <- sim_pop(
#   lh = lh,
#   Nyears = 50,
#   Fdynamics = 'Constant',
#   Rdynamics = 'BH',
#   nburn = 5,
#   modname = 'blah',
#   seed = 42,
#   init_depl = 0,
#   comp_sample = 1000,
#   pool = T,
#   Nyears_comp = 50,
#   mismatch = F
# )
#
# plot(1:length(simmed_fish$LF), simmed_fish$LF)
#
#
# lbspr_lengths <- simmed_fish$LF %>%
#   t() %>%
#   as_data_frame() %>%
#   set_names(1:dim(.)[2])
#
# lbspr_lengths <-
#   data.frame(LMids = simmed_fish$mids %>% as.numeric(),
#              lbspr_lengths,
#              check.names = F)
#
#
# babel_lengths <- new("LB_lengths")
#
# babel_lengths@LMids <- simmed_fish$mids
#
# babel_lengths@LData <- lbspr_lengths %>% as.matrix()
#
# babel_lengths@Years <-
#   as.numeric(lbspr_lengths %>% select(-LMids) %>% colnames())
#
# babel_lengths@NYears <- length(babel_lengths@Years)
#
# babel_pars <- new("LB_pars")
#
# babel_pars@Species <- "babelfish"
#
# babel_pars@Linf <- linf
#
# babel_pars@L50 <- M50
#
# babel_pars@L95 <-  M50 + 1
#
# babel_pars@MK <- M / vbk
#
# babel_pars@BinWidth <- 1
#
# babel_pars@CVLinf <- 0.1
#
# babel_fit <- LBSPRfit(
#   babel_pars,
#   babel_lengths,
#   yrs = 1:babel_lengths@NYears,
#   Control = list(modtype = 'absel')
# )
#
#
# real <-
#   data_frame(year = 1:50,
#              spr = simmed_fish$SPR_t,
#              name = 'True Value')
#
# lbspr <-
#   data_frame(year = 1:50,
#              spr = babel_fit@SPR,
#              name = 'LBSPR Estimate')
#
# spr_comp_plot <- real %>%
#   bind_rows(lbspr) %>%
#   ggplot(aes(year, spr, color = name)) +
#   geom_line(size = 2) +
#   scale_y_continuous(limits = c(0, NA))
#
# real <-
#   data_frame(year = 1:50,
#              fvm = simmed_fish$F_t / lh$M,
#              name = 'True Value')
#
# lbspr <-
#   data_frame(year = 1:50,
#              fvm = babel_fit@FM,
#              name = 'LBSPR Estimate')
#
# fvm_comp_plot <- real %>%
#   bind_rows(lbspr) %>%
#   ggplot(aes(year, fvm, color = name)) +
#   geom_line(size = 2) +
#   scale_y_continuous(limits = c(0, NA))
#
# fvm_comp_plot