#' \code{fit_lbspr}
#'
#' @param fish
#' @param fleet
#' @param length_comps counts of samples by length bin
#'
#' @return lbspr fits
#' @export
#'
fit_lbspr <- function(fish, fleet, length_comps)
{
  lbspr_fish <-  new("LB_pars")

  lbspr_fish@Species <- 'Spotted Babelfish'

  lbspr_fish@Linf <- fish$linf

  lbspr_fish@M <- fish$m

  lbspr_fish@MK <- fish$m / fish$vbk

  lbspr_fish@Steepness <- fish$steepness

  # lbspr_fish@CVLinf <- fish$cv_len

  lbspr_fish@L50 <- fish$length_50_mature

  lbspr_fish@L95 <- fish$length_95_mature

  lbspr_fish@Walpha <- fish$weight_a
  #
  lbspr_fish@Wbeta <- fish$weight_b

  lbspr_fish@SL50 <- fleet$length_50_sel

  lbspr_fish@SL95 <- fleet$length_50_sel + fleet$delta

  # lbspr_fish@FM <- f_v_m

  lbspr_fish@BinWidth <- 1

  lbspr_fish@L_units <- 'cm'

  # lbspr_sim <- LBSPRsim(lbspr_fish, Control = list(modtype = 'absel', Nage = fish$max_age))

  # plotSim(lbspr_sim)

  lbspr_lengths <-
    new("LB_lengths", LB_pars = lbspr_fish, dataType = 'freq')

  lbspr_lens <- length_comps %>%
    spread(year,numbers) %>%
    mutate(length_bin = length_bin + 0.5) %>%
    rename(LMids = length_bin) %>%
    as.matrix()


  lbspr_lengths@LData <- lbspr_lens[,-1] %>% as.matrix()

  lbspr_lengths@LMids <- lbspr_lens[,1] %>% as.numeric()

  lbspr_lengths@NYears <- ncol(lbspr_lens) - 1 # - 1

  lbspr_lengths@Years <- 1:lbspr_lengths@NYears

  lbspr_fit <- LBSPRfit(lbspr_fish, lbspr_lengths,Control = list(modtype = 'absel', Nage = fish$max_age/fish$time_step))

  out <- lbspr_fit@Ests


}