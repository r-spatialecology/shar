#' plot_sum_stat
#'
#' @description plot_sum_stat
#'
#' @param reconstruction Result list of the dot pattern reconstruction with
#' multiple marks.
#'
#' @details
#' Calculates and visualises various summary statistics for the results of
#' multi-marks point pattern reconstruction.
#'
#' @return ggplot object
#'
#' @aliases plot_sum_stat
#' @rdname plot_sum_stat
#'
#' @export
plot_sum_stat <- function(reconstruction) {

  # Data import from the results of point pattern reconstruction.
  k_func_recon <- list()
  pcf_func_recon <- list()
  dbh_markcorr_func_recon <- list()
  species_markcorr_func_recon <- list()

  rmin       <- 0
  divisor    <- "r"
  kernel_arg <- "epanechnikov"

  if(nchar(names(reconstruction[1])) == 9){

    n_repetitions     <- 1
    rmax              <- as.numeric(reconstruction$Parameter_setting$rmax)
    rcount            <- as.numeric(reconstruction$Parameter_setting$rcount)
    rpresented        <- min(c(as.numeric(reconstruction$window[2]),as.numeric(reconstruction$window[4])))
    r                 <- seq(rmin, if(rpresented >= rmax*2 ){rmax*2}else{rpresented}, length.out = rcount)
    bw                <- as.numeric(reconstruction$Parameter_setting$bw)

    ppp_reference <- spatstat.geom::as.ppp(reconstruction$reference, reconstruction$window)
    reconstruction    <- list(reconstruction)
  } else {

    n_repetitions     <- reconstruction[[1]]$Parameter_setting$n_repetitions
    rmax              <- as.numeric(reconstruction[[n_repetitions]]$Parameter_setting$rmax)
    rpresented        <- min(c(as.numeric(reconstruction[[n_repetitions]]$window[2]),as.numeric(reconstruction[[n_repetitions]]$window[4])))
    rcount            <- as.numeric(reconstruction[[n_repetitions]]$Parameter_setting$rcount)
    r                 <- seq(rmin, if(rpresented <= rmax*2 ){rmax*2}else{rpresented}, length.out = rcount)
    bw                <- as.numeric(reconstruction[[n_repetitions]]$Parameter_setting$bw)

    ppp_reference <- spatstat.geom::as.ppp(reconstruction$reconstruction_1$reference, reconstruction$reconstruction_1$window)
  }

  for (i in seq_len(n_repetitions)) {

    message("Progress in the creation of the figures: ", i/n_repetitions*100,"%\t\t\r", appendLF = FALSE)

    ppp_reconstructed <- spatstat.geom::as.ppp(reconstruction[[i]][[2]], reconstruction[[i]][[3]])

    pcf_func <-spatstat.explore::pcf.ppp(ppp_reconstructed, bw = bw, kernel=kernel_arg,
                                         correction = "none", divisor = divisor, r = r)
    pcf_func_recon[[i]] <- pcf_func$un

    k_func <- spatstat.explore::Kest(ppp_reconstructed, correction="none", r = r)
    k_func_recon[[i]]<- k_func$un

    ppp_reconstructed$marks$mark <- ppp_reconstructed$marks$mark[,2]
    markcorr_func                <- spatstat.explore::markcorr(ppp_reconstructed, correction = "none", r = r)

    dbh_markcorr_func_recon[[i]]     <- markcorr_func$diameter$un
    species_markcorr_func_recon[[i]] <- markcorr_func$species$un
  }

  # Visualising the k function of the results of the point pattern reconstruction.
  k_func_recon <- as.data.frame(k_func_recon)
  name <- c(sprintf("k_func%03d", seq_len(n_repetitions)))
  colnames(k_func_recon) <- name

  k_func            <- spatstat.explore::Kest(ppp_reference, correction="none", r = r)
  k_func_recon_mean <- rowMeans(k_func_recon)
  k_func_all        <- data.frame(cbind(k_func$un, k_func_recon, k_func_recon_mean, k_func$r))

  colnames(k_func_all)[1] <- c("Reference")
  colnames(k_func_all)[length(k_func_all)] <- c("r")

  # Visualising the Pair correlation functions (pcf) of the results of the point pattern reconstruction.
  pcf_func_recon <- as.data.frame(pcf_func_recon)
  name <- c(sprintf("pcf_func_recon%03d", seq_len(n_repetitions)))
  colnames(pcf_func_recon) <- name

  pcf_func <- spatstat.explore::pcf.ppp(ppp_reference, bw = bw, kernel=kernel_arg,
                                        correction = "none", r = r, divisor = divisor)
  pcf_func_recon_mean <- rowMeans(pcf_func_recon)
  pcf_func_all        <- data.frame(cbind(pcf_func$un, pcf_func_recon, pcf_func_recon_mean, pcf_func$r))

  colnames(pcf_func_all)[1] <- c("Reference")
  colnames(pcf_func_all)[length(pcf_func_all)] <- c("r")

  # Visualising the mark correlation functions (mcf`s) of the results of the point pattern reconstruction.
  dbh_markcorr_func_recon<-as.data.frame(dbh_markcorr_func_recon)
  name <- c(sprintf("dbh_markcorr_func_recon%03d", seq_len(n_repetitions)))
  colnames(dbh_markcorr_func_recon) <- name

  species_markcorr_func_recon <- as.data.frame(species_markcorr_func_recon)
  name <- c(sprintf("species_markcorr_func_recon%03d", seq_len(n_repetitions)))
  colnames(species_markcorr_func_recon) <- name

  markcorr_func         <- spatstat.explore::markcorr(ppp_reference, correction = "none", r = r)
  dbh_markcorr_func     <- markcorr_func$diameter$un
  species_markcorr_func <- markcorr_func$species$un

  dbh_markcorr_func_recon_mean <- rowMeans(dbh_markcorr_func_recon)
  dbh_markcorr_all <- data.frame(cbind(dbh_markcorr_func, dbh_markcorr_func_recon,
                                       dbh_markcorr_func_recon_mean, markcorr_func$diameter$r))

  species_markcorr_func_recon_mean <- rowMeans(species_markcorr_func_recon)
  species_markcorr_all <- data.frame(cbind(species_markcorr_func, species_markcorr_func_recon,
                                           species_markcorr_func_recon_mean, markcorr_func$diameter$r))

  colnames(dbh_markcorr_all)[1] <- c("Reference_mark_dbh")
  colnames(dbh_markcorr_all)[length(dbh_markcorr_all)] <- c("r")

  colnames(species_markcorr_all)[1] <- c("Reference_mark_species")
  colnames(species_markcorr_all)[length(species_markcorr_all)] <- c("r")

  graphics::par(mfrow = c(2, 2))

  # plot Kest
  graphics::plot(NULL, xlim = range(r), ylim = range(c(k_func_all$Reference,
                                                       k_func_all$k_func_recon_mean)),
                 main = "K-function", xlab = "r", ylab = "K(r)")

  graphics::lines(x = k_func_all$r, y = k_func_all$Reference, lty = "dashed")
  graphics::lines(x = k_func_all$r, y = k_func_all$k_func_recon_mean)
  graphics::abline(v = rmax, lty = "dotted", col = "grey")

  graphics::legend(x = "topleft", legend = c("Reference", "Reconstructed"),
                   lty = c(2, 1), inset = 0.015)

  # plot pcf
  graphics::plot(NULL, xlim = range(r), ylim = range(c(pcf_func_all$Reference,
                                                       pcf_func_all$pcf_func_recon_mean),
                                                     finite = TRUE),
                 main = "Pair correlation function", xlab = "r", ylab = "g(r)")

  graphics::lines(x = pcf_func_all$r, y = pcf_func_all$Reference, lty = "dashed")
  graphics::lines(x = pcf_func_all$r, y = pcf_func_all$pcf_func_recon_mean)
  graphics::abline(v = rmax, lty = "dotted", col = "grey")

  # # ask user to hit enter
  # graphics::par(ask = TRUE)

  # plot kmm(r)
  graphics::plot(NULL, xlim = range(r), ylim = range(c(dbh_markcorr_all$Reference_mark_dbh,
                                                       dbh_markcorr_all$dbh_markcorr_func_recon_mean)),
                main = "Mark Correlation Function (dbh)", xlab = "r", ylab = "kmm(r)")

  graphics::lines(x = dbh_markcorr_all$r, y = dbh_markcorr_all$Reference_mark_dbh, lty = "dashed")
  graphics::lines(x = dbh_markcorr_all$r, y = dbh_markcorr_all$dbh_markcorr_func_recon_mean)
  graphics::abline(v = rmax, lty = "dotted", col = "grey")

  graphics::plot(NULL, xlim = range(r), ylim = range(c(species_markcorr_all$Reference_mark_species,
                                                       species_markcorr_all$species_markcorr_func_recon_mean)),
                 main = "Mark Correlation Function (species)", xlab = "r", ylab = "kmm(r)")

  graphics::lines(x = species_markcorr_all$r, y = species_markcorr_all$Reference_mark_species, lty = "dashed")
  graphics::lines(x = species_markcorr_all$r, y = species_markcorr_all$species_markcorr_func_recon_mean)
  graphics::abline(v = rmax, lty = "dotted", col = "grey")

  graphics::par(mfrow = c(1, 1))

}
