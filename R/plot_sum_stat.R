#' plot_sum_stat
#'
#' @description plot_sum_stat
#'
#' @param reconstruction Result list of the dot pattern recostruction with
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
#' @keywords internal

plot_sum_stat <- function(reconstruction) {

library(ggplot2)
library(spatstat)
library(reshape)

# Data import from the results of point pattern reconstruction.

k_func_recon<-list()
pcf_func_recon<-list()
Hs_func_recon<-list()
dbh_markcorr_func_recon<-list()
species_markcorr_func_recon<-list()

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

  ppp_reference <- as.ppp(reconstruction$reference, reconstruction$window)
  }else {
    n_repetitions     <-reconstruction[[1]]$Parameter_setting$n_repetitions
    rmax              <- as.numeric(reconstruction[[n_repetitions]]$Parameter_setting$rmax)
    rpresented        <- min(c(as.numeric(reconstruction[[n_repetitions]]$window[2]),as.numeric(reconstruction[[n_repetitions]]$window[4])))
    rcount            <- as.numeric(reconstruction[[n_repetitions]]$Parameter_setting$rcount)
    r                 <- seq(rmin, if(rpresented <= rmax*2 ){rmax*2}else{rpresented}, length.out = rcount)
    bw                <- as.numeric(reconstruction[[n_repetitions]]$Parameter_setting$bw)

    ppp_reference <- as.ppp(reconstruction$reconstruction_1$reference, reconstruction$reconstruction_1$window)
    }

for (i in seq_len(n_repetitions)) {
message("Progress in the creation of the figures:", i/n_repetitions*100,"%\t\t\r", appendLF = FALSE)
if (n_repetitions > 1) {
  ppp_reconstructed<-as.ppp(reconstruction[[i]]$reconstructed, reconstruction[[i]]$window)
  }else {
    ppp_reconstructed<-as.ppp(reconstruction$reconstructed, reconstruction$window)
    }
  pcf_func<-spatstat.explore::pcf.ppp(ppp_reconstructed, bw = bw,
                                      kernel=kernel_arg, correction = "none", divisor = divisor, r = r)
  pcf_func_recon[[i]]<-  pcf_func$un

  k_func <- Kest(ppp_reconstructed, correction="none", r = r)
  k_func_recon[[i]]<-  k_func$un

  ppp_reconstructed$marks$mark<-ppp_reconstructed$marks$mark[,2]
  markcorr_func               <- markcorr(ppp_reconstructed, correction = "none", r = r)

  dbh_markcorr_func_recon[[i]]     <- markcorr_func$diameter$un
  species_markcorr_func_recon[[i]] <- markcorr_func$species$un
}
# Visualising the k function of the results of the point pattern reconstruction.
k_func_recon<-as.data.frame(k_func_recon)
name <- c(sprintf("k_func%03d", seq_len(n_repetitions)))
colnames(k_func_recon) <- name

k_func       <- spatstat.explore::Kest(ppp_reference, correction="none", r = r)

if(n_repetitions > 1){
  k_func_recon_mean <- rowMeans(k_func_recon)
  k_func_all           <- data.frame(cbind(k_func$un,k_func_recon, k_func_recon_mean, k_func$r))
  }else {
    k_func_all           <- data.frame(cbind(k_func$un,k_func_recon, k_func$r))
    }

colnames(k_func_all)[1] <- c("Reference")
colnames(k_func_all)[length(k_func_all)] <- c("r")

df_k_func               <- na.omit(data.frame(melt(k_func_all,"r")))

ggp_k_func_all <-
ggplot(data = df_k_func, aes(x = r, y = value)) +
  geom_line(aes(group = variable), col = "grey") +
  geom_line(data = subset(df_k_func, variable == "Reference"), col = "black") +
  geom_line(data = subset(df_k_func, variable == "k_func_recon_mean"), col = "black",linetype = "dashed") +
  geom_vline(xintercept = rmax, linetype='dashed')+
  theme_bw() +
   theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  ggtitle("K-function") +
  labs(y = "K(r)", x = "r [m]") +
  theme(legend.title = element_blank())

# Visualising the Pair correlation functions (pcf) of the results of the point pattern reconstruction.
pcf_func_recon<-as.data.frame(pcf_func_recon)
name <- c(sprintf("pcf_func_recon%03d", seq_len(n_repetitions)))
colnames(pcf_func_recon) <- name

pcf_func_recon_min <- apply(pcf_func_recon, 1, FUN = min)
pcf_func_recon_max <- apply(pcf_func_recon, 1, FUN = max)

pcf_func       <- spatstat.explore::pcf.ppp(ppp_reference, bw = bw, kernel=kernel_arg,
                                            correction = "none", r = r, divisor = divisor)
if(n_repetitions > 1){
  pcf_func_recon_mean <- rowMeans(pcf_func_recon)
  pcf_func_all           <- data.frame(cbind(pcf_func$un,pcf_func_recon, pcf_func_recon_min, pcf_func_recon_max, pcf_func_recon_mean, pcf_func$r))

  }else {
    pcf_func_all           <- data.frame(cbind(pcf_func$un,pcf_func_recon, pcf_func_recon_min, pcf_func_recon_max, pcf_func$r))

    }

colnames(pcf_func_all)[1] <- c("Reference")
colnames(pcf_func_all)[length(pcf_func_all)] <- c("r")

df_pcf_func               <- na.omit(data.frame(melt(pcf_func_all,"r")))

ggp_pcf_func_all <-
ggplot(data = df_pcf_func, aes(x = r, y = value)) +
  geom_line(aes(group = variable), col = "grey") +
  geom_line(data = subset(df_pcf_func, variable == "Reference"), col = "black") +
  geom_line(data = subset(df_pcf_func, variable == "pcf_func_recon_mean"), col = "black",linetype = "dashed") +
  geom_vline(xintercept = rmax, linetype='dashed')+
  theme_bw() +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  ggtitle("Pair correlation function") +
  labs(y = "g(r)", x = "r [m]") +
  theme(legend.title = element_blank())

# Visualising the mark correlation functions (mcf`s) of the results of the point pattern reconstruction.
dbh_markcorr_func_recon<-as.data.frame(dbh_markcorr_func_recon)
name <- c(sprintf("dbh_markcorr_func_recon%03d", seq_len(n_repetitions)))
colnames(dbh_markcorr_func_recon) <- name

species_markcorr_func_recon<-as.data.frame(species_markcorr_func_recon)
name <- c(sprintf("species_markcorr_func_recon%03d", seq_len(n_repetitions)))
colnames(species_markcorr_func_recon) <- name

markcorr_func               <- markcorr(ppp_reference, correction = "none", r = r)
dbh_markcorr_func           <- markcorr_func$diameter$un
species_markcorr_func       <- markcorr_func$species$un

if(n_repetitions > 1){
  dbh_markcorr_func_recon_mean <- rowMeans(dbh_markcorr_func_recon)
  dbh_markcorr_all <- data.frame(cbind(dbh_markcorr_func,
                                     dbh_markcorr_func_recon,
                                     dbh_markcorr_func_recon_mean,
                                     markcorr_func$diameter$r))
  }else {
   dbh_markcorr_all <- data.frame(cbind(dbh_markcorr_func,
                                     dbh_markcorr_func_recon,
                                     markcorr_func$diameter$r))
    }

if(n_repetitions > 1){
  species_markcorr_func_recon_mean <- rowMeans(species_markcorr_func_recon)
  species_markcorr_all <- data.frame(cbind(species_markcorr_func,
                                     species_markcorr_func_recon,
                                     species_markcorr_func_recon_mean,
                                     markcorr_func$diameter$r))
  }else {
   species_markcorr_all <- data.frame(cbind(species_markcorr_func,
                                     species_markcorr_func_recon,
                                     markcorr_func$diameter$r))
    }

colnames(dbh_markcorr_all)[1] <- c("Reference mark dbh")
colnames(dbh_markcorr_all)[length(dbh_markcorr_all)] <- c("r")

colnames(species_markcorr_all)[1] <- c("Reference mark species")
colnames(species_markcorr_all)[length(species_markcorr_all)] <- c("r")

df_dbh_markcorr_func_all              <- na.omit(data.frame(melt(dbh_markcorr_all,"r")))
df_species_markcorr_func_all              <- na.omit(data.frame(melt(species_markcorr_all,"r")))

ggp_dbh_markcorr_func_all <-
  ggplot(data = df_dbh_markcorr_func_all, aes(x = r, y = value)) +
  geom_line(aes(group = variable ), col = "grey") +
  geom_line(data = subset(df_dbh_markcorr_func_all, variable == "Reference mark dbh"), col = "black") +
  geom_line(data = subset(df_dbh_markcorr_func_all, variable == "dbh_markcorr_func_recon_mean"), col = "black",linetype = "dashed") +
  geom_vline(xintercept = rmax, linetype='dashed')+
  theme_bw() +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  ggtitle("Mark Correlation Function (dbh)") +
  labs(y = "kmm(r)",x = "r [m]") +
  theme(legend.title = element_blank())

 ggp_species_markcorr_func_all <-
  ggplot(data = df_species_markcorr_func_all, aes(x = r, y = value)) +
  geom_line(aes(group = variable), col = "grey") +
  geom_line(data = subset(df_species_markcorr_func_all, variable == "Reference mark species"), col = "black") +
  geom_line(data = subset(df_species_markcorr_func_all, variable == "species_markcorr_func_recon_mean"), col = "black",linetype = "dashed") +
  geom_vline(xintercept = rmax, linetype='dashed')+
  theme_bw() +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  ggtitle("Mark Correlation Function (species)") +
  labs(y = "kmm(r)",x = "r [m]") +
  theme(legend.title = element_blank())

result <- list(ggp_k_func_all, ggp_pcf_func_all, ggp_species_markcorr_func_all,ggp_dbh_markcorr_func_all)

cat(sep="\n\n")
print("look under Plots to see the result.")

return(result)
}
