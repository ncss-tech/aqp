library(aqp)
library(microbenchmark)
library(ggplot2)
library(ggthemes)

library(sp)
library(plyr)
library(stringr)
source('classes-rebooted.r')
source('init.r')
source('accessors.r')
source('setters.r')
source('overloads.r')
source('coerce.r')
source('spatial.r')

## Benchmark parameters

# Number of repeats
n_repeats <- 30 
# Number of profiles in collection
test_sizes <- c(1, 10, 100, 500, 1000, 5000 , 10000, 20000, 30000, 50000)
names(test_sizes) <- test_sizes

################################################
##
## Benchmark 1: Creating a profile collection
##
################################################

init_aqp <- function(x) {
  aqp::depths(x) <- id ~ top + bottom
  x
}

init_spc <- function(x) {
  depths(x) <- id ~ top + bottom
  x
}

benchmark_init_profiles <- function(n_profiles, n_repeats = 10) {
  
  # Create profiles collection of size n_profiles
  profiles_collection <- ldply(1:n_profiles, random_profile)
  
  # Benchmark it
  res <- microbenchmark(
    init_aqp(profiles_collection), 
    init_spc(profiles_collection), 
    times = n_repeats
  )
  
  # Return data.frame
  as.data.frame(res)
}

# Benchmark
res_init_profiles <- ldply(test_sizes, benchmark_init_profiles, n_repeats = n_repeats, .progress = "text")

# Data wrangling for illustration
res_init_profiles$.id <- as.numeric(res_init_profiles$.id)
res_init_profiles$seconds <- res_init_profiles$time * 10^(-9)
res_init_profiles$method <- factor(
  res_init_profiles$expr, 
  levels = c('init_aqp(profiles_collection)', 'init_spc(profiles_collection)'),
  labels = c('Current implementation', 'Proposed S4 classes')
)
rg <- pretty(range(res_init_profiles$seconds))
rg[1] <- 1

# Plot
p_init_profiles <- ggplot(res_init_profiles, aes(x = .id, y = seconds, colour = method)) + 
  geom_point() + 
  geom_smooth(se = F) +
  scale_x_log10(breaks = test_sizes) + scale_y_log10(breaks = rg) +
  guides(colour = guide_legend(title = NULL)) +
  labs(
    x = "Number of profiles in the collection",
    y = "Seconds"
    ) +
  theme_economist()

pdf('benchmark-init-profiles.pdf', width = 15, height = 7)
print(p_init_profiles)
dev.off()

################################################
##
## Benchmark 2: Selecting a given profile
##
################################################

get_profiles_aqp <- function(x, i = 1) {
  x[i,]
}

get_profiles_spc <- function(x, i = 1) {
  profiles(x, i)
}

benchmark_get_profile <- function(n_profiles, n_repeats = 10) {
  
  # Create profiles collection of size n_profiles
  profiles_collection <- ldply(1:n_profiles, random_profile)
  aqp <- spc <- profiles_collection
  aqp <- aqp::SoilProfileCollection(idcol = 'id', depthcols = c('top', 'bottom'), horizons = aqp) 
  depths(spc) <- id ~ top + bottom
  
  # Benchmark it
  res <- microbenchmark(
    get_profiles_aqp(aqp), 
    get_profiles_spc(spc), 
    times = n_repeats
  )
  
  # Return data.frame
  as.data.frame(res)
}

res_get_profile <- ldply(test_sizes, benchmark_get_profile, n_repeats = n_repeats, .progress = "text")

# Data wrangling for illustration
res_get_profile$.id <- as.numeric(res_get_profile$.id)
res_get_profile$seconds <- res_get_profile$time * 10^(-9)
res_get_profile$method <- factor(
  res_get_profile$expr, 
  levels = c('get_profiles_aqp(aqp)', 'get_profiles_spc(spc)'),
  labels = c('Current implementation', 'Proposed S4 classes')
)
rg <- pretty(range(res_get_profile$seconds))
rg[1] <- 1

# Plot
p_get_profile <- ggplot(res_get_profile, aes(x = .id, y = seconds, colour = method)) + 
  geom_point() + 
  geom_smooth(se = F) +
  scale_x_log10(breaks = test_sizes) + scale_y_log10(breaks = rg) +
  guides(colour = guide_legend(title = NULL)) +
  labs(
    x = "Number of profiles in the collection",
    y = "Seconds"
  ) +
  theme_economist()

pdf('benchmark-get-profiles.pdf', width = 15, height = 7)
print(p_get_profile)
dev.off()

################################################
##
## Benchmark 3: profileApply-like approaches
##
################################################

# Reducing test sizes as profileApply seems very inefficient
test_sizes <- c(1, 10, 100, 500, 1000, 2000)
names(test_sizes) <- test_sizes

apply_aqp <- function(x) {
  profileApply(x, FUN = function(x) mean(x$p1, na.rm=TRUE))
}

apply_spc <- function(x, i = 1) {
  lapply(profiles(x), function(x) mean(horizons(x)$p1, na.rm=TRUE))
}

benchmark_profile_apply <- function(n_profiles, n_repeats = 10) {
  
  # Create profiles collection of size n_profiles
  profiles_collection <- ldply(1:n_profiles, random_profile)
  aqp <- spc <- profiles_collection
  aqp <- aqp::SoilProfileCollection(idcol = 'id', depthcols = c('top', 'bottom'), horizons = aqp) 
  depths(spc) <- id ~ top + bottom
  
  # Benchmark it
  res <- microbenchmark(
    apply_aqp(aqp), 
    apply_spc(spc), 
    times = n_repeats
  )
  
  # Return data.frame
  as.data.frame(res)
}

res_profile_apply <- ldply(test_sizes, benchmark_profile_apply, n_repeats = n_repeats, .progress = "text")

# Data wrangling for illustration
res_profile_apply$.id <- as.numeric(res_profile_apply$.id)
res_profile_apply$seconds <- res_profile_apply$time * 10^(-9)
res_profile_apply$method <- factor(
  res_profile_apply$expr, 
  levels = c('apply_aqp(aqp)', 'apply_spc(spc)'),
  labels = c('Current implementation', 'Proposed S4 classes')
)
rg <- pretty(range(res_profile_apply$seconds))
rg[1] <- 1

# Plot
p_profile_apply <- ggplot(res_profile_apply, aes(x = .id, y = seconds, colour = method)) + 
  geom_point() + 
  geom_smooth(method = lm, se = F) +
  scale_x_log10(breaks = test_sizes) + scale_y_log10(breaks = rg) +
  guides(colour = guide_legend(title = NULL)) +
  labs(
    x = "Number of profiles in the collection",
    y = "Seconds"
  ) +
  theme_economist()

pdf('benchmark-profile-apply.pdf', width = 15, height = 7)
print(p_profile_apply)
dev.off()

# Based on the linear models:
res_profile_apply_mean <- ddply(res_profile_apply, .(method, .id), summarise, seconds = mean(seconds))
lm_aqp <- lm(seconds ~ .id, data = subset(res_profile_apply_mean, method == 'Current implementation'))
lm_spc <- lm(seconds ~ .id, data = subset(res_profile_apply_mean, method == 'Proposed S4 classes'))

pred_sizes <- c(1, 10, 100, 500, 1000, 5000 , 10000, 20000, 30000, 50000)
res_profile_apply_model <- rbind(
  data.frame(.id = pred_sizes, method = "Current implementation", seconds = predict(lm_aqp, newdata = data.frame(.id = pred_sizes))),
  data.frame(.id = pred_sizes, method = "Proposed S4 classes", seconds = predict(lm_spc, newdata = data.frame(.id = pred_sizes)))
)

rg <- pretty(range(res_profile_apply_model$seconds))
rg[which(rg <= 0)] <- 1
rg <- unique(rg)

# Plot
p_profile_apply_model <- ggplot() + 
  geom_point(data = res_profile_apply, aes(x = .id, y = seconds, colour = method), shape = 3) +
  geom_point(data = res_profile_apply_model, aes(x = .id, y = seconds, colour = method)) +
  scale_x_log10(breaks = pred_sizes) + scale_y_log10(breaks = rg) +
  guides(colour = guide_legend(title = NULL)) +
  labs(
    x = "Number of profiles in the collection",
    y = "Seconds"
  ) +
  theme_economist()

pdf('benchmark-profile-apply-model.pdf', width = 15, height = 7)
print(p_profile_apply_model)
dev.off()