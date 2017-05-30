library(tourr)
library(RColorBrewer)
data(olive)
olive$region <- factor(olive$region, levels=1:3, labels=c("South", "Sardinia", "North"))
pal <- brewer.pal(length(levels(olive$region)), "Dark2")
col <- pal[as.numeric(olive$region)]
#animate_xy(olive[,3:10], col=col)
olive_tour <- save_history(olive[,3:10], grand_tour(1), max = 3)
olive_tour_i <- interpolate(olive_tour, 0.03)

steps <- 1:50
dat <- as.matrix(attr(olive_tour_i, "data"))
val <- lapply(steps, function(x) dat %*% olive_tour_i[, , x])
t <- lapply(steps, function(x) rep(x, 572))
d <- tibble(d = unlist(val, use.names = FALSE), t = unlist(t, use.names = FALSE))
d <- d %>% mutate(region=rep(olive$region, length(steps)))
p3 <- d %>% 
  ggplot(aes(x=d, fill=region, group=t)) +
  geom_density(alpha=0) +
  geom_density(aes(frame = t, fill=region), fill="black", alpha=1)
animation_opts(
  ggplotly(p3), frame = 10
)

#----- flea example
  
oned <- save_history(flea[, 1:6], grand_tour(1), max = 3)
animate_dist(flea[, 1:6], planned_tour(oned))
oned_p <- new_geodesic_path(oned, grand_tour)

oned_i <- interpolate(oned, 0.01)

d <- data_frame(d=c(attr(oned_i, "data")%*%oned_i[,,1], 
                    attr(oned_i, "data")%*%oned_i[,,2],
                    attr(oned_i, "data")%*%oned_i[,,3]),
                    t=c(rep(1, 74), rep(2, 74), rep(3, 74)))
steps <- 1:150
dat <- attr(oned_i, "data")
val <- lapply(steps, function(x) dat %*% oned_i[, , x])
t <- lapply(steps, function(x) rep(x, 74))
d <- tibble(d = unlist(val, use.names = FALSE), t = unlist(t, use.names = FALSE))
p3 <- d %>% 
  ggplot(aes(x=d, group=t)) +
  geom_density(alpha=0) +
  geom_density(aes(frame = t), fill="black", alpha=1)
animation_opts(
  ggplotly(p3), frame = 10, redraw=FALSE
)
steps <- 1:150
dat <- attr(oned_i, "data")
val <- lapply(steps, function(x) dat %*% oned_i[, , x])
t <- lapply(steps, function(x) rep(x, 74))
d <- tibble(d = unlist(val, use.names = FALSE), t = unlist(t, use.names = FALSE))
p3 <- d %>% 
  ggplot(aes(x=d, group=t)) +
    geom_density(alpha=0) +
    geom_density(aes(frame = t), fill="black", alpha=1)
animation_opts(
  ggplotly(p3), frame = 10, redraw=FALSE
)
