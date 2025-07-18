library(data.table)
library(jsonlite)
library(igraph)
my.id = "109399675152989746"
my.instance = "mastodon.social"

brazilian.instances = fromJSON(file.path("data", "brazil.json"))[, 1]

# Ancillary functions ----

get.community.polygons = function(layout, membership) {
  stopifnot(ncol(layout) == 2, nrow(layout) == length(membership))
  inside = 1.10; outside = 1.15
  l = layout[order(membership), ]
  m = sort(membership)
  res = vector("list", max(m))
  for (i in unique(m)) {
    x = subset(l, m == i)
    res[[i]] = rbind(x * inside, x[nrow(x):1, ] * outside)
  }
  return(res)
}

get.community.colors = function(membership) {
  pal = hcl.colors(1000, palette = "viridis")
  res = rep(NA_character_, max(membership))
  tbl = data.table(membership) |>
    _[, .N, keyby = "membership"] |>
    setorder(-"N") |>
    _[, prop := N/sum(N)] |>
    _[, cum := cumsum(prop)] |>
    _[, midpoint := round((cum - prop/2) * 1000)] |>
    _[, color := pal[midpoint]]
  res[tbl$membership] = tbl$color
  return(res)
}

# i: integer vector
# dt: eg the 'instances' data.table
# out: color vector, same length as i
get.instance.color = function(instance.i, dt) {
  pal = hcl.colors(1000, palette = "YlGnBu")
  midpoint = dt[, round((cum - prop/2) * 1000)]
  colors = pal[midpoint]
  return(colors[instance.i])
}

get.top.quantile = function(dt, quantile = 2/3) {
  smallest.n = dt[cum <= quantile, last(N)]
  dt[N >= smallest.n]
}

plot.community.polygons = function(polygons, colors) {
  stopifnot(length(polygons) == length(colors),
            all(sapply(polygons, is.null) == is.na(colors)))
  for (i in seq_along(polygons)) {
    if (is.null(polygons[[i]])) next
    polygon(polygons[[i]], border = colors[i], col=paste0(colors[i], "E6"))
  }
}

tabulate.instances = function(dt) {
  setorder(dt[, .N, by = instance][, prop := N/sum(N)], -"N")[, cum := cumsum(prop)][, i := .I]
}

# Read data ----

accounts = fread(file.path("data", "accounts.csv"),
                 colClasses = c(id = "character"),
                 key = "id")
accounts[acct %flike% "@", instance := sub("[^@]+@([^@]+)", "\\1", acct)]
accounts[is.na(instance), instance := my.instance]

follows = fread(file.path("data", "follows.csv"),
                colClasses = list(character = c("follower", "following")),
                key = c("follower", "following"))
stopifnot(anyDuplicated(follows) == 0)

accounts[my.id, core := TRUE]
accounts[follows[my.id, following, on = "follower"], my.following := TRUE]
accounts[follows[my.id, follower, on = "following"], my.follower := TRUE]
accounts[(my.following | my.follower), core := TRUE]

accounts[(my.following & my.follower), mutual := TRUE]
accounts[, level := 2]
accounts[(core), level := 1]
accounts[my.id, level := 0]
accounts[, br.inst := instance %in% brazilian.instances]
accounts[, node := .I]

instances = tabulate.instances(accounts[(core)])
instances.followers = tabulate.instances(accounts[(my.follower)])
instances.followings = tabulate.instances(accounts[(my.following)])
instances.mutuals = tabulate.instances(accounts[(my.following & my.follower)])

accounts[(core), instance.i := instances[accounts[(core), instance], i, on = "instance"]]

# Build graph ----

g = graph_from_edgelist(cbind(from = follows$following, to = follows$follower))
g.core = subgraph_from_edges(g, which(follows$following %in% accounts[(core), id] & follows$follower %in% accounts[(core), id]))
cluster = cluster_infomap(g)
accounts[, membership := membership(cluster)]

accounts[(core), size := 2.0]
accounts[my.id, size := 5.0]
accounts[(core), color := get.instance.color(instance.i, instances)]
accounts[(core), frame.color := NA_character_]
accounts[my.id, frame.color := "black"]

# Plot core graph ----

accounts[(core), size := 2.0]
accounts[my.id, size := 5.0]
accounts[(core), color := get.instance.color(instance.i, instances)]
accounts[(core), frame.color := NA_character_]
accounts[my.id, frame.color := "black"]

V(g.core)$label = NA_character_
V(g.core)$size = accounts[(core), size]
V(g.core)$color = accounts[(core), color]
V(g.core)$frame.color = accounts[(core), frame.color]

circle = layout_in_circle(g.core, order = accounts[(core), order(membership)])
community.polygons = get.community.polygons(circle, accounts[(core), membership])
community.colors = get.community.colors(accounts[(core), membership])

op = par(mar = rep(2.1, 4L))
png(width = 1080, height = 1080, type = "cairo-png", antialias = "gray")
plot(g.core, layout = circle, xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
     edge.width = 0.1, edge.arrow.size = 0.1, edge.arrow.witdh = 0.1, edge.curved = TRUE)
plot.community.polygons(community.polygons, community.colors)
dev.off()
par(op)
