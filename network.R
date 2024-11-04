# Initial set-up ----

library(data.table)
library(igraph)
my.id = "109399675152989746"

# Ancillary functions ----

# i: integer vector
# dt: eg the 'instances' data.table
# out: color vector, same length as i
get.instance.color = function(instance.i, dt) {
	pal = hcl.colors(1000, palette = "YlGnBu")
	midpoint = dt[, round((cum - prop/2) * 1000)]
	colors = pal[midpoint]
	return(colors[instance.i])
}

tabulate.instances = function(dt) {
	setorder(dt[, .N, by = instance][, prop := N/sum(N)], -"N")[, cum := cumsum(prop)][, i := .I]
}

# Read data ----

accounts = fread(file.path("data", "accounts.csv"), 
                 colClasses = c(id = "character"), 
                 key = "id")
accounts[acct %flike% "@", instance := sub("[^@]+@([^@]+)", "\\1", acct)]
accounts[is.na(instance), instance := "mastodon.social"]

follows = fread(file.path("data", "follows.csv"), 
                colClasses = list(character = c("from", "to")),
                key = c("from", "to"))
stopifnot(anyDuplicated(follows) == 0)

# # These stats are only available for me and level 1 nodes
# 
# accounts[(core), followers := follows[, .N, by = "to"][accounts[(core), id], N, on = "to"]]

# cluster_infomap takes forever with the whole graph, so let's keep only
# me, level 1 nodes (my followings and followers) and accounts followed
# by me and level 1, because on a directed graph they can influence the
# groups of my and level 1 nodes.
accounts[my.id, core := TRUE]
accounts[follows[my.id, to, on = "from"], my.following := TRUE]
accounts[follows[my.id, from, on = "to"], my.follower := TRUE]
accounts[(my.following | my.follower), core := TRUE]
followed.by.core = follows[from %in% accounts[(core), id], unique(to)]
accounts[(core), keep := TRUE]
accounts[followed.by.core, keep := TRUE]
accounts = accounts[(keep)][, keep := NULL]
follows = follows[from %in% accounts[(core), id]]
accounts[(core), followings := follows[, .N, by = "from"][accounts[(core), id], N, on = "from"]]

accounts[, node := .I]
follows[, from.node := accounts[from, node]]
follows[, to.node := accounts[to, node]]

# instances = tabulate.instances(accounts)
instances = tabulate.instances(accounts[(core)])
instances.followers = tabulate.instances(accounts[(my.follower)])
instances.followings = tabulate.instances(accounts[(my.following)])
instances.mutuals = tabulate.instances(accounts[(my.following & my.follower)])

accounts[(core), instance.i := instances[accounts[(core), instance], i, on = "instance"]]

# barplot(with(instances[1:10], setNames(N, instance)))


# Build graph ----

# WARNING: The follows data.table is structured so that the account in 
# "from" follows the account in "to". However, information flows in the
# other direction, from the followed account to the followee.
g = graph_from_edgelist(cbind(from = follows$to.node, to = follows$from.node))
g.core = induced_subgraph(g, accounts[(core), node])
cluster = cluster_infomap(g)
accounts[, membership := membership(cluster)]

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

l = layout_in_circle(g.core, order = accounts[(core), order(membership)])
plot(g.core, layout = l, # mark.groups = cluster,
     edge.width = 0.1, edge.arrow.size = 0.1, edge.arrow.witdh = 0.1, edge.curved = TRUE)
