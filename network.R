# Initial set-up ----

library(data.table)
library(igraph)
my.id = "109399675152989746"

# Ancillary functions ----

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

accounts[, node := .I]
follows[, from.node := accounts[from, node]]
follows[, to.node := accounts[to, node]]

accounts[my.id, core := TRUE]
accounts[follows[my.id, to, on = "from"], my.following := TRUE]
accounts[follows[my.id, to, on = "from"], core := TRUE]
accounts[follows[my.id, from, on = "to"], my.follower := TRUE]
accounts[follows[my.id, from, on = "to"], core := TRUE]
# These stats are only available for me and level 1 nodes
accounts[(core), followings := follows[, .N, by = "from"][accounts[(core), id], N, on = "from"]]
accounts[(core), followers := follows[, .N, by = "to"][accounts[(core), id], N, on = "to"]]

# Describe data ----

# instances = tabulate.instances(accounts)
instances = tabulate.instances(accounts[(core)])
instances.followers = tabulate.instances(accounts[(my.follower)])
instances.followings = tabulate.instances(accounts[(my.following)])
instances.mutuals = tabulate.instances(accounts[(my.following & my.follower)])

accounts[(core), instance.i := instances[accounts[(core), instance], i, on = "instance"]]

# barplot(with(instances.core[1:10], setNames(N, instance)))


# Build graph ----

g = cbind(from = follows$to.node, to = follows$from.node) |>
	graph_from_edgelist() |>
	induced_subgraph(accounts[(core), node])
cluster = cluster_infomap(g)
accounts[(core), membership := cluster$membership]
l = layout_in_circle(g, order = order(cluster$membership))
plot(g, layout = l, # mark.groups = cluster,
     vertex.size = 3, vertex.color = accounts[(core), instance.i], vertex.frame.color = NA_character_, vertex.label = NA_character_,
     edge.width = 0.2, edge.arrow.size = 0.2, edge.arrow.witdh = 0.2, edge.curved = TRUE)
