# Initial set-up ----

library(data.table)
library(igraph)
library(jsonlite)
my.id = "109399675152989746"

account.colnames = c("id", "username", "acct", "display_name", 
                     "locked", "bot", "created_at", 
                     "followers_count", "following_count", "statuses_count")

# Ancillary functions ----

get.path = function(id, folder) file.path(folder, sprintf("%s.json", id))
get.account.path = function(id) get.path(id, "accounts")
# These could be CSV but let's keep the symmetry
get.followers.path  = function(id) get.path(id, "followers")
get.followings.path = function(id) get.path(id, "followings")

check.account = function(account) {
  is.list(account) &&
    !is.null(account$id) &&
    !is.null(account$following_count) &&
    !is.null(account$followers_count)
}

fake.account = function(id) {
  l = vector("list", length(account.colnames))
  names(l) = account.colnames
  l$id = id
  return(l)
}

get.account = function(id) {
  account.path = get.account.path(id)
  if (!file.exists(account.path)) {
    warning(sprintf("%s does not exist", account.path))
    return(fake.account(id))
  }
  account = fromJSON(account.path)
  if (!isTRUE(check.account(account))) {
    warning(sprintf("%s is invalid", account.path))
    return(fake.account(id))
  }
  return(account[account.colnames])
}

get.follows = function(id, what) {
  stopifnot(what %in% c("followings", "followers"))
  follows.path = get.path(id, what)
  if (!file.exists(follows.path)) {
    warning(sprintf("%s does not exist", follows.path))
    return(data.table(from = character(0), to = character(0)))
  }
  follows = as.data.table(fromJSON(follows.path))
  return(follows)
}

# Read data ----

my.followings = get.follows(my.id, "followings")
my.followers = get.follows(my.id, "followers")

ids = unique(c(my.followings$to, my.followers$from))
nodes = rbindlist(lapply(ids, get.account), fill = TRUE)

n_edges = sum(nodes$followers_count, nodes$following_count, na.rm = TRUE)
edges = rbind(my.followings,
              my.followers,
              data.table(from = character(n_edges), 
                         to = character(n_edges)))
i = nrow(my.followings) + nrow(my.followers)
if (interactive()) {
  message("Reading follows")
  pb = txtProgressBar(max = nrow(nodes), initial = i, style = 3)
}
for (id in nodes$id) {
  # Followings
  e = get.follows(id, "followings")
  stopifnot(is.data.table(e))
  if (nrow(e) > 0L) {
    new.i = i + nrow(e)
    edges[i:(new.i-1), c("from", "to") := e]
    i = new.i
  }
  # Followers
  e = get.follows(id, "followers")
  stopifnot(is.data.table(e))
  if (nrow(e) > 0L) {
    new.i = i + nrow(e)
    edges[i:(new.i-1), c("from", "to") := e]
    i = new.i
  }
  if (interactive()) setTxtProgressBar(pb, i)
}
if (interactive()) {
  close(pb)
  cat("\n")
}

ids = sort(unique(c(edges$to, edges$from)))
ids = ids[ids != ""]
nodes = sapply(account.colnames, \(x) character(length(ids)), simplify = FALSE)
setDT(nodes)
nodes[, id := ids]
setkey(nodes, id)
# nodes[, (account.colnames) := get.account(id), keyby = "id"]

not.id = account.colnames[account.colnames != "id"]
if (interactive()) {
  message("Reading accounts")
  pb = txtProgressBar(max = nrow(nodes), style = 3)
}
for (i in seq_len(nrow(nodes))) {
  id = nodes[i, id]
  account = get.account(id)
  if (!check.account(account)) next
  nodes[id, (not.id) := account[not.id]]
  if (interactive()) setTxtProgressBar(pb, i)
}
if (interactive()) {
  close(pb)
  cat("\n")
}

