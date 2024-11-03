# Initial set-up ---

library(data.table)
library(jsonlite)
library(fs)
library(rtoot)

my.id = "109399675152989746"

account.colnames = c("id", "username", "acct", "display_name", 
                     "locked", "bot", "created_at", 
                     "followers_count", "following_count", "statuses_count")
some.colnames = account.colnames[-1]

# Ancillary functions ---

get.path = function(id, folder) path(folder, id, ext = "json")
get.account.path = function(id) get.path(id, "accounts")
# These could be CSV but let's keep the symmetry
get.followers.path  = function(id) get.path(id, "followers")
get.followings.path = function(id) get.path(id, "followings")
get.moves.path = function(id) get.path(id, "moves")

read.account = function(account.path) fromJSON(account.path)[account.colnames]
read.follows = function(follows.path) setDT(data.frame(fromJSON(follows.path)))

do.contents.match.path = function(dt, what) {
	require(fs)
	stopifnot("path" %in% names(dt),
	          what %in% c("from", "to"))
	id = path_ext_remove(path_file(dt$path))
	stopifnot(all.equal(id, dt[[what]]))
	invisible(dt)
}

is.valid = function(account) {
	is.list(account) &&
		!is.null(account$id)
}

# This function does not deal with limits because it was meant to redo 
# accounts with 9 followings or 9 followers. The follows' accounts had
# not been downloaded, and the relationship had not been recorded.
get.follows = function(id, what, verbose = TRUE) {
  stopifnot(what %in% c("followings", "followers"))
  follows.path = get.path(id, what)
  fun = switch(what, 
               followings = get_account_following, 
               followers = get_account_followers)
  response = fun(id, parse = FALSE)
  Sys.sleep(1)
  ids = sapply(response, write.account.or.not)
  if (is.null(ids) || (length(ids) == 0)) {
    l = list(from = character(0), to = character(0))
  } else {
    l = switch(what, 
               followings = list(from = id, to = ids),
               followers =  list(from = ids, to = id))
  }
  write_json(l, follows.path)
  return(ids)
}

write.account.or.not = function(account) {
  path = get.account.path(account$id)
  if (!file.exists(path)) write_json(account, path)
  return(account$id)
}


# Proceed ---

if (interactive()) message("Setting up")

moves = dir_ls("moves", glob = "*.json") |> 
  sapply(fromJSON, simplify = FALSE) |> 
  rbindlist(idcol = "path") |>
  do.contents.match.path("from") |>
  _[, path := NULL] |>
  setkey("from")


core.id = c(my.id,
            fromJSON(get.followings.path(my.id))$to,
            fromJSON(get.followers.path(my.id))$from) |>
  unique() |> 
  data.table(from = _) |> 
  merge(x = moves, y = _, all.y = TRUE) |> 
  with(fcoalesce(to, from)) |>
  sort()

accounts.path = get.account.path(core.id)
followings.path = get.followings.path(core.id)
followers.path = get.followers.path(core.id)

stopifnot(all(file.exists(accounts.path)),
        all(file.exists(followings.path)),
        all(file.exists(followers.path)))

if (interactive()) message("Collating a data.table of follows")

followings = followings.path |>
  _[file.exists(followings.path)] |> 
  sapply(read.follows, simplify = FALSE) |> 
  rbindlist(idcol = "path") |> 
   do.contents.match.path("from") |>
  _[, path := NULL]

followers = followers.path |>
  _[file.exists(followers.path)] |> 
  sapply(read.follows, simplify = FALSE) |> 
  rbindlist(idcol = "path") |> 
  do.contents.match.path("to") |>
  _[, path := NULL]


follows = rbind(followings, followers)
m1 = moves[follows, on = "from"]
follows[!is.na(m1$to), from := m1[!is.na(to), to]]
m2 = moves[follows, on = c(from="to")]
follows[!is.na(m2$to), to := m2[!is.na(to), to]]
rm(followings, followers, m1, m2)
follows = unique(follows)

all.id = unique(c(follows$from, follows$to))
all.account.path = get.account.path(all.id)
stopifnot(all(file.exists(all.account.path)))

if (interactive()) {
	message("Collating a data.table of account data")
	pb = txtProgressBar(max = length(all.id), style = 3)
}
accounts = data.table(id = all.id, 
  username = NA_character_, acct = NA_character_, display_name = NA_character_, 
  locked = NA, bot = NA, created_at = NA_character_, followers_count = NA_integer_, 
  following_count = NA_integer_, statuses_count = NA_integer_, key = "id")
for (i in seq_along(all.id)) {
  account = read.account(all.account.path[i])
  id = all.id[i]
  if (!is.valid(account)) warning(sprintf("Account %s is not valid", id))
  accounts[id, (some.colnames) := account[some.colnames]]
  if (interactive()) setTxtProgressBar(pb, i)
}
if (interactive()) {close(pb); cat("\n")}

# We are not checking if followers_count and following_count match the lines
# in the follows data.frame because, frankly, downloading the data took days and
# everyone's followers and followings could have changed in the mean time.

# Previously, a typo in get.follows() on download.R treated accounts with
# 9 followers/followings as if they had 0 followers/followings
nine.followers = accounts[core.id][followers_count == 9, id]
nine.followings = accounts[core.id][following_count == 9, id]
stopifnot(all(follows[nine.followers, length(na.omit(from)) > 0, on = "to"]))
stopifnot(all(follows[nine.followings, length(na.omit(to)) > 0, on = "from"]))

## This code took care of making sure the checks above worked.
# if (interactive()) {
# 	message(sprintf("Redoing %d accounts with 9 followers", length(nine.followers)))
# 	pb = txtProgressBar(max = length(nine.followers), style = 3)
# }
# for (i in seq_along(nine.followers)) {
# 	get.follows(nine.followers[i], "followers")
# 	if (interactive()) setTxtProgressBar(pb, i)
# }
# if (interactive()) {close(pb); cat("\n")} 
# if (interactive()) {
# 	message(sprintf("Redoing %d accounts with 9 followings", length(nine.followings)))
# 	pb = txtProgressBar(max = length(nine.followings), style = 3)
# }
# for (i in seq_along(nine.followers)) {
# 	get.follows(nine.followers[i], "followings")
# 	if (interactive()) setTxtProgressBar(pb, i)
# }
# if (interactive()) {close(pb); cat("\n")}


if (!dir.exists("data")) dir.create("data")
fwrite(follows, path("data", "follows", ext = "csv"), quote = TRUE)
fwrite(accounts, path("data", "accounts", ext = "csv"), quote = TRUE)

# follows = fread("data/follows.csv", colClasses = list(character = c("from", "to")))
# accounts = fread("data/accounts.csv", colClasses=c(id="character"), key = "id")

