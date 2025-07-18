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
            what %in% c("follower", "following"))
  id = path_ext_remove(path_file(dt$path))
  stopifnot(all.equal(id, dt[[what]]))
  invisible(dt)
}

is.valid = function(account) {
  is.list(account) &&
    !is.null(account$id)
}

# Proceed ---

if (interactive()) message("Setting up")

my.followings.id = fromJSON(get.followings.path(my.id))$following
my.followers.id = fromJSON(get.followers.path(my.id))$follower
core.id = unique(c(my.id, my.followings.id, my.followers.id))
rm(my.followings.id, my.followers.id)

stopifnot(length(dir_ls("moves", glob = "*.json")) == 0)

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
  do.contents.match.path("follower") |>
  _[, path := NULL]
followers = followers.path |>
  _[file.exists(followers.path)] |>
  sapply(read.follows, simplify = FALSE) |>
  rbindlist(idcol = "path") |>
  do.contents.match.path("following") |>
  _[, path := NULL]
follows = unique(rbind(followings, followers))
rm(followings, followers)

all.id = unique(c(follows$follower, follows$following))
all.account.path = get.account.path(all.id)
stopifnot(all(file.exists(all.account.path)))

accounts = data.table(
  id = all.id, username = NA_character_, acct = NA_character_, display_name = NA_character_,
  locked = NA, bot = NA, created_at = NA_character_, followers_count = NA_integer_,
  following_count = NA_integer_, statuses_count = NA_integer_, key = "id"
)
if (interactive()) {
  message("Collating a data.table of account data")
  pb = txtProgressBar(max = length(all.id), style = 3)
}
for (i in seq_along(all.id)) {
  account = read.account(all.account.path[i])
  id = all.id[i]
  if (!is.valid(account)) warning(sprintf("Account %s is not valid", id))
  accounts[id, (some.colnames) := account[some.colnames]]
  if (interactive()) setTxtProgressBar(pb, i)
}
if (interactive()) {close(pb); cat("\n")}

if (!dir.exists("data")) dir.create("data")
fwrite(follows, path("data", "follows", ext = "csv"), quote = TRUE)
fwrite(accounts, path("data", "accounts", ext = "csv"), quote = TRUE)
