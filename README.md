Just a playful attempt at analyzing my ego network on the Fediverse.

- `data/`
  - `accounts.csv` --- nodes
  - `follows.csv` --- edges
  - `brasil.txt` -- [list of Brazilian instances](https://brasil.rednet.social/)
- `transition.R` --- (Pun intended) a script to move files from a previous attempt and into this new one
- `download.R` --- A script to (hopefully) download the remaining (many hundred thousands of) account files
- `collate.R` --- Check the data and gather relevant accounts and follows in one CSV file each
- `network.R` --- Finaly analyze the data
