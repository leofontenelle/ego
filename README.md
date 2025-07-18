Just a playful attempt at analyzing my ego network on the Fediverse. See the [blog post](https://blog.leonardof.med.br/2024/grafo-fediverso.html).

- `data/`
  - `accounts.csv` — nodes
  - `follows.csv` — edges
  - `brasil.json` — [list of Brazilian instances](https://brasil.rednet.social/)
- `download.R` — A script to download the (hundreds of thousands of) account files
- `collate.R` — Check the data and gather relevant accounts and follows in one CSV file each
- `network.R` — Finally analyze the data
- `Rplot001.png` — The resulting network diagram
