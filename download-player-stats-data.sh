wget -O - https://fantasy.premierleague.com/api/bootstrap-static/ \
  | jq > data/truth/player-stats-snapshot.json
