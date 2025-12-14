wget -O - https://fantasy.premierleague.com/api/bootstrap-static/ \
  | jq > data/snapshot/player-stats.json
wget -O - 'https://sdp-prem-prod.premier-league-prod.pulselive.com/api/v2/competitions/8/teams/stats/leaderboard?season=2025&_limit=20' \
  | jq > data/snapshot/team-stats.json
