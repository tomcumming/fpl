// For parsing each game week from the premier league website

function parseDate(str) {
  if(str == 'Yesterday')
    return new Date(new Date() - 24 * 60 * 60 * 1_000);
  if (str == 'Today')
    return new Date();
  
  const [_, day, monStr] = str.split(' ');
  const mon = {
    'Jan': 1,
    'Feb': 2,
    'Mar': 3,
    'Apr': 4,
    'May': 5,
    'Jun': 6,
    'Jul': 7,
    'Aug': 8,
    'Sep': 9,
    'Oct': 10,
    'Nov': 11,
    'Dec': 112,
  }[monStr];
  if(day < 1 || day > 31) throw new Error(`Weird day: '${day}'`)
  if(mon < 1 || mon > 12) throw new Error(`Weird month: '${mon}'`)
  const year = mon >= 8 ? 2025 : 2026;
  return new Date(year, mon - 1, day);
}

function scrapeFixtures() {
  const dayContainers = document.querySelectorAll('.match-list__day-matches');
  return Array.from(dayContainers)
    .map(dayContainer => {
      const date = parseDate(dayContainer.querySelector('.match-list__day-date').innerText);
      const dateStr = date.toISOString().split('T')[0];
      const fixtures = Array.from(dayContainer.querySelectorAll('.match-card'))
        .map(matchCard => {
          const [home, away] = Array.from(matchCard.querySelectorAll('.match-card__team-name--full'))
            .map(e => e.innerText);

          return `- ${home} - ${away}`;
        });
      return [`## ${dateStr}`, ...fixtures].join('\n')
    })
    .join('\n\n')
}

function scrapeResults() {
  const dayContainers = document.querySelectorAll('.match-list__day-matches');
  return Array.from(dayContainers)
    .map(dayContainer => {
      const date = parseDate(dayContainer.querySelector('.match-list__day-date').innerText);
      const dateStr = date.toISOString().split('T')[0];
      const fixtures = Array.from(dayContainer.querySelectorAll('.match-card'))
        .map(matchCard => {
          const [home, away] = Array.from(matchCard.querySelectorAll('.match-card__team-name--full'))
            .map(e => e.innerText);

          const [_, homeScore, awayScore] = /^(\d+)\s-\s(\d+)$/
            .exec(matchCard.querySelector('.match-card__score-label').innerText);

          return `- ${home} ${homeScore} - ${awayScore} ${away}`;
        });
      return [`## ${dateStr}`, ...fixtures].join('\n')
    })
    .join('\n\n')
}
