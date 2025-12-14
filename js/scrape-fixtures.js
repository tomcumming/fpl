function scrapeMatchWeek() {
  const title = document.querySelector('.match-list-header__title').innerText;
  let matchWeek =parseInt(/^Matchweek (\d+)$/.exec(title)[1]);
  const dayContainers = document.querySelectorAll('.match-list__day-matches');
  const fixtures = Array.from(dayContainers)
    .flatMap(dayContainer => Array.from(dayContainer.querySelectorAll('.match-card'))
        .map(matchCard => {
          const [home, away] = Array.from(matchCard.querySelectorAll('.match-card__team-name--full'))
            .map(e => e.innerText);

          if (matchCard.querySelector('.match-card__score-label')) {
            const [_, homeScore, awayScore] = /^(\d+)\s-\s(\d+)$/
              .exec(matchCard.querySelector('.match-card__score-label').innerText);
            return [home, parseInt(homeScore), parseInt(awayScore), away];
          } else {
            return [home, away];
          }
        })
    );
  return [
    { matchWeek },
    ...fixtures
  ];
}

async function scrapeHistorical() {
  function navPreviousPage() {
    const PAGING_DELAY = 1000;
    document.querySelector('button[aria-label="Previous Matchweek"]').click();
    return new Promise(res => setTimeout(res, PAGING_DELAY));
  }
  
  const thisWeek = scrapeMatchWeek();
  if(thisWeek[0].matchWeek == 1) {
    return [thisWeek];
  } else {
    await navPreviousPage();
    return [...(await scrapeHistorical()), thisWeek];
  }
}

async function scrapeUpcoming(count = 10) {
  function navNextPage() {
    const PAGING_DELAY = 1000;
    document.querySelector('button[aria-label="Next Matchweek"]').click();
    return new Promise(res => setTimeout(res, PAGING_DELAY));
  }

  await navNextPage();
  const thisWeek = scrapeMatchWeek();
  if (count <= 1 || thisWeek[0].matchWeek >= 38) {
    return [thisWeek];
  } else {
    return [thisWeek, ...(await scrapeUpcoming(count - 1))];
  }
}
