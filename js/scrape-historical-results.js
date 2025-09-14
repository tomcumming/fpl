// For parsing the results table from a wikipedia page for some PL season

function parseResultsTable() {
  const table = document.querySelector('.wikitable.plainrowheaders');
  const rows = Array.from(table.querySelectorAll('tbody > tr'));
  const teams = Array.from(rows[0].querySelectorAll('th'))
    .slice(1)
    .map(e => e.innerText);

  return rows.slice(1)
    .flatMap((row, y) => Array.from(row.querySelectorAll('td'))
        .flatMap((cell, x) => {
          const txt = cell.innerText;
          if (txt === '—') return [];

          const m = /^(\d+)–(\d+)$/.exec(txt);
          return [[teams[y], parseInt(m[1]), parseInt(m[2]), teams[x]]];
        })
    );
}
