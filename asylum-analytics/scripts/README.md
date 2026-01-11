# UK Tribunal Decisions Scraper

Systematically scrapes asylum and immigration tribunal decisions from the GOV.UK Tribunals Decisions Portal.

## Source

- **Portal**: https://tribunalsdecisions.service.gov.uk/utiac
- **Data Owner**: HM Courts & Tribunals Service (HMCTS)
- **License**: Open Government Licence

## Usage

```bash
# Install dependencies first
npm install

# Run full scrape (10 pages, up to 200 decisions)
npm run scrape

# Run quick scrape (3 pages, up to 50 decisions)
npm run scrape:quick

# Or run directly with custom options
npx tsx scripts/scrapeDecisions.ts
```

## Output

The scraper outputs to `src/data/scrapedDecisions.json` with the following structure:

```typescript
{
  decisions: [
    {
      caseRef: "UI-2024-004870",
      url: "https://tribunalsdecisions.service.gov.uk/utiac/ui-2024-004870",
      promulgationDate: "19 February 2025",
      judges: {
        upperTribunal: ["Deputy Upper Tribunal Judge Saini"],
        firstTier: ["Judge C J Williams"]
      },
      outcome: "Dismissed",
      nationality: "Iraqi",
      caseType: "Asylum",
      summary: "...",
      hearingCentre: "Taylor House",
      scrapedAt: "2025-01-11T..."
    },
    // ...
  ],
  stats: {
    totalScraped: 150,
    successfulDetails: 148,
    failedDetails: 2,
    uniqueUTJudges: ["Judge A", "Judge B", ...],
    uniqueFTTJudges: ["Judge X", "Judge Y", ...],
    outcomeBreakdown: {
      "Allowed": 45,
      "Dismissed": 60,
      "Remitted": 15,
      ...
    },
    nationalityBreakdown: {
      "Iraqi": 20,
      "Iranian": 18,
      ...
    }
  },
  scrapedAt: "2025-01-11T...",
  source: "https://tribunalsdecisions.service.gov.uk/utiac"
}
```

## Extracted Data

The scraper extracts:

| Field | Description |
|-------|-------------|
| `caseRef` | Case reference (e.g., UI-2024-004870) |
| `judges.upperTribunal` | Upper Tribunal judges on the case |
| `judges.firstTier` | First-tier Tribunal judges mentioned |
| `outcome` | Allowed, Dismissed, Remitted, Set Aside, etc. |
| `nationality` | Appellant's nationality if mentioned |
| `caseType` | Asylum, Human Rights, Entry Clearance, etc. |
| `hearingCentre` | Tribunal centre where heard |
| `summary` | Brief summary from decision text |

## Rate Limiting

The scraper implements polite scraping:
- 1.5 second delay between requests
- Identifies itself via User-Agent
- Respects the public nature of the data

## Legal Note

All data scraped is from publicly available, published tribunal decisions under the Open Government Licence. This tool is for research and analytics purposes.

## Configuration

Edit `CONFIG` in `scrapeDecisions.ts`:

```typescript
const CONFIG = {
  baseUrl: 'https://tribunalsdecisions.service.gov.uk',
  searchUrl: 'https://tribunalsdecisions.service.gov.uk/utiac',
  outputPath: 'src/data/scrapedDecisions.json',
  delayMs: 1500,      // Delay between requests
  maxPages: 10,       // Listing pages to scrape
  maxDecisions: 200,  // Max decisions to fetch
};
```
