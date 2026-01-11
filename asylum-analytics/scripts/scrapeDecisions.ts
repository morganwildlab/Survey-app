/**
 * UK Tribunal Decisions Scraper
 *
 * Systematically fetches and parses asylum/immigration tribunal decisions
 * from the GOV.UK Tribunals Decisions portal.
 *
 * Source: https://tribunalsdecisions.service.gov.uk/utiac
 *
 * Usage:
 *   npx ts-node scripts/scrapeDecisions.ts
 *   node scripts/scrapeDecisions.js
 *
 * Output: src/data/scrapedDecisions.json
 */

import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Configuration
const CONFIG = {
  baseUrl: 'https://tribunalsdecisions.service.gov.uk',
  searchUrl: 'https://tribunalsdecisions.service.gov.uk/utiac',
  outputPath: path.join(__dirname, '../src/data/scrapedDecisions.json'),
  delayMs: 1500, // Polite delay between requests
  maxPages: 10, // Number of listing pages to scrape
  maxDecisions: 200, // Maximum decisions to fetch details for
};

// Types
interface ScrapedDecision {
  caseRef: string;
  url: string;
  promulgationDate: string;
  judges: {
    upperTribunal: string[];
    firstTier: string[];
  };
  outcome: string | null;
  nationality: string | null;
  caseType: string | null;
  summary: string | null;
  hearingCentre: string | null;
  scrapedAt: string;
}

interface ScrapeResult {
  decisions: ScrapedDecision[];
  stats: {
    totalScraped: number;
    successfulDetails: number;
    failedDetails: number;
    uniqueUTJudges: string[];
    uniqueFTTJudges: string[];
    outcomeBreakdown: Record<string, number>;
    nationalityBreakdown: Record<string, number>;
  };
  scrapedAt: string;
  source: string;
}

// Utility: delay function
const delay = (ms: number) => new Promise(resolve => setTimeout(resolve, ms));

// Utility: simple HTML text extraction
function extractText(html: string, selector: string): string | null {
  // Simple regex-based extraction for common patterns
  const patterns: Record<string, RegExp> = {
    'title': /<title[^>]*>([^<]+)<\/title>/i,
    'h1': /<h1[^>]*>([^<]+)<\/h1>/i,
    'h2': /<h2[^>]*>([^<]+)<\/h2>/i,
  };

  if (patterns[selector]) {
    const match = html.match(patterns[selector]);
    return match ? match[1].trim() : null;
  }
  return null;
}

// Extract case references from listing page
function extractCaseRefs(html: string): { ref: string; url: string; date: string }[] {
  const cases: { ref: string; url: string; date: string }[] = [];

  // Match case reference links like /utiac/ui-2024-004870
  const linkPattern = /<a[^>]*href="(\/utiac\/[^"]+)"[^>]*>([^<]+)<\/a>/gi;
  let match;

  while ((match = linkPattern.exec(html)) !== null) {
    const url = match[1];
    const ref = match[2].trim();

    // Only include case references (UI-, JR-, EA-, etc.)
    if (/^(UI|JR|EA|PA|IA|HU|DC|RP)-?\d{4}/i.test(ref) || /^\[?\d{4}\]?\s*UK(UT|IAC)/i.test(ref)) {
      cases.push({
        ref,
        url: CONFIG.baseUrl + url,
        date: '', // Will be extracted from detail page
      });
    }
  }

  return cases;
}

// Extract judge names from decision text
function extractJudges(html: string): { upperTribunal: string[]; firstTier: string[] } {
  const utJudges: Set<string> = new Set();
  const fttJudges: Set<string> = new Set();

  // Upper Tribunal Judge patterns
  const utPatterns = [
    /Upper Tribunal Judge\s+([A-Z][a-zA-Z\-']+(?:\s+[A-Z][a-zA-Z\-']+)?)/gi,
    /Deputy Upper Tribunal Judge\s+([A-Z][a-zA-Z\-']+(?:\s+[A-Z][a-zA-Z\-']+)?(?:\s+KC)?)/gi,
    /UTJ\s+([A-Z][a-zA-Z\-']+)/gi,
    /The Honourable Mr Justice\s+([A-Z][a-zA-Z\-']+)/gi,
    /Mrs Justice\s+([A-Z][a-zA-Z\-']+)/gi,
  ];

  // First-tier Tribunal Judge patterns
  const fttPatterns = [
    /First-tier Tribunal Judge\s+([A-Z][a-zA-Z\-']+(?:\s+[A-Z][a-zA-Z\-']+)?)/gi,
    /First Tier Tribunal Judge\s+([A-Z][a-zA-Z\-']+(?:\s+[A-Z][a-zA-Z\-']+)?)/gi,
    /FtTJ\s+([A-Z][a-zA-Z\-']+)/gi,
    /FTTJ\s+([A-Z][a-zA-Z\-']+)/gi,
    /Judge\s+([A-Z][a-zA-Z\-']+)\s+(?:promulgated|dismissed|allowed)/gi,
  ];

  for (const pattern of utPatterns) {
    let match;
    while ((match = pattern.exec(html)) !== null) {
      const name = match[1].trim();
      if (name.length > 2 && name.length < 50) {
        utJudges.add(name);
      }
    }
  }

  for (const pattern of fttPatterns) {
    let match;
    while ((match = pattern.exec(html)) !== null) {
      const name = match[1].trim();
      if (name.length > 2 && name.length < 50 && !utJudges.has(name)) {
        fttJudges.add(name);
      }
    }
  }

  return {
    upperTribunal: Array.from(utJudges),
    firstTier: Array.from(fttJudges),
  };
}

// Extract outcome from decision text
function extractOutcome(html: string): string | null {
  const lowerHtml = html.toLowerCase();

  // Check for clear outcome patterns
  if (/appeal\s+is\s+allowed/i.test(html) || /the\s+appeal\s+allowed/i.test(html)) {
    return 'Allowed';
  }
  if (/appeal\s+is\s+dismissed/i.test(html) || /the\s+appeal\s+dismissed/i.test(html)) {
    return 'Dismissed';
  }
  if (/decision\s+is\s+set\s+aside/i.test(html)) {
    return 'Set Aside';
  }
  if (/remitted\s+to\s+the\s+first-tier/i.test(html) || /case\s+is\s+remitted/i.test(html)) {
    return 'Remitted';
  }
  if (/permission\s+to\s+appeal\s+is\s+refused/i.test(html)) {
    return 'Permission Refused';
  }
  if (/permission\s+to\s+appeal\s+is\s+granted/i.test(html)) {
    return 'Permission Granted';
  }

  return null;
}

// Extract nationality from decision text
function extractNationality(html: string): string | null {
  const nationalityPatterns = [
    /(?:citizen|national)\s+of\s+([A-Z][a-zA-Z]+)/i,
    /([A-Z][a-zA-Z]+)\s+(?:citizen|national)/i,
    /nationality:\s*([A-Z][a-zA-Z]+)/i,
    /from\s+([A-Z][a-zA-Z]+)\s+(?:who|and)/i,
  ];

  const validNationalities = [
    'Afghan', 'Albanian', 'Algerian', 'Bangladeshi', 'Chinese', 'Congolese',
    'Egyptian', 'Eritrean', 'Ethiopian', 'Gambian', 'Ghanaian', 'Indian',
    'Iranian', 'Iraqi', 'Jamaican', 'Kenyan', 'Kurdish', 'Lebanese', 'Libyan',
    'Malawian', 'Nepalese', 'Nigerian', 'Pakistani', 'Palestinian', 'Russian',
    'Rwandan', 'Somali', 'Sudanese', 'Syrian', 'Turkish', 'Ugandan', 'Ukrainian',
    'Vietnamese', 'Yemeni', 'Zimbabwean', 'Sri Lankan', 'Uzbek',
    // Country names
    'Afghanistan', 'Albania', 'Algeria', 'Bangladesh', 'China', 'DRC', 'Congo',
    'Egypt', 'Eritrea', 'Ethiopia', 'Gambia', 'Ghana', 'India', 'Iran', 'Iraq',
    'Jamaica', 'Kenya', 'Kurdistan', 'Lebanon', 'Libya', 'Malawi', 'Nepal',
    'Nigeria', 'Pakistan', 'Palestine', 'Russia', 'Rwanda', 'Somalia', 'Sudan',
    'Syria', 'Turkey', 'Uganda', 'Ukraine', 'Vietnam', 'Yemen', 'Zimbabwe',
  ];

  for (const pattern of nationalityPatterns) {
    const match = html.match(pattern);
    if (match) {
      const candidate = match[1];
      if (validNationalities.some(n => n.toLowerCase() === candidate.toLowerCase())) {
        return candidate;
      }
    }
  }

  // Direct search for nationalities in text
  for (const nat of validNationalities) {
    if (html.includes(nat)) {
      return nat;
    }
  }

  return null;
}

// Extract case type
function extractCaseType(html: string): string | null {
  const lowerHtml = html.toLowerCase();

  if (lowerHtml.includes('asylum') || lowerHtml.includes('refugee')) {
    return 'Asylum';
  }
  if (lowerHtml.includes('humanitarian protection')) {
    return 'Humanitarian Protection';
  }
  if (lowerHtml.includes('article 8') || lowerHtml.includes('human rights')) {
    return 'Human Rights';
  }
  if (lowerHtml.includes('entry clearance')) {
    return 'Entry Clearance';
  }
  if (lowerHtml.includes('deportation')) {
    return 'Deportation';
  }
  if (lowerHtml.includes('eea') || lowerHtml.includes('eu settlement')) {
    return 'EEA/EU Settlement';
  }

  return null;
}

// Extract hearing centre
function extractHearingCentre(html: string): string | null {
  const centres = [
    'Taylor House', 'Hatton Cross', 'Field House', 'Birmingham',
    'Manchester', 'Glasgow', 'Newport', 'Bradford', 'Nottingham',
    'Leeds', 'Newcastle', 'Liverpool', 'Bristol', 'Belfast', 'Edinburgh', 'Cardiff',
  ];

  for (const centre of centres) {
    const pattern = new RegExp(`(?:at|in)\\s+${centre}`, 'i');
    if (pattern.test(html)) {
      return centre;
    }
  }

  return null;
}

// Extract summary (first paragraph of decision)
function extractSummary(html: string): string | null {
  // Try to find summary or introduction
  const summaryMatch = html.match(/<p[^>]*>([^<]{100,500})<\/p>/i);
  if (summaryMatch) {
    return summaryMatch[1].replace(/\s+/g, ' ').trim();
  }
  return null;
}

// Fetch a page with error handling
async function fetchPage(url: string): Promise<string | null> {
  try {
    const response = await fetch(url, {
      headers: {
        'User-Agent': 'Mozilla/5.0 (compatible; AsylumAnalytics/1.0; +https://github.com/asylum-analytics)',
        'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8',
      },
    });

    if (!response.ok) {
      console.error(`Failed to fetch ${url}: ${response.status}`);
      return null;
    }

    return await response.text();
  } catch (error) {
    console.error(`Error fetching ${url}:`, error);
    return null;
  }
}

// Scrape a single decision page
async function scrapeDecision(caseRef: string, url: string): Promise<ScrapedDecision | null> {
  const html = await fetchPage(url);
  if (!html) return null;

  const judges = extractJudges(html);
  const outcome = extractOutcome(html);
  const nationality = extractNationality(html);
  const caseType = extractCaseType(html);
  const summary = extractSummary(html);
  const hearingCentre = extractHearingCentre(html);

  // Extract promulgation date
  const dateMatch = html.match(/(?:promulgated|dated?)\s*:?\s*(\d{1,2}\s+\w+\s+\d{4})/i);
  const promulgationDate = dateMatch ? dateMatch[1] : '';

  return {
    caseRef,
    url,
    promulgationDate,
    judges,
    outcome,
    nationality,
    caseType,
    summary,
    hearingCentre,
    scrapedAt: new Date().toISOString(),
  };
}

// Main scraping function
async function scrapeDecisions(): Promise<ScrapeResult> {
  console.log('🔍 Starting UK Tribunal Decisions Scraper...\n');
  console.log(`Source: ${CONFIG.searchUrl}`);
  console.log(`Max pages: ${CONFIG.maxPages}`);
  console.log(`Max decisions: ${CONFIG.maxDecisions}\n`);

  const allCases: { ref: string; url: string; date: string }[] = [];

  // Step 1: Fetch listing pages to get case references
  console.log('📋 Fetching listing pages...');

  for (let page = 1; page <= CONFIG.maxPages; page++) {
    const pageUrl = page === 1
      ? CONFIG.searchUrl
      : `${CONFIG.searchUrl}?page=${page}`;

    console.log(`  Page ${page}...`);
    const html = await fetchPage(pageUrl);

    if (html) {
      const cases = extractCaseRefs(html);
      allCases.push(...cases);
      console.log(`    Found ${cases.length} case references`);
    }

    await delay(CONFIG.delayMs);
  }

  console.log(`\n📊 Total case references found: ${allCases.length}`);

  // Deduplicate
  const uniqueCases = Array.from(
    new Map(allCases.map(c => [c.ref, c])).values()
  ).slice(0, CONFIG.maxDecisions);

  console.log(`📊 Unique cases to scrape: ${uniqueCases.length}\n`);

  // Step 2: Fetch individual decision pages
  console.log('📖 Fetching individual decisions...');

  const decisions: ScrapedDecision[] = [];
  let successCount = 0;
  let failCount = 0;

  for (let i = 0; i < uniqueCases.length; i++) {
    const { ref, url } = uniqueCases[i];

    process.stdout.write(`  [${i + 1}/${uniqueCases.length}] ${ref}... `);

    const decision = await scrapeDecision(ref, url);

    if (decision) {
      decisions.push(decision);
      successCount++;
      console.log('✓');
    } else {
      failCount++;
      console.log('✗');
    }

    await delay(CONFIG.delayMs);
  }

  // Step 3: Compile statistics
  console.log('\n📈 Compiling statistics...');

  const allUTJudges = new Set<string>();
  const allFTTJudges = new Set<string>();
  const outcomeBreakdown: Record<string, number> = {};
  const nationalityBreakdown: Record<string, number> = {};

  for (const d of decisions) {
    d.judges.upperTribunal.forEach(j => allUTJudges.add(j));
    d.judges.firstTier.forEach(j => allFTTJudges.add(j));

    if (d.outcome) {
      outcomeBreakdown[d.outcome] = (outcomeBreakdown[d.outcome] || 0) + 1;
    }
    if (d.nationality) {
      nationalityBreakdown[d.nationality] = (nationalityBreakdown[d.nationality] || 0) + 1;
    }
  }

  const result: ScrapeResult = {
    decisions,
    stats: {
      totalScraped: decisions.length,
      successfulDetails: successCount,
      failedDetails: failCount,
      uniqueUTJudges: Array.from(allUTJudges).sort(),
      uniqueFTTJudges: Array.from(allFTTJudges).sort(),
      outcomeBreakdown,
      nationalityBreakdown,
    },
    scrapedAt: new Date().toISOString(),
    source: CONFIG.searchUrl,
  };

  // Step 4: Save results
  console.log(`\n💾 Saving results to ${CONFIG.outputPath}...`);

  fs.writeFileSync(CONFIG.outputPath, JSON.stringify(result, null, 2));

  // Print summary
  console.log('\n✅ Scraping complete!\n');
  console.log('='.repeat(50));
  console.log('SUMMARY');
  console.log('='.repeat(50));
  console.log(`Total decisions scraped: ${decisions.length}`);
  console.log(`Successful: ${successCount}`);
  console.log(`Failed: ${failCount}`);
  console.log(`\nUnique Upper Tribunal Judges: ${allUTJudges.size}`);
  console.log(`Unique First-tier Judges: ${allFTTJudges.size}`);
  console.log(`\nOutcome breakdown:`);
  for (const [outcome, count] of Object.entries(outcomeBreakdown).sort((a, b) => b[1] - a[1])) {
    console.log(`  ${outcome}: ${count}`);
  }
  console.log(`\nTop nationalities:`);
  const sortedNats = Object.entries(nationalityBreakdown).sort((a, b) => b[1] - a[1]).slice(0, 10);
  for (const [nat, count] of sortedNats) {
    console.log(`  ${nat}: ${count}`);
  }

  return result;
}

// Run if executed directly
scrapeDecisions().catch(console.error);

export { scrapeDecisions, ScrapedDecision, ScrapeResult };
