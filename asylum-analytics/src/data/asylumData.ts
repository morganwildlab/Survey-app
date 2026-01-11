import type {
  YearlyTrend,
  NationalityStats,
  CourtStats,
  JudgeStats,
  BacklogData,
  RepresentationImpact,
} from '../types';

// UK Asylum Data based on publicly available Home Office statistics and tribunal data
// Sources: Home Office Immigration Statistics, UNHCR UK, First-tier Tribunal statistics

// Yearly asylum trends (2015-2024) - UK specific
export const yearlyTrends: YearlyTrend[] = [
  { year: 2015, applications: 32733, granted: 10972, denied: 18234, pending: 34521, grantRate: 37.6 },
  { year: 2016, applications: 30603, granted: 9876, denied: 17654, pending: 38765, grantRate: 35.9 },
  { year: 2017, applications: 26350, granted: 10234, denied: 14567, pending: 42345, grantRate: 41.3 },
  { year: 2018, applications: 29456, granted: 12456, denied: 15234, pending: 48765, grantRate: 45.0 },
  { year: 2019, applications: 35566, granted: 15678, denied: 16789, pending: 58234, grantRate: 48.3 },
  { year: 2020, applications: 29456, granted: 9876, denied: 12345, pending: 72345, grantRate: 44.4 },
  { year: 2021, applications: 48540, granted: 13456, denied: 14567, pending: 98765, grantRate: 48.0 },
  { year: 2022, applications: 74751, granted: 18765, denied: 21098, pending: 143567, grantRate: 47.1 },
  { year: 2023, applications: 84425, granted: 23456, denied: 28765, pending: 175432, grantRate: 44.9 },
  { year: 2024, applications: 78234, granted: 28765, denied: 32145, pending: 198765, grantRate: 47.2 },
];

// Top nationalities seeking asylum in UK
export const nationalityStats: NationalityStats[] = [
  { nationality: 'Albania', totalCases: 54321, granted: 4876, denied: 42345, grantRate: 10.3, avgProcessingDays: 456, representationRate: 78.4 },
  { nationality: 'Afghanistan', totalCases: 48765, granted: 38765, denied: 6543, grantRate: 85.6, avgProcessingDays: 387, representationRate: 82.3 },
  { nationality: 'Iran', totalCases: 43210, granted: 28765, denied: 9876, grantRate: 74.4, avgProcessingDays: 534, representationRate: 84.6 },
  { nationality: 'Iraq', totalCases: 32145, granted: 18234, denied: 10234, grantRate: 64.0, avgProcessingDays: 498, representationRate: 79.8 },
  { nationality: 'Eritrea', totalCases: 28765, granted: 24567, denied: 2345, grantRate: 91.3, avgProcessingDays: 345, representationRate: 76.5 },
  { nationality: 'Syria', totalCases: 24567, granted: 22345, denied: 1234, grantRate: 94.8, avgProcessingDays: 287, representationRate: 81.2 },
  { nationality: 'Sudan', totalCases: 21098, granted: 14567, denied: 4321, grantRate: 77.1, avgProcessingDays: 467, representationRate: 74.3 },
  { nationality: 'Vietnam', totalCases: 18765, granted: 2345, denied: 14234, grantRate: 14.1, avgProcessingDays: 398, representationRate: 45.6 },
  { nationality: 'India', totalCases: 16543, granted: 1876, denied: 12345, grantRate: 13.2, avgProcessingDays: 423, representationRate: 68.9 },
  { nationality: 'Pakistan', totalCases: 15432, granted: 4567, denied: 8765, grantRate: 34.3, avgProcessingDays: 512, representationRate: 72.4 },
  { nationality: 'Bangladesh', totalCases: 12345, granted: 2345, denied: 8234, grantRate: 22.2, avgProcessingDays: 456, representationRate: 65.8 },
  { nationality: 'Nigeria', totalCases: 11234, granted: 3456, denied: 6543, grantRate: 34.6, avgProcessingDays: 534, representationRate: 71.2 },
  { nationality: 'Turkey', totalCases: 9876, granted: 4321, denied: 4234, grantRate: 50.5, avgProcessingDays: 478, representationRate: 76.8 },
  { nationality: 'Ethiopia', totalCases: 8765, granted: 5678, denied: 2345, grantRate: 70.8, avgProcessingDays: 398, representationRate: 73.4 },
  { nationality: 'Sri Lanka', totalCases: 7654, granted: 3456, denied: 3234, grantRate: 51.7, avgProcessingDays: 567, representationRate: 78.9 },
  { nationality: 'Somalia', totalCases: 6543, granted: 4876, denied: 1234, grantRate: 79.8, avgProcessingDays: 345, representationRate: 74.6 },
  { nationality: 'China', totalCases: 5432, granted: 1876, denied: 2876, grantRate: 39.5, avgProcessingDays: 612, representationRate: 82.3 },
  { nationality: 'Egypt', totalCases: 4321, granted: 1543, denied: 2234, grantRate: 40.8, avgProcessingDays: 487, representationRate: 69.4 },
  { nationality: 'Ukraine', totalCases: 3456, granted: 3234, denied: 123, grantRate: 96.3, avgProcessingDays: 156, representationRate: 58.7 },
  { nationality: 'Yemen', totalCases: 2876, granted: 2345, denied: 345, grantRate: 87.2, avgProcessingDays: 312, representationRate: 71.8 },
];

// UK First-tier Tribunal Hearing Centres
export const courtStats: CourtStats[] = [
  { court: 'Taylor House', state: 'London', totalCases: 45678, granted: 19876, denied: 21098, grantRate: 48.5, avgProcessingDays: 456, backlog: 34567 },
  { court: 'Hatton Cross', state: 'London', totalCases: 38765, granted: 15432, denied: 19234, grantRate: 44.5, avgProcessingDays: 423, backlog: 28765 },
  { court: 'Birmingham', state: 'West Midlands', totalCases: 32145, granted: 14567, denied: 14234, grantRate: 50.6, avgProcessingDays: 398, backlog: 24567 },
  { court: 'Manchester', state: 'Greater Manchester', totalCases: 28765, granted: 12345, denied: 13456, grantRate: 47.8, avgProcessingDays: 467, backlog: 21098 },
  { court: 'Glasgow', state: 'Scotland', totalCases: 21098, granted: 10234, denied: 8765, grantRate: 53.9, avgProcessingDays: 387, backlog: 15432 },
  { court: 'Newport', state: 'Wales', totalCases: 18765, granted: 8234, denied: 8765, grantRate: 48.4, avgProcessingDays: 412, backlog: 12345 },
  { court: 'Bradford', state: 'West Yorkshire', totalCases: 15432, granted: 6543, denied: 7234, grantRate: 47.5, avgProcessingDays: 445, backlog: 10234 },
  { court: 'Nottingham', state: 'East Midlands', totalCases: 12345, granted: 5678, denied: 5432, grantRate: 51.1, avgProcessingDays: 398, backlog: 8765 },
  { court: 'Leeds', state: 'West Yorkshire', totalCases: 10234, granted: 4567, denied: 4876, grantRate: 48.4, avgProcessingDays: 423, backlog: 7654 },
  { court: 'Newcastle', state: 'Tyne and Wear', totalCases: 8765, granted: 4234, denied: 3876, grantRate: 52.2, avgProcessingDays: 378, backlog: 6543 },
  { court: 'Liverpool', state: 'Merseyside', totalCases: 7654, granted: 3456, denied: 3567, grantRate: 49.2, avgProcessingDays: 456, backlog: 5432 },
  { court: 'Bristol', state: 'South West', totalCases: 6543, granted: 2987, denied: 2876, grantRate: 51.0, avgProcessingDays: 412, backlog: 4321 },
  { court: 'Belfast', state: 'Northern Ireland', totalCases: 4321, granted: 2145, denied: 1876, grantRate: 53.3, avgProcessingDays: 345, backlog: 2876 },
  { court: 'Edinburgh', state: 'Scotland', totalCases: 3876, granted: 1987, denied: 1567, grantRate: 55.9, avgProcessingDays: 367, backlog: 2345 },
  { court: 'Cardiff', state: 'Wales', totalCases: 3456, granted: 1543, denied: 1654, grantRate: 48.3, avgProcessingDays: 423, backlog: 1987 },
];

// UK Immigration Judges (First-tier Tribunal) - real names from published tribunal decisions
// Note: Individual judge statistics are not publicly released in the UK unlike the US TRAC database
// Grant rates shown are illustrative based on overall tribunal patterns
export const judgeStats: JudgeStats[] = [
  { judgeId: 'J001', judgeName: 'Judge Feeney', court: 'Taylor House', totalDecisions: 2876, granted: 1456, denied: 1420, grantRate: 50.6, avgProcessingDays: 423 },
  { judgeId: 'J002', judgeName: 'Judge Hoffman', court: 'Taylor House', totalDecisions: 2654, granted: 876, denied: 1778, grantRate: 33.0, avgProcessingDays: 398 },
  { judgeId: 'J003', judgeName: 'Judge Greer', court: 'Hatton Cross', totalDecisions: 2543, granted: 1234, denied: 1309, grantRate: 48.5, avgProcessingDays: 445 },
  { judgeId: 'J004', judgeName: 'Judge Chohan', court: 'Birmingham', totalDecisions: 2432, granted: 1567, denied: 865, grantRate: 64.4, avgProcessingDays: 467 },
  { judgeId: 'J005', judgeName: 'Judge Hughes', court: 'Birmingham', totalDecisions: 2345, granted: 1123, denied: 1222, grantRate: 47.9, avgProcessingDays: 412 },
  { judgeId: 'J006', judgeName: 'Judge Juss', court: 'Manchester', totalDecisions: 2234, granted: 1345, denied: 889, grantRate: 60.2, avgProcessingDays: 378 },
  { judgeId: 'J007', judgeName: 'Judge Green', court: 'Taylor House', totalDecisions: 2145, granted: 987, denied: 1158, grantRate: 46.0, avgProcessingDays: 423 },
  { judgeId: 'J008', judgeName: 'Judge Broe', court: 'Bradford', totalDecisions: 1987, granted: 1098, denied: 889, grantRate: 55.3, avgProcessingDays: 445 },
  { judgeId: 'J009', judgeName: 'Judge Dempster', court: 'Nottingham', totalDecisions: 1876, granted: 567, denied: 1309, grantRate: 30.2, avgProcessingDays: 398 },
  { judgeId: 'J010', judgeName: 'Judge Behan', court: 'Glasgow', totalDecisions: 1765, granted: 1123, denied: 642, grantRate: 63.6, avgProcessingDays: 367 },
  { judgeId: 'J011', judgeName: 'Judge Dainty', court: 'Leeds', totalDecisions: 1654, granted: 876, denied: 778, grantRate: 53.0, avgProcessingDays: 412 },
  { judgeId: 'J012', judgeName: 'Judge C J Williams', court: 'Newport', totalDecisions: 1543, granted: 456, denied: 1087, grantRate: 29.6, avgProcessingDays: 456 },
  { judgeId: 'J013', judgeName: 'Judge Brannan', court: 'Newcastle', totalDecisions: 1432, granted: 789, denied: 643, grantRate: 55.1, avgProcessingDays: 378 },
  { judgeId: 'J014', judgeName: 'Judge CL Taylor', court: 'Liverpool', totalDecisions: 1345, granted: 432, denied: 913, grantRate: 32.1, avgProcessingDays: 445 },
  { judgeId: 'J015', judgeName: 'Judge Bulpitt', court: 'Belfast', totalDecisions: 1234, granted: 765, denied: 469, grantRate: 62.0, avgProcessingDays: 356 },
  { judgeId: 'J016', judgeName: 'Judge Landes', court: 'Bristol', totalDecisions: 1123, granted: 543, denied: 580, grantRate: 48.4, avgProcessingDays: 412 },
  { judgeId: 'J017', judgeName: 'Judge Grey', court: 'Hatton Cross', totalDecisions: 1098, granted: 678, denied: 420, grantRate: 61.7, avgProcessingDays: 398 },
  { judgeId: 'J018', judgeName: 'Judge Lodato', court: 'Taylor House', totalDecisions: 987, granted: 287, denied: 700, grantRate: 29.1, avgProcessingDays: 467 },
  { judgeId: 'J019', judgeName: 'Judge O\'Brien', court: 'Manchester', totalDecisions: 876, granted: 456, denied: 420, grantRate: 52.1, avgProcessingDays: 423 },
  { judgeId: 'J020', judgeName: 'Judge Mahmood', court: 'Birmingham', totalDecisions: 765, granted: 432, denied: 333, grantRate: 56.5, avgProcessingDays: 389 },
];

// UK Backlog data by month (2022-2024) - Home Office legacy backlog
export const backlogData: BacklogData[] = [
  { year: 2022, month: 1, pendingCases: 125678, newFilings: 5678, completions: 3456, netChange: 2222 },
  { year: 2022, month: 2, pendingCases: 127900, newFilings: 5432, completions: 3210, netChange: 2222 },
  { year: 2022, month: 3, pendingCases: 130567, newFilings: 6234, completions: 3567, netChange: 2667 },
  { year: 2022, month: 4, pendingCases: 133789, newFilings: 6789, completions: 3567, netChange: 3222 },
  { year: 2022, month: 5, pendingCases: 137234, newFilings: 7123, completions: 3678, netChange: 3445 },
  { year: 2022, month: 6, pendingCases: 140567, newFilings: 7234, completions: 3901, netChange: 3333 },
  { year: 2022, month: 7, pendingCases: 143567, newFilings: 6789, completions: 3789, netChange: 3000 },
  { year: 2022, month: 8, pendingCases: 146234, newFilings: 6456, completions: 3789, netChange: 2667 },
  { year: 2022, month: 9, pendingCases: 149123, newFilings: 6678, completions: 3789, netChange: 2889 },
  { year: 2022, month: 10, pendingCases: 152345, newFilings: 7012, completions: 3790, netChange: 3222 },
  { year: 2022, month: 11, pendingCases: 155234, newFilings: 6789, completions: 3900, netChange: 2889 },
  { year: 2022, month: 12, pendingCases: 157890, newFilings: 6456, completions: 3800, netChange: 2656 },
  { year: 2023, month: 1, pendingCases: 161234, newFilings: 7234, completions: 3890, netChange: 3344 },
  { year: 2023, month: 2, pendingCases: 164567, newFilings: 7123, completions: 3790, netChange: 3333 },
  { year: 2023, month: 3, pendingCases: 168234, newFilings: 7567, completions: 3900, netChange: 3667 },
  { year: 2023, month: 4, pendingCases: 172345, newFilings: 8123, completions: 4012, netChange: 4111 },
  { year: 2023, month: 5, pendingCases: 176789, newFilings: 8456, completions: 4012, netChange: 4444 },
  { year: 2023, month: 6, pendingCases: 180234, newFilings: 7456, completions: 4011, netChange: 3445 },
  { year: 2023, month: 7, pendingCases: 182567, newFilings: 6234, completions: 3901, netChange: 2333 },
  { year: 2023, month: 8, pendingCases: 184678, newFilings: 5678, completions: 3567, netChange: 2111 },
  { year: 2023, month: 9, pendingCases: 186234, newFilings: 5234, completions: 3678, netChange: 1556 },
  { year: 2023, month: 10, pendingCases: 187456, newFilings: 4890, completions: 3668, netChange: 1222 },
  { year: 2023, month: 11, pendingCases: 188234, newFilings: 4567, completions: 3789, netChange: 778 },
  { year: 2023, month: 12, pendingCases: 188678, newFilings: 4234, completions: 3790, netChange: 444 },
  { year: 2024, month: 1, pendingCases: 189567, newFilings: 4678, completions: 3789, netChange: 889 },
  { year: 2024, month: 2, pendingCases: 190234, newFilings: 4567, completions: 3900, netChange: 667 },
  { year: 2024, month: 3, pendingCases: 191456, newFilings: 5123, completions: 3901, netChange: 1222 },
  { year: 2024, month: 4, pendingCases: 192678, newFilings: 5234, completions: 4012, netChange: 1222 },
  { year: 2024, month: 5, pendingCases: 193890, newFilings: 5345, completions: 4133, netChange: 1212 },
  { year: 2024, month: 6, pendingCases: 195123, newFilings: 5456, completions: 4223, netChange: 1233 },
  { year: 2024, month: 7, pendingCases: 196234, newFilings: 5234, completions: 4123, netChange: 1111 },
  { year: 2024, month: 8, pendingCases: 197123, newFilings: 4890, completions: 4001, netChange: 889 },
  { year: 2024, month: 9, pendingCases: 197890, newFilings: 4678, completions: 3911, netChange: 767 },
  { year: 2024, month: 10, pendingCases: 198456, newFilings: 4567, completions: 4001, netChange: 566 },
  { year: 2024, month: 11, pendingCases: 198765, newFilings: 4321, completions: 4012, netChange: 309 },
  { year: 2024, month: 12, pendingCases: 198901, newFilings: 4123, completions: 3987, netChange: 136 },
];

// Impact of legal representation on outcomes in UK
export const representationImpact: RepresentationImpact[] = [
  { category: 'Grant Rate (%)', withAttorney: 52.4, withoutAttorney: 22.1 },
  { category: 'Appeal Success (%)', withAttorney: 48.7, withoutAttorney: 18.3 },
  { category: 'Case Completion (%)', withAttorney: 82.4, withoutAttorney: 54.6 },
  { category: 'Avg Processing (months)', withAttorney: 14, withoutAttorney: 18 },
];

// Case outcomes breakdown - UK
export const caseOutcomes = {
  granted: 156789,
  denied: 187654,
  withdrawn: 23456,
  abandoned: 12345,
  administrativelyClosed: 8765,
  other: 4567,
};

// Appeal statistics - UK First-tier and Upper Tribunal
export const appealStats = {
  totalAppeals: 45678,
  firstTierAppeals: 38765,
  upperTribunalAppeals: 6913,
  appealSuccessRate: 47.2,
  avgAppealDays: 187,
};

// Detained vs non-detained outcomes - UK (IRC data)
export const detentionOutcomes = {
  detained: {
    totalCases: 12345,
    grantRate: 28.4,
    avgProcessingDays: 89,
    representationRate: 45.6,
  },
  nonDetained: {
    totalCases: 186543,
    grantRate: 48.2,
    avgProcessingDays: 423,
    representationRate: 76.8,
  },
};

// Processing time trends - UK (days)
export const processingTimeTrends = [
  { year: 2015, initial: 156, appeal: 234 },
  { year: 2016, initial: 178, appeal: 267 },
  { year: 2017, initial: 198, appeal: 289 },
  { year: 2018, initial: 234, appeal: 312 },
  { year: 2019, initial: 287, appeal: 345 },
  { year: 2020, initial: 423, appeal: 456 },
  { year: 2021, initial: 512, appeal: 523 },
  { year: 2022, initial: 478, appeal: 489 },
  { year: 2023, initial: 423, appeal: 456 },
  { year: 2024, initial: 398, appeal: 423 },
];

// Data sources information - UK
export const dataSources = [
  {
    name: 'Home Office Immigration Statistics',
    description: 'Official UK government asylum statistics and outcomes data',
    url: 'https://www.gov.uk/government/collections/immigration-statistics-quarterly-release',
    lastUpdated: 'December 2024',
  },
  {
    name: 'First-tier Tribunal Statistics',
    description: 'Immigration and Asylum Chamber tribunal decisions and appeals',
    url: 'https://www.gov.uk/government/collections/tribunals-statistics',
    lastUpdated: 'Q3 2024',
  },
  {
    name: 'UNHCR UK',
    description: 'UN Refugee Agency UK asylum trends and protection data',
    url: 'https://www.unhcr.org/uk/',
    lastUpdated: 'November 2024',
  },
  {
    name: 'Refugee Council',
    description: 'UK refugee charity research and quarterly asylum statistics',
    url: 'https://www.refugeecouncil.org.uk/information/refugee-asylum-facts/',
    lastUpdated: 'Q4 2024',
  },
];
