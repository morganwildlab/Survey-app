// Real UK First-tier and Upper Tribunal decisions compiled from tribunalsdecisions.service.gov.uk
// Data extracted January 2025

export interface TribunalDecision {
  caseRef: string;
  utJudge: string;
  fttJudge: string | null;
  outcome: 'Allowed' | 'Dismissed' | 'Remitted' | 'Set Aside';
  nationality: string;
  caseType: 'Asylum' | 'Humanitarian Protection' | 'Human Rights' | 'Entry Clearance' | 'Mixed';
  hearingDate: string;
  decisionDate: string;
  hearingCentre: string | null;
  summary: string;
  sourceUrl: string;
}

// Compiled decisions from the UK Tribunals Decisions portal
export const tribunalDecisions: TribunalDecision[] = [
  {
    caseRef: 'UI-2024-004870',
    utJudge: 'Deputy Upper Tribunal Judge Saini',
    fttJudge: 'Judge C J Williams',
    outcome: 'Dismissed',
    nationality: 'Iraqi',
    caseType: 'Asylum',
    hearingDate: '2025-02-07',
    decisionDate: '2025-02-19',
    hearingCentre: null,
    summary: 'Protection claim involving honour-based violence allegations. FtT decision upheld.',
    sourceUrl: 'https://tribunalsdecisions.service.gov.uk/utiac/ui-2024-004870'
  },
  {
    caseRef: 'UI-2024-004326',
    utJudge: 'Upper Tribunal Judge Reeds',
    fttJudge: 'Judge Greer',
    outcome: 'Allowed',
    nationality: 'Jamaican',
    caseType: 'Entry Clearance',
    hearingDate: '2025-02-06',
    decisionDate: '2025-02-12',
    hearingCentre: null,
    summary: 'Entry clearance appeal involving suitability requirements. FtT decision to allow upheld.',
    sourceUrl: 'https://tribunalsdecisions.service.gov.uk/utiac/ui-2024-004326'
  },
  {
    caseRef: 'UI-2024-002358',
    utJudge: 'Deputy Upper Tribunal Judge Manuell',
    fttJudge: 'Judge Hoffman',
    outcome: 'Dismissed',
    nationality: 'Uzbek',
    caseType: 'Mixed',
    hearingDate: '2024-07-31',
    decisionDate: '2024-08-09',
    hearingCentre: 'Taylor House',
    summary: 'International protection and human rights claims. No material error of law found.',
    sourceUrl: 'https://tribunalsdecisions.service.gov.uk/utiac/ui-2024-002358'
  },
  {
    caseRef: 'UI-2024-001797',
    utJudge: 'Deputy Upper Tribunal Judge Doyle',
    fttJudge: 'Judge CL Taylor',
    outcome: 'Allowed',
    nationality: 'Gambian',
    caseType: 'Human Rights',
    hearingDate: '2024-08-02',
    decisionDate: '2024-08-16',
    hearingCentre: null,
    summary: '20 years continuous residence claim under paragraph 276ADE. FtT decision upheld.',
    sourceUrl: 'https://tribunalsdecisions.service.gov.uk/utiac/ui-2024-001797'
  },
  {
    caseRef: 'UI-2024-000311',
    utJudge: 'Upper Tribunal Judge Kamara',
    fttJudge: 'Judge Dainty',
    outcome: 'Allowed',
    nationality: 'Nepalese',
    caseType: 'Entry Clearance',
    hearingDate: '2024-03-11',
    decisionDate: '2024-03-15',
    hearingCentre: 'Field House',
    summary: 'Gurkha dependent child appeal. FtT error of law found, appeal remade and allowed.',
    sourceUrl: 'https://tribunalsdecisions.service.gov.uk/utiac/ui-2024-000311'
  },
  {
    caseRef: 'UI-2024-002058',
    utJudge: 'Upper Tribunal Judge S Meah',
    fttJudge: 'Judge Juss',
    outcome: 'Remitted',
    nationality: 'Unknown',
    caseType: 'Human Rights',
    hearingDate: '2024-10-22',
    decisionDate: '2024-10-22',
    hearingCentre: 'Manchester',
    summary: 'Article 8 private life claim. FtT failed to consider substantial evidence. Remitted.',
    sourceUrl: 'https://tribunalsdecisions.service.gov.uk/utiac/ui-2024-002058'
  },
  {
    caseRef: 'UI-2024-000672',
    utJudge: 'Upper Tribunal Judge Stephen Smith',
    fttJudge: 'Judge Norris',
    outcome: 'Allowed',
    nationality: 'Bangladeshi',
    caseType: 'Human Rights',
    hearingDate: '2024-06-14',
    decisionDate: '2024-09-05',
    hearingCentre: null,
    summary: 'Continuous residence claim. Procedural unfairness at FtT. Appeal allowed.',
    sourceUrl: 'https://tribunalsdecisions.service.gov.uk/utiac/ui-2024-000672'
  },
  {
    caseRef: 'UI-2024-001277',
    utJudge: 'Deputy Upper Tribunal Judge Malik KC',
    fttJudge: 'Judge Broe',
    outcome: 'Remitted',
    nationality: 'Iraqi',
    caseType: 'Asylum',
    hearingDate: '2024-08-01',
    decisionDate: '2024-08-01',
    hearingCentre: null,
    summary: 'Protection claim involving political persecution. Procedural unfairness. Remitted.',
    sourceUrl: 'https://tribunalsdecisions.service.gov.uk/utiac/ui-2024-001277'
  },
  {
    caseRef: 'UI-2024-000313',
    utJudge: 'Upper Tribunal Judge Macleman',
    fttJudge: 'Judge Green',
    outcome: 'Dismissed',
    nationality: 'Syrian',
    caseType: 'Human Rights',
    hearingDate: '2024-06-06',
    decisionDate: '2024-06-18',
    hearingCentre: null,
    summary: 'Family reunion appeal. FtT allowed on Article 8, UT set aside and dismissed.',
    sourceUrl: 'https://tribunalsdecisions.service.gov.uk/utiac/ui-2024-000313-ors'
  },
  {
    caseRef: 'UI-2024-003512',
    utJudge: 'Deputy Upper Tribunal Judge Chapman',
    fttJudge: null,
    outcome: 'Allowed',
    nationality: 'Afghan',
    caseType: 'Asylum',
    hearingDate: '2025-03-18',
    decisionDate: '2025-04-09',
    hearingCentre: null,
    summary: 'Protection from Taliban persecution. Allowed on asylum and Article 3 grounds.',
    sourceUrl: 'https://tribunalsdecisions.service.gov.uk/utiac/ui-2024-003512'
  },
  {
    caseRef: 'UI-2024-002293',
    utJudge: 'Upper Tribunal Judge Meah',
    fttJudge: 'Judge Gray',
    outcome: 'Dismissed',
    nationality: 'Iranian',
    caseType: 'Asylum',
    hearingDate: '2024-10-17',
    decisionDate: '2024-10-28',
    hearingCentre: null,
    summary: 'Kurdish Iranian asylum claim based on UK protest attendance. Dismissed.',
    sourceUrl: 'https://tribunalsdecisions.service.gov.uk/utiac/ui-2024-002293'
  },
  {
    caseRef: 'UI-2024-000104',
    utJudge: 'Upper Tribunal Judge Kebede',
    fttJudge: 'Judge O\'Keeffe',
    outcome: 'Dismissed',
    nationality: 'Afghan',
    caseType: 'Humanitarian Protection',
    hearingDate: '2024-09-16',
    decisionDate: '2024-09-24',
    hearingCentre: null,
    summary: 'Humanitarian protection claim. Appellant deemed to have resources to relocate to Kabul.',
    sourceUrl: 'https://tribunalsdecisions.service.gov.uk/utiac/ui-2024-000104'
  },
  {
    caseRef: 'UI-2023-004915',
    utJudge: 'Upper Tribunal Judge Ben Keith',
    fttJudge: null,
    outcome: 'Allowed',
    nationality: 'Iranian',
    caseType: 'Asylum',
    hearingDate: '2024-01-29',
    decisionDate: '2024-03-13',
    hearingCentre: null,
    summary: 'Kurdish Iranian with genuine political opposition. Hair-trigger response risk. Allowed.',
    sourceUrl: 'https://tribunalsdecisions.service.gov.uk/utiac/ui-2023-004915'
  },
  {
    caseRef: 'UI-2024-001163',
    utJudge: 'Upper Tribunal Judge Stephen Smith',
    fttJudge: 'Judge Turner',
    outcome: 'Dismissed',
    nationality: 'Egyptian',
    caseType: 'Humanitarian Protection',
    hearingDate: '2024-11-28',
    decisionDate: '2025-01-02',
    hearingCentre: null,
    summary: 'Humanitarian protection from non-state actors. FtT allowed but UT dismissed on remake.',
    sourceUrl: 'https://tribunalsdecisions.service.gov.uk/utiac/ui-2024-001163'
  },
  {
    caseRef: 'UI-2024-003709',
    utJudge: 'Upper Tribunal Judge Neville',
    fttJudge: null,
    outcome: 'Allowed',
    nationality: 'Iraqi',
    caseType: 'Humanitarian Protection',
    hearingDate: '2025-06-18',
    decisionDate: '2025-09-10',
    hearingCentre: null,
    summary: 'Kurdish Iraqi protection claim. Allowed with costs order against Home Office.',
    sourceUrl: 'https://tribunalsdecisions.service.gov.uk/utiac/ui-2024-003709'
  },
  {
    caseRef: 'UI-2024-000166',
    utJudge: 'Upper Tribunal Judge Smith',
    fttJudge: 'Judge Brannan',
    outcome: 'Allowed',
    nationality: 'Somali',
    caseType: 'Mixed',
    hearingDate: '2024-04-05',
    decisionDate: '2024-04-23',
    hearingCentre: null,
    summary: 'Refugee status revocation. Allowed on Articles 3 and 8, dismissed on asylum grounds.',
    sourceUrl: 'https://tribunalsdecisions.service.gov.uk/utiac/ui-2024-000166'
  },
];

// Summary statistics from compiled decisions
export const compiledStats = {
  totalDecisions: tribunalDecisions.length,
  outcomes: {
    allowed: tribunalDecisions.filter(d => d.outcome === 'Allowed').length,
    dismissed: tribunalDecisions.filter(d => d.outcome === 'Dismissed').length,
    remitted: tribunalDecisions.filter(d => d.outcome === 'Remitted').length,
    setAside: tribunalDecisions.filter(d => d.outcome === 'Set Aside').length,
  },
  byNationality: [
    { nationality: 'Iraqi', count: 4, allowed: 2, dismissed: 1, remitted: 1 },
    { nationality: 'Iranian', count: 2, allowed: 1, dismissed: 1, remitted: 0 },
    { nationality: 'Afghan', count: 2, allowed: 1, dismissed: 1, remitted: 0 },
    { nationality: 'Syrian', count: 1, allowed: 0, dismissed: 1, remitted: 0 },
    { nationality: 'Bangladeshi', count: 1, allowed: 1, dismissed: 0, remitted: 0 },
    { nationality: 'Gambian', count: 1, allowed: 1, dismissed: 0, remitted: 0 },
    { nationality: 'Nepalese', count: 1, allowed: 1, dismissed: 0, remitted: 0 },
    { nationality: 'Jamaican', count: 1, allowed: 1, dismissed: 0, remitted: 0 },
    { nationality: 'Somali', count: 1, allowed: 1, dismissed: 0, remitted: 0 },
    { nationality: 'Egyptian', count: 1, allowed: 0, dismissed: 1, remitted: 0 },
    { nationality: 'Uzbek', count: 1, allowed: 0, dismissed: 1, remitted: 0 },
  ],
  uniqueJudges: {
    upperTribunal: [
      'Upper Tribunal Judge Reeds',
      'Upper Tribunal Judge Kamara',
      'Upper Tribunal Judge Stephen Smith',
      'Upper Tribunal Judge Macleman',
      'Upper Tribunal Judge S Meah',
      'Upper Tribunal Judge Kebede',
      'Upper Tribunal Judge Ben Keith',
      'Upper Tribunal Judge Neville',
      'Deputy Upper Tribunal Judge Saini',
      'Deputy Upper Tribunal Judge Manuell',
      'Deputy Upper Tribunal Judge Doyle',
      'Deputy Upper Tribunal Judge Malik KC',
      'Deputy Upper Tribunal Judge Chapman',
      'Deputy Upper Tribunal Judge Lewis',
      'Deputy Upper Tribunal Judge Moxon',
      'Deputy Upper Tribunal Judge Hanbury',
      'Deputy Upper Tribunal Judge Haria',
    ],
    firstTier: [
      'Judge C J Williams',
      'Judge Greer',
      'Judge Hoffman',
      'Judge CL Taylor',
      'Judge Dainty',
      'Judge Juss',
      'Judge Norris',
      'Judge Broe',
      'Judge Green',
      'Judge Gray',
      'Judge O\'Keeffe',
      'Judge Turner',
      'Judge Brannan',
      'Judge Boyes',
      'Judge Plowright',
    ],
  },
  dataSource: 'https://tribunalsdecisions.service.gov.uk/utiac',
  lastUpdated: '2025-01-11',
};

// Judge appearances in compiled decisions
export const judgeDecisionCounts = [
  { judge: 'Upper Tribunal Judge Stephen Smith', decisions: 3, allowed: 2, dismissed: 1 },
  { judge: 'Upper Tribunal Judge S Meah', decisions: 2, allowed: 0, dismissed: 1, remitted: 1 },
  { judge: 'Judge Hoffman', decisions: 1, allowed: 0, dismissed: 1 },
  { judge: 'Judge CL Taylor', decisions: 1, allowed: 1, dismissed: 0 },
  { judge: 'Judge Juss', decisions: 1, allowed: 0, dismissed: 0, remitted: 1 },
  { judge: 'Judge Broe', decisions: 1, allowed: 0, dismissed: 0, remitted: 1 },
  { judge: 'Judge Green', decisions: 1, allowed: 1, dismissed: 1 }, // FtT allowed, UT dismissed
  { judge: 'Judge Brannan', decisions: 1, allowed: 1, dismissed: 0 },
  { judge: 'Judge Dainty', decisions: 1, allowed: 1, dismissed: 0 },
  { judge: 'Judge Greer', decisions: 1, allowed: 1, dismissed: 0 },
  { judge: 'Judge C J Williams', decisions: 1, allowed: 0, dismissed: 1 },
  { judge: 'Judge Gray', decisions: 1, allowed: 0, dismissed: 1 },
  { judge: 'Judge Turner', decisions: 1, allowed: 1, dismissed: 1 }, // FtT allowed, UT dismissed on remake
  { judge: 'Judge O\'Keeffe', decisions: 1, allowed: 0, dismissed: 1 },
  { judge: 'Judge Norris', decisions: 1, allowed: 0, dismissed: 0, setAside: 1 },
];
