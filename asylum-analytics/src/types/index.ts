// Asylum case data types based on EOIR/TRAC data structures

export interface AsylumCase {
  id: string;
  fiscalYear: number;
  nationality: string;
  court: string;
  state: string;
  judgeId: string;
  judgeName: string;
  caseType: 'Affirmative' | 'Defensive';
  outcome: 'Granted' | 'Denied' | 'Withdrawn' | 'Abandoned' | 'Other' | 'Pending';
  hasAttorney: boolean;
  filingDate: string;
  decisionDate: string | null;
  processingDays: number | null;
  language: string;
  detained: boolean;
  appealFiled: boolean;
  appealOutcome: 'Granted' | 'Denied' | 'Pending' | null;
}

export interface YearlyTrend {
  year: number;
  applications: number;
  granted: number;
  denied: number;
  pending: number;
  grantRate: number;
}

export interface NationalityStats {
  nationality: string;
  totalCases: number;
  granted: number;
  denied: number;
  grantRate: number;
  avgProcessingDays: number;
  representationRate: number;
}

export interface CourtStats {
  court: string;
  state: string;
  totalCases: number;
  granted: number;
  denied: number;
  grantRate: number;
  avgProcessingDays: number;
  backlog: number;
}

export interface JudgeStats {
  judgeId: string;
  judgeName: string;
  court: string;
  totalDecisions: number;
  granted: number;
  denied: number;
  grantRate: number;
  avgProcessingDays: number;
}

export interface BacklogData {
  year: number;
  month: number;
  pendingCases: number;
  newFilings: number;
  completions: number;
  netChange: number;
}

export interface RepresentationImpact {
  category: string;
  withAttorney: number;
  withoutAttorney: number;
}

export interface FilterState {
  years: number[];
  nationalities: string[];
  courts: string[];
  outcomes: string[];
  hasAttorney: boolean | null;
  detained: boolean | null;
}

export type TabType =
  | 'overview'
  | 'trends'
  | 'nationality'
  | 'courts'
  | 'judges'
  | 'proceedings'
  | 'representation'
  | 'backlog';
