import {
  PieChart,
  Pie,
  Cell,
  ResponsiveContainer,
  BarChart,
  Bar,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  Legend,
  Sankey,
  Layer,
  Rectangle,
} from 'recharts';
import ChartCard from '../ui/ChartCard';
import StatCard from '../ui/StatCard';
import {
  caseOutcomes,
  appealStats,
  detentionOutcomes,
  processingTimeTrends,
} from '../../data/asylumData';
import {
  FileText,
  Gavel,
  Clock,
  AlertTriangle,
  CheckCircle,
  XCircle,
  Scale,
  Building,
} from 'lucide-react';

const formatNumber = (num: number) => {
  if (num >= 1000000) return (num / 1000000).toFixed(2) + 'M';
  if (num >= 1000) return (num / 1000).toFixed(1) + 'K';
  return num.toString();
};

const OUTCOME_COLORS = {
  granted: '#22c55e',
  denied: '#ef4444',
  withdrawn: '#f59e0b',
  abandoned: '#94a3b8',
  administrativelyClosed: '#6366f1',
  other: '#ec4899',
};

const outcomeData = [
  { name: 'Granted', value: caseOutcomes.granted, color: OUTCOME_COLORS.granted },
  { name: 'Denied', value: caseOutcomes.denied, color: OUTCOME_COLORS.denied },
  { name: 'Withdrawn', value: caseOutcomes.withdrawn, color: OUTCOME_COLORS.withdrawn },
  { name: 'Abandoned', value: caseOutcomes.abandoned, color: OUTCOME_COLORS.abandoned },
  { name: 'Admin. Closed', value: caseOutcomes.administrativelyClosed, color: OUTCOME_COLORS.administrativelyClosed },
  { name: 'Other', value: caseOutcomes.other, color: OUTCOME_COLORS.other },
];

const detentionComparisonData = [
  {
    metric: 'Grant Rate',
    detained: detentionOutcomes.detained.grantRate,
    nonDetained: detentionOutcomes.nonDetained.grantRate,
  },
  {
    metric: 'Representation Rate',
    detained: detentionOutcomes.detained.representationRate,
    nonDetained: detentionOutcomes.nonDetained.representationRate,
  },
];

const appealOutcomeData = [
  {
    name: 'Appeals Filed',
    bia: appealStats.biaAppeals,
    federal: appealStats.federalCourtAppeals,
  },
];

export default function ProceedingsTab() {
  const totalCases = Object.values(caseOutcomes).reduce((sum, val) => sum + val, 0);
  const grantRate = ((caseOutcomes.granted / (caseOutcomes.granted + caseOutcomes.denied)) * 100).toFixed(1);
  const completionRate = (((caseOutcomes.granted + caseOutcomes.denied) / totalCases) * 100).toFixed(1);

  return (
    <div className="space-y-6">
      <div>
        <h2 className="text-2xl font-bold text-slate-800 mb-2">
          Court Proceedings Analysis
        </h2>
        <p className="text-slate-500">
          Detailed breakdown of asylum case outcomes, appeals, and detention impacts
        </p>
      </div>

      {/* Summary Stats */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        <StatCard
          title="Total Cases Analyzed"
          value={formatNumber(totalCases)}
          icon={<FileText className="w-5 h-5" />}
          color="blue"
        />
        <StatCard
          title="Overall Grant Rate"
          value={`${grantRate}%`}
          subtitle="Of decided cases"
          icon={<CheckCircle className="w-5 h-5" />}
          color="green"
        />
        <StatCard
          title="Appeals Filed"
          value={formatNumber(appealStats.totalAppeals)}
          subtitle={`${appealStats.appealSuccessRate}% success rate`}
          icon={<Scale className="w-5 h-5" />}
          color="purple"
        />
        <StatCard
          title="Completion Rate"
          value={`${completionRate}%`}
          subtitle="Cases with final decision"
          icon={<Gavel className="w-5 h-5" />}
          color="orange"
        />
      </div>

      {/* Case Outcomes */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <ChartCard
          title="Case Outcome Distribution"
          subtitle="All asylum case dispositions"
        >
          <div className="h-80">
            <ResponsiveContainer width="100%" height="100%">
              <PieChart>
                <Pie
                  data={outcomeData}
                  cx="50%"
                  cy="50%"
                  innerRadius={60}
                  outerRadius={100}
                  paddingAngle={2}
                  dataKey="value"
                  label={({ name, percent }) =>
                    `${name} ${(percent * 100).toFixed(1)}%`
                  }
                  labelLine={{ stroke: '#94a3b8' }}
                >
                  {outcomeData.map((entry, index) => (
                    <Cell key={`cell-${index}`} fill={entry.color} />
                  ))}
                </Pie>
                <Tooltip
                  formatter={(value: number) => formatNumber(value)}
                  contentStyle={{
                    backgroundColor: 'white',
                    border: '1px solid #e2e8f0',
                    borderRadius: '8px',
                  }}
                />
                <Legend />
              </PieChart>
            </ResponsiveContainer>
          </div>
        </ChartCard>

        <ChartCard
          title="Outcome Breakdown"
          subtitle="Detailed case disposition counts"
        >
          <div className="space-y-4">
            {outcomeData.map((outcome) => (
              <div key={outcome.name} className="flex items-center gap-4">
                <div
                  className="w-4 h-4 rounded-full flex-shrink-0"
                  style={{ backgroundColor: outcome.color }}
                />
                <div className="flex-1">
                  <div className="flex items-center justify-between mb-1">
                    <span className="font-medium text-slate-700">{outcome.name}</span>
                    <span className="text-slate-600">
                      {formatNumber(outcome.value)} ({((outcome.value / totalCases) * 100).toFixed(1)}%)
                    </span>
                  </div>
                  <div className="w-full h-2 bg-slate-100 rounded-full overflow-hidden">
                    <div
                      className="h-full rounded-full"
                      style={{
                        width: `${(outcome.value / Math.max(...outcomeData.map(o => o.value))) * 100}%`,
                        backgroundColor: outcome.color,
                      }}
                    />
                  </div>
                </div>
              </div>
            ))}
          </div>
        </ChartCard>
      </div>

      {/* Detention Impact */}
      <ChartCard
        title="Impact of Detention on Outcomes"
        subtitle="Comparing detained vs non-detained asylum seekers"
      >
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
          <div className="lg:col-span-1">
            <div className="space-y-4">
              <div className="p-4 bg-orange-50 border border-orange-200 rounded-lg">
                <div className="flex items-center gap-2 mb-2">
                  <Building className="w-5 h-5 text-orange-600" />
                  <span className="font-semibold text-orange-800">Detained Cases</span>
                </div>
                <div className="space-y-2 text-sm">
                  <div className="flex justify-between">
                    <span className="text-orange-700">Total Cases:</span>
                    <span className="font-medium">{formatNumber(detentionOutcomes.detained.totalCases)}</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-orange-700">Grant Rate:</span>
                    <span className="font-medium text-red-600">{detentionOutcomes.detained.grantRate}%</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-orange-700">Representation:</span>
                    <span className="font-medium">{detentionOutcomes.detained.representationRate}%</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-orange-700">Avg Processing:</span>
                    <span className="font-medium">{detentionOutcomes.detained.avgProcessingDays} days</span>
                  </div>
                </div>
              </div>

              <div className="p-4 bg-blue-50 border border-blue-200 rounded-lg">
                <div className="flex items-center gap-2 mb-2">
                  <FileText className="w-5 h-5 text-blue-600" />
                  <span className="font-semibold text-blue-800">Non-Detained Cases</span>
                </div>
                <div className="space-y-2 text-sm">
                  <div className="flex justify-between">
                    <span className="text-blue-700">Total Cases:</span>
                    <span className="font-medium">{formatNumber(detentionOutcomes.nonDetained.totalCases)}</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-blue-700">Grant Rate:</span>
                    <span className="font-medium text-green-600">{detentionOutcomes.nonDetained.grantRate}%</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-blue-700">Representation:</span>
                    <span className="font-medium">{detentionOutcomes.nonDetained.representationRate}%</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-blue-700">Avg Processing:</span>
                    <span className="font-medium">{detentionOutcomes.nonDetained.avgProcessingDays} days</span>
                  </div>
                </div>
              </div>
            </div>
          </div>

          <div className="lg:col-span-2">
            <div className="h-64">
              <ResponsiveContainer width="100%" height="100%">
                <BarChart data={detentionComparisonData}>
                  <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                  <XAxis dataKey="metric" tick={{ fill: '#64748b', fontSize: 12 }} />
                  <YAxis
                    tick={{ fill: '#64748b', fontSize: 12 }}
                    tickFormatter={(value) => `${value}%`}
                    domain={[0, 60]}
                  />
                  <Tooltip
                    contentStyle={{
                      backgroundColor: 'white',
                      border: '1px solid #e2e8f0',
                      borderRadius: '8px',
                    }}
                    formatter={(value: number) => [`${value}%`]}
                  />
                  <Legend />
                  <Bar dataKey="detained" name="Detained" fill="#f59e0b" radius={[4, 4, 0, 0]} />
                  <Bar dataKey="nonDetained" name="Non-Detained" fill="#3b82f6" radius={[4, 4, 0, 0]} />
                </BarChart>
              </ResponsiveContainer>
            </div>
            <div className="mt-4 p-4 bg-red-50 rounded-lg">
              <p className="text-sm text-red-800">
                <strong>Critical Finding:</strong> Detained asylum seekers have a grant rate
                of {detentionOutcomes.detained.grantRate}% compared to {detentionOutcomes.nonDetained.grantRate}%
                for non-detained individuals. This {Math.round((detentionOutcomes.nonDetained.grantRate / detentionOutcomes.detained.grantRate - 1) * 100)}%
                difference is partly explained by lower representation rates ({detentionOutcomes.detained.representationRate}%
                vs {detentionOutcomes.nonDetained.representationRate}%).
              </p>
            </div>
          </div>
        </div>
      </ChartCard>

      {/* Appeals Analysis */}
      <ChartCard
        title="Appeals Process Analysis"
        subtitle="Board of Immigration Appeals (BIA) and Federal Court appeals"
      >
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
          <div className="space-y-4">
            <div className="p-4 bg-slate-50 rounded-lg">
              <h4 className="font-semibold text-slate-800 mb-3">Appeal Statistics</h4>
              <div className="space-y-3">
                <div className="flex justify-between items-center">
                  <span className="text-slate-600">Total Appeals Filed</span>
                  <span className="font-bold text-slate-800">
                    {formatNumber(appealStats.totalAppeals)}
                  </span>
                </div>
                <div className="flex justify-between items-center">
                  <span className="text-slate-600">BIA Appeals</span>
                  <span className="font-medium text-purple-600">
                    {formatNumber(appealStats.biaAppeals)}
                  </span>
                </div>
                <div className="flex justify-between items-center">
                  <span className="text-slate-600">Federal Court Appeals</span>
                  <span className="font-medium text-blue-600">
                    {formatNumber(appealStats.federalCourtAppeals)}
                  </span>
                </div>
                <div className="flex justify-between items-center pt-3 border-t border-slate-200">
                  <span className="text-slate-600">Appeal Success Rate</span>
                  <span className="font-bold text-green-600">
                    {appealStats.appealSuccessRate}%
                  </span>
                </div>
                <div className="flex justify-between items-center">
                  <span className="text-slate-600">Avg Appeal Duration</span>
                  <span className="font-medium text-slate-800">
                    {appealStats.avgAppealDays} days
                  </span>
                </div>
              </div>
            </div>
          </div>

          <div className="lg:col-span-2">
            <div className="h-64">
              <ResponsiveContainer width="100%" height="100%">
                <BarChart data={[
                  { stage: 'BIA Appeals', filed: appealStats.biaAppeals, success: Math.round(appealStats.biaAppeals * appealStats.appealSuccessRate / 100) },
                  { stage: 'Federal Court', filed: appealStats.federalCourtAppeals, success: Math.round(appealStats.federalCourtAppeals * 0.12) },
                ]}>
                  <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                  <XAxis dataKey="stage" tick={{ fill: '#64748b', fontSize: 12 }} />
                  <YAxis
                    tick={{ fill: '#64748b', fontSize: 12 }}
                    tickFormatter={(value) => formatNumber(value)}
                  />
                  <Tooltip
                    contentStyle={{
                      backgroundColor: 'white',
                      border: '1px solid #e2e8f0',
                      borderRadius: '8px',
                    }}
                    formatter={(value: number) => formatNumber(value)}
                  />
                  <Legend />
                  <Bar dataKey="filed" name="Appeals Filed" fill="#8b5cf6" radius={[4, 4, 0, 0]} />
                  <Bar dataKey="success" name="Successful" fill="#22c55e" radius={[4, 4, 0, 0]} />
                </BarChart>
              </ResponsiveContainer>
            </div>
          </div>
        </div>
      </ChartCard>

      {/* Processing Time by Case Type */}
      <ChartCard
        title="Processing Time Trends"
        subtitle="Average days to decision by case type over time"
      >
        <div className="h-80">
          <ResponsiveContainer width="100%" height="100%">
            <BarChart data={processingTimeTrends}>
              <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
              <XAxis dataKey="year" tick={{ fill: '#64748b', fontSize: 12 }} />
              <YAxis
                tick={{ fill: '#64748b', fontSize: 12 }}
                label={{
                  value: 'Days',
                  angle: -90,
                  position: 'insideLeft',
                  fill: '#64748b',
                }}
              />
              <Tooltip
                contentStyle={{
                  backgroundColor: 'white',
                  border: '1px solid #e2e8f0',
                  borderRadius: '8px',
                }}
                formatter={(value: number) => [`${value} days`]}
              />
              <Legend />
              <Bar
                dataKey="affirmative"
                name="Affirmative (USCIS)"
                fill="#8b5cf6"
                radius={[4, 4, 0, 0]}
              />
              <Bar
                dataKey="defensive"
                name="Defensive (Court)"
                fill="#ec4899"
                radius={[4, 4, 0, 0]}
              />
            </BarChart>
          </ResponsiveContainer>
        </div>
        <div className="mt-4 grid grid-cols-1 md:grid-cols-2 gap-4">
          <div className="p-4 bg-purple-50 rounded-lg">
            <div className="flex items-center gap-2 mb-1">
              <div className="w-3 h-3 bg-purple-500 rounded" />
              <span className="font-medium text-purple-800">Affirmative Cases</span>
            </div>
            <p className="text-sm text-purple-700">
              Filed with USCIS before removal proceedings. Currently averaging{' '}
              {processingTimeTrends[processingTimeTrends.length - 1].affirmative} days
              ({Math.round(processingTimeTrends[processingTimeTrends.length - 1].affirmative / 30)} months).
            </p>
          </div>
          <div className="p-4 bg-pink-50 rounded-lg">
            <div className="flex items-center gap-2 mb-1">
              <div className="w-3 h-3 bg-pink-500 rounded" />
              <span className="font-medium text-pink-800">Defensive Cases</span>
            </div>
            <p className="text-sm text-pink-700">
              Filed in immigration court as defense to removal. Currently averaging{' '}
              {processingTimeTrends[processingTimeTrends.length - 1].defensive} days
              ({Math.round(processingTimeTrends[processingTimeTrends.length - 1].defensive / 365 * 10) / 10} years).
            </p>
          </div>
        </div>
      </ChartCard>
    </div>
  );
}
