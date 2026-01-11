import {
  Users,
  CheckCircle,
  XCircle,
  Clock,
  TrendingUp,
  Scale,
  Building2,
  Globe2,
} from 'lucide-react';
import {
  AreaChart,
  Area,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  ResponsiveContainer,
  PieChart,
  Pie,
  Cell,
  Legend,
} from 'recharts';
import StatCard from '../ui/StatCard';
import ChartCard from '../ui/ChartCard';
import {
  yearlyTrends,
  caseOutcomes,
  nationalityStats,
  courtStats,
} from '../../data/asylumData';

const OUTCOME_COLORS = ['#22c55e', '#ef4444', '#f59e0b', '#94a3b8', '#6366f1', '#ec4899'];

const outcomeData = [
  { name: 'Granted', value: caseOutcomes.granted, color: OUTCOME_COLORS[0] },
  { name: 'Denied', value: caseOutcomes.denied, color: OUTCOME_COLORS[1] },
  { name: 'Withdrawn', value: caseOutcomes.withdrawn, color: OUTCOME_COLORS[2] },
  { name: 'Abandoned', value: caseOutcomes.abandoned, color: OUTCOME_COLORS[3] },
  { name: 'Admin. Closed', value: caseOutcomes.administrativelyClosed, color: OUTCOME_COLORS[4] },
  { name: 'Other', value: caseOutcomes.other, color: OUTCOME_COLORS[5] },
];

const formatNumber = (num: number) => {
  if (num >= 1000000) return (num / 1000000).toFixed(2) + 'M';
  if (num >= 1000) return (num / 1000).toFixed(1) + 'K';
  return num.toString();
};

export default function OverviewTab() {
  const latestYear = yearlyTrends[yearlyTrends.length - 1];
  const previousYear = yearlyTrends[yearlyTrends.length - 2];

  const applicationsTrend =
    ((latestYear.applications - previousYear.applications) /
      previousYear.applications) *
    100;
  const grantRateTrend = latestYear.grantRate - previousYear.grantRate;

  const topNationalities = [...nationalityStats]
    .sort((a, b) => b.totalCases - a.totalCases)
    .slice(0, 5);

  const topCourts = [...courtStats]
    .sort((a, b) => b.totalCases - a.totalCases)
    .slice(0, 5);

  return (
    <div className="space-y-6">
      <div>
        <h2 className="text-2xl font-bold text-slate-800 mb-2">
          Asylum Data Overview
        </h2>
        <p className="text-slate-500">
          Comprehensive analysis of U.S. asylum cases from FY2015 to FY2024
        </p>
      </div>

      {/* Key Stats */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        <StatCard
          title="FY2024 Applications"
          value={formatNumber(latestYear.applications)}
          icon={<Users className="w-5 h-5" />}
          trend={{ value: Math.round(applicationsTrend), label: 'vs FY2023' }}
          color="blue"
        />
        <StatCard
          title="Cases Granted"
          value={formatNumber(latestYear.granted)}
          subtitle={`${latestYear.grantRate}% grant rate`}
          icon={<CheckCircle className="w-5 h-5" />}
          trend={{ value: Math.round(grantRateTrend * 10) / 10, label: 'vs FY2023' }}
          color="green"
        />
        <StatCard
          title="Cases Denied"
          value={formatNumber(latestYear.denied)}
          icon={<XCircle className="w-5 h-5" />}
          color="red"
        />
        <StatCard
          title="Pending Backlog"
          value={formatNumber(latestYear.pending)}
          subtitle="Awaiting decision"
          icon={<Clock className="w-5 h-5" />}
          color="orange"
        />
      </div>

      {/* Charts Row */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <ChartCard
          title="Asylum Applications Trend"
          subtitle="FY2015 - FY2024"
        >
          <div className="h-80">
            <ResponsiveContainer width="100%" height="100%">
              <AreaChart data={yearlyTrends}>
                <defs>
                  <linearGradient id="colorApplications" x1="0" y1="0" x2="0" y2="1">
                    <stop offset="5%" stopColor="#3b82f6" stopOpacity={0.3} />
                    <stop offset="95%" stopColor="#3b82f6" stopOpacity={0} />
                  </linearGradient>
                </defs>
                <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                <XAxis
                  dataKey="year"
                  tick={{ fill: '#64748b', fontSize: 12 }}
                  axisLine={{ stroke: '#e2e8f0' }}
                />
                <YAxis
                  tick={{ fill: '#64748b', fontSize: 12 }}
                  axisLine={{ stroke: '#e2e8f0' }}
                  tickFormatter={(value) => formatNumber(value)}
                />
                <Tooltip
                  contentStyle={{
                    backgroundColor: 'white',
                    border: '1px solid #e2e8f0',
                    borderRadius: '8px',
                    boxShadow: '0 4px 6px -1px rgb(0 0 0 / 0.1)',
                  }}
                  formatter={(value: number) => [formatNumber(value), 'Applications']}
                />
                <Area
                  type="monotone"
                  dataKey="applications"
                  stroke="#3b82f6"
                  strokeWidth={2}
                  fill="url(#colorApplications)"
                />
              </AreaChart>
            </ResponsiveContainer>
          </div>
        </ChartCard>

        <ChartCard
          title="Case Outcomes Distribution"
          subtitle="All completed cases"
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
      </div>

      {/* Quick Stats Tables */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <ChartCard
          title="Top 5 Nationalities"
          subtitle="By total case volume"
          actions={
            <Globe2 className="w-5 h-5 text-slate-400" />
          }
        >
          <div className="space-y-3">
            {topNationalities.map((nat, index) => (
              <div
                key={nat.nationality}
                className="flex items-center justify-between p-3 bg-slate-50 rounded-lg"
              >
                <div className="flex items-center gap-3">
                  <span className="w-6 h-6 flex items-center justify-center bg-blue-100 text-blue-600 rounded-full text-sm font-semibold">
                    {index + 1}
                  </span>
                  <span className="font-medium text-slate-700">
                    {nat.nationality}
                  </span>
                </div>
                <div className="flex items-center gap-4">
                  <span className="text-sm text-slate-500">
                    {formatNumber(nat.totalCases)} cases
                  </span>
                  <span
                    className={`px-2 py-0.5 rounded text-sm font-medium ${
                      nat.grantRate >= 50
                        ? 'bg-green-100 text-green-700'
                        : nat.grantRate >= 30
                        ? 'bg-yellow-100 text-yellow-700'
                        : 'bg-red-100 text-red-700'
                    }`}
                  >
                    {nat.grantRate}%
                  </span>
                </div>
              </div>
            ))}
          </div>
        </ChartCard>

        <ChartCard
          title="Busiest Immigration Courts"
          subtitle="By case volume"
          actions={
            <Building2 className="w-5 h-5 text-slate-400" />
          }
        >
          <div className="space-y-3">
            {topCourts.map((court, index) => (
              <div
                key={court.court}
                className="flex items-center justify-between p-3 bg-slate-50 rounded-lg"
              >
                <div className="flex items-center gap-3">
                  <span className="w-6 h-6 flex items-center justify-center bg-purple-100 text-purple-600 rounded-full text-sm font-semibold">
                    {index + 1}
                  </span>
                  <div>
                    <span className="font-medium text-slate-700">
                      {court.court}
                    </span>
                    <span className="text-slate-400 ml-2 text-sm">
                      {court.state}
                    </span>
                  </div>
                </div>
                <div className="flex items-center gap-4">
                  <span className="text-sm text-slate-500">
                    {formatNumber(court.backlog)} backlog
                  </span>
                  <span
                    className={`px-2 py-0.5 rounded text-sm font-medium ${
                      court.grantRate >= 50
                        ? 'bg-green-100 text-green-700'
                        : court.grantRate >= 35
                        ? 'bg-yellow-100 text-yellow-700'
                        : 'bg-red-100 text-red-700'
                    }`}
                  >
                    {court.grantRate}%
                  </span>
                </div>
              </div>
            ))}
          </div>
        </ChartCard>
      </div>

      {/* Summary Stats */}
      <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
        <StatCard
          title="Total Courts"
          value={courtStats.length}
          icon={<Building2 className="w-5 h-5" />}
          color="purple"
        />
        <StatCard
          title="Nationalities Tracked"
          value={nationalityStats.length}
          icon={<Globe2 className="w-5 h-5" />}
          color="blue"
        />
        <StatCard
          title="Avg Grant Rate"
          value="42.3%"
          subtitle="10-year average"
          icon={<TrendingUp className="w-5 h-5" />}
          color="green"
        />
        <StatCard
          title="Avg Processing"
          value="4.3 years"
          subtitle="Defensive cases"
          icon={<Scale className="w-5 h-5" />}
          color="orange"
        />
      </div>
    </div>
  );
}
