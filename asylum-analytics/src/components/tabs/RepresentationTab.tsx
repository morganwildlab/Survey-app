import {
  BarChart,
  Bar,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  Legend,
  ResponsiveContainer,
  PieChart,
  Pie,
  Cell,
  LineChart,
  Line,
} from 'recharts';
import ChartCard from '../ui/ChartCard';
import StatCard from '../ui/StatCard';
import { representationImpact, nationalityStats } from '../../data/asylumData';
import { Users, Scale, TrendingUp, AlertCircle, UserCheck, UserX } from 'lucide-react';

const formatNumber = (num: number) => {
  if (num >= 1000000) return (num / 1000000).toFixed(2) + 'M';
  if (num >= 1000) return (num / 1000).toFixed(1) + 'K';
  return num.toString();
};

// Calculate overall representation stats
const avgRepresentationRate =
  nationalityStats.reduce((sum, n) => sum + n.representationRate, 0) / nationalityStats.length;

const highRepNations = nationalityStats.filter((n) => n.representationRate >= 60);
const lowRepNations = nationalityStats.filter((n) => n.representationRate < 40);

// Grant rate by representation quartile
const sortedByRep = [...nationalityStats].sort(
  (a, b) => a.representationRate - b.representationRate
);
const quartileSize = Math.ceil(sortedByRep.length / 4);
const quartileData = [
  {
    quartile: 'Q1 (Lowest)',
    avgRep: sortedByRep.slice(0, quartileSize).reduce((s, n) => s + n.representationRate, 0) / quartileSize,
    avgGrant: sortedByRep.slice(0, quartileSize).reduce((s, n) => s + n.grantRate, 0) / quartileSize,
  },
  {
    quartile: 'Q2',
    avgRep: sortedByRep.slice(quartileSize, quartileSize * 2).reduce((s, n) => s + n.representationRate, 0) / quartileSize,
    avgGrant: sortedByRep.slice(quartileSize, quartileSize * 2).reduce((s, n) => s + n.grantRate, 0) / quartileSize,
  },
  {
    quartile: 'Q3',
    avgRep: sortedByRep.slice(quartileSize * 2, quartileSize * 3).reduce((s, n) => s + n.representationRate, 0) / quartileSize,
    avgGrant: sortedByRep.slice(quartileSize * 2, quartileSize * 3).reduce((s, n) => s + n.grantRate, 0) / quartileSize,
  },
  {
    quartile: 'Q4 (Highest)',
    avgRep: sortedByRep.slice(quartileSize * 3).reduce((s, n) => s + n.representationRate, 0) / (sortedByRep.length - quartileSize * 3),
    avgGrant: sortedByRep.slice(quartileSize * 3).reduce((s, n) => s + n.grantRate, 0) / (sortedByRep.length - quartileSize * 3),
  },
];

// Representation by nationality
const topRepNations = [...nationalityStats]
  .sort((a, b) => b.representationRate - a.representationRate)
  .slice(0, 10);

const bottomRepNations = [...nationalityStats]
  .sort((a, b) => a.representationRate - b.representationRate)
  .slice(0, 10);

export default function RepresentationTab() {
  const withAttorneyGrant = representationImpact.find(
    (r) => r.category === 'Grant Rate (%)'
  );
  const grantRateGap = withAttorneyGrant
    ? withAttorneyGrant.withAttorney - withAttorneyGrant.withoutAttorney
    : 0;

  return (
    <div className="space-y-6">
      <div>
        <h2 className="text-2xl font-bold text-slate-800 mb-2">
          Legal Representation Analysis
        </h2>
        <p className="text-slate-500">
          Examining the critical impact of legal representation on asylum outcomes
        </p>
      </div>

      {/* Summary Stats */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        <StatCard
          title="Avg Representation Rate"
          value={`${avgRepresentationRate.toFixed(1)}%`}
          subtitle="Across all nationalities"
          icon={<Users className="w-5 h-5" />}
          color="blue"
        />
        <StatCard
          title="Grant Rate Gap"
          value={`+${grantRateGap.toFixed(1)}%`}
          subtitle="With attorney vs without"
          icon={<Scale className="w-5 h-5" />}
          color="green"
        />
        <StatCard
          title="High Representation"
          value={highRepNations.length}
          subtitle="Nationalities with 60%+ rep"
          icon={<UserCheck className="w-5 h-5" />}
          color="green"
        />
        <StatCard
          title="Low Representation"
          value={lowRepNations.length}
          subtitle="Nationalities with <40% rep"
          icon={<UserX className="w-5 h-5" />}
          color="red"
        />
      </div>

      {/* Key Impact Comparison */}
      <ChartCard
        title="Impact of Legal Representation"
        subtitle="Outcomes with attorney vs without attorney"
      >
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          <div className="h-80">
            <ResponsiveContainer width="100%" height="100%">
              <BarChart data={representationImpact} layout="vertical">
                <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                <XAxis
                  type="number"
                  tick={{ fill: '#64748b', fontSize: 11 }}
                  domain={[0, 100]}
                />
                <YAxis
                  type="category"
                  dataKey="category"
                  tick={{ fill: '#64748b', fontSize: 11 }}
                  width={140}
                />
                <Tooltip
                  contentStyle={{
                    backgroundColor: 'white',
                    border: '1px solid #e2e8f0',
                    borderRadius: '8px',
                  }}
                />
                <Legend />
                <Bar
                  dataKey="withAttorney"
                  name="With Attorney"
                  fill="#22c55e"
                  radius={[0, 4, 4, 0]}
                />
                <Bar
                  dataKey="withoutAttorney"
                  name="Without Attorney"
                  fill="#ef4444"
                  radius={[0, 4, 4, 0]}
                />
              </BarChart>
            </ResponsiveContainer>
          </div>

          <div className="space-y-4">
            {representationImpact.map((item) => (
              <div
                key={item.category}
                className="p-4 bg-slate-50 rounded-lg"
              >
                <h4 className="font-semibold text-slate-800 mb-2">
                  {item.category}
                </h4>
                <div className="flex items-center gap-4">
                  <div className="flex-1">
                    <div className="flex items-center justify-between mb-1">
                      <span className="text-sm text-green-600">With Attorney</span>
                      <span className="font-bold text-green-700">
                        {item.withAttorney}
                        {item.category.includes('%') ? '%' : ''}
                      </span>
                    </div>
                    <div className="w-full h-2 bg-slate-200 rounded-full overflow-hidden">
                      <div
                        className="h-full bg-green-500 rounded-full"
                        style={{ width: `${item.withAttorney}%` }}
                      />
                    </div>
                  </div>
                  <div className="flex-1">
                    <div className="flex items-center justify-between mb-1">
                      <span className="text-sm text-red-600">Without Attorney</span>
                      <span className="font-bold text-red-700">
                        {item.withoutAttorney}
                        {item.category.includes('%') ? '%' : ''}
                      </span>
                    </div>
                    <div className="w-full h-2 bg-slate-200 rounded-full overflow-hidden">
                      <div
                        className="h-full bg-red-500 rounded-full"
                        style={{ width: `${item.withoutAttorney}%` }}
                      />
                    </div>
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>

        <div className="mt-4 p-4 bg-green-50 border border-green-200 rounded-lg">
          <p className="text-sm text-green-800">
            <strong>Key Finding:</strong> Asylum seekers with legal representation
            have a {Math.round((representationImpact[0].withAttorney / representationImpact[0].withoutAttorney) * 10) / 10}x
            higher grant rate than those without representation. This demonstrates
            the critical importance of access to counsel in asylum proceedings.
          </p>
        </div>
      </ChartCard>

      {/* Grant Rate by Representation Quartile */}
      <ChartCard
        title="Grant Rate by Representation Level"
        subtitle="Nationalities grouped by representation rate quartile"
      >
        <div className="h-80">
          <ResponsiveContainer width="100%" height="100%">
            <BarChart data={quartileData}>
              <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
              <XAxis dataKey="quartile" tick={{ fill: '#64748b', fontSize: 12 }} />
              <YAxis
                tick={{ fill: '#64748b', fontSize: 12 }}
                tickFormatter={(value) => `${value}%`}
              />
              <Tooltip
                contentStyle={{
                  backgroundColor: 'white',
                  border: '1px solid #e2e8f0',
                  borderRadius: '8px',
                }}
                formatter={(value: number) => [`${value.toFixed(1)}%`]}
              />
              <Legend />
              <Bar
                dataKey="avgRep"
                name="Avg Representation Rate"
                fill="#3b82f6"
                radius={[4, 4, 0, 0]}
              />
              <Bar
                dataKey="avgGrant"
                name="Avg Grant Rate"
                fill="#22c55e"
                radius={[4, 4, 0, 0]}
              />
            </BarChart>
          </ResponsiveContainer>
        </div>
      </ChartCard>

      {/* Nationalities by Representation */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <ChartCard
          title="Highest Representation Rates"
          subtitle="Top 10 nationalities by attorney representation"
        >
          <div className="h-80">
            <ResponsiveContainer width="100%" height="100%">
              <BarChart data={topRepNations} layout="vertical">
                <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                <XAxis
                  type="number"
                  tick={{ fill: '#64748b', fontSize: 11 }}
                  domain={[0, 100]}
                  tickFormatter={(value) => `${value}%`}
                />
                <YAxis
                  type="category"
                  dataKey="nationality"
                  tick={{ fill: '#64748b', fontSize: 11 }}
                  width={80}
                />
                <Tooltip
                  contentStyle={{
                    backgroundColor: 'white',
                    border: '1px solid #e2e8f0',
                    borderRadius: '8px',
                  }}
                  formatter={(value: number, name: string, props: { payload: typeof nationalityStats[0] }) => [
                    `${value}%`,
                    `Representation (Grant: ${props.payload.grantRate}%)`,
                  ]}
                />
                <Bar
                  dataKey="representationRate"
                  fill="#22c55e"
                  radius={[0, 4, 4, 0]}
                />
              </BarChart>
            </ResponsiveContainer>
          </div>
        </ChartCard>

        <ChartCard
          title="Lowest Representation Rates"
          subtitle="Bottom 10 nationalities by attorney representation"
        >
          <div className="h-80">
            <ResponsiveContainer width="100%" height="100%">
              <BarChart data={bottomRepNations} layout="vertical">
                <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                <XAxis
                  type="number"
                  tick={{ fill: '#64748b', fontSize: 11 }}
                  domain={[0, 60]}
                  tickFormatter={(value) => `${value}%`}
                />
                <YAxis
                  type="category"
                  dataKey="nationality"
                  tick={{ fill: '#64748b', fontSize: 11 }}
                  width={80}
                />
                <Tooltip
                  contentStyle={{
                    backgroundColor: 'white',
                    border: '1px solid #e2e8f0',
                    borderRadius: '8px',
                  }}
                  formatter={(value: number, name: string, props: { payload: typeof nationalityStats[0] }) => [
                    `${value}%`,
                    `Representation (Grant: ${props.payload.grantRate}%)`,
                  ]}
                />
                <Bar
                  dataKey="representationRate"
                  fill="#ef4444"
                  radius={[0, 4, 4, 0]}
                />
              </BarChart>
            </ResponsiveContainer>
          </div>
        </ChartCard>
      </div>

      {/* Detailed Comparison Table */}
      <ChartCard
        title="Representation vs Outcomes by Nationality"
        subtitle="Correlation between legal representation and asylum grant rates"
      >
        <div className="overflow-x-auto">
          <table className="w-full">
            <thead>
              <tr className="bg-slate-50 border-y border-slate-200">
                <th className="px-4 py-3 text-left text-sm font-semibold text-slate-600">
                  Nationality
                </th>
                <th className="px-4 py-3 text-right text-sm font-semibold text-slate-600">
                  Total Cases
                </th>
                <th className="px-4 py-3 text-right text-sm font-semibold text-slate-600">
                  Representation
                </th>
                <th className="px-4 py-3 text-right text-sm font-semibold text-slate-600">
                  Grant Rate
                </th>
                <th className="px-4 py-3 text-left text-sm font-semibold text-slate-600">
                  Comparison
                </th>
              </tr>
            </thead>
            <tbody>
              {nationalityStats.slice(0, 15).map((nat) => (
                <tr
                  key={nat.nationality}
                  className="border-b border-slate-100 hover:bg-slate-50"
                >
                  <td className="px-4 py-3 font-medium text-slate-800">
                    {nat.nationality}
                  </td>
                  <td className="px-4 py-3 text-right text-slate-600">
                    {formatNumber(nat.totalCases)}
                  </td>
                  <td className="px-4 py-3 text-right">
                    <div className="flex items-center justify-end gap-2">
                      <div className="w-20 h-2 bg-slate-200 rounded-full overflow-hidden">
                        <div
                          className={`h-full rounded-full ${
                            nat.representationRate >= 60
                              ? 'bg-green-500'
                              : nat.representationRate >= 40
                              ? 'bg-yellow-500'
                              : 'bg-red-500'
                          }`}
                          style={{ width: `${nat.representationRate}%` }}
                        />
                      </div>
                      <span className="text-sm w-12 text-right">
                        {nat.representationRate}%
                      </span>
                    </div>
                  </td>
                  <td className="px-4 py-3 text-right">
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
                  </td>
                  <td className="px-4 py-3">
                    <div className="flex items-center gap-1 text-sm">
                      {nat.representationRate >= avgRepresentationRate ? (
                        <span className="text-green-600">Above avg rep</span>
                      ) : (
                        <span className="text-red-600">Below avg rep</span>
                      )}
                    </div>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </ChartCard>
    </div>
  );
}
