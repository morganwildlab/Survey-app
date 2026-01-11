import { useState } from 'react';
import {
  LineChart,
  Line,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  Legend,
  ResponsiveContainer,
  BarChart,
  Bar,
  ComposedChart,
  Area,
} from 'recharts';
import ChartCard from '../ui/ChartCard';
import { yearlyTrends, processingTimeTrends } from '../../data/asylumData';

const formatNumber = (num: number) => {
  if (num >= 1000000) return (num / 1000000).toFixed(2) + 'M';
  if (num >= 1000) return (num / 1000).toFixed(1) + 'K';
  return num.toString();
};

type MetricType = 'all' | 'applications' | 'outcomes' | 'grantRate';

export default function TrendsTab() {
  const [selectedMetric, setSelectedMetric] = useState<MetricType>('all');

  const combinedData = yearlyTrends.map((trend) => ({
    ...trend,
    denialRate: 100 - trend.grantRate,
  }));

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-2xl font-bold text-slate-800 mb-2">
            Yearly Trends Analysis
          </h2>
          <p className="text-slate-500">
            Track asylum application patterns and outcomes over time (FY2015-FY2024)
          </p>
        </div>

        <div className="flex items-center gap-2">
          {(['all', 'applications', 'outcomes', 'grantRate'] as MetricType[]).map(
            (metric) => (
              <button
                key={metric}
                onClick={() => setSelectedMetric(metric)}
                className={`px-4 py-2 rounded-lg text-sm font-medium transition-colors ${
                  selectedMetric === metric
                    ? 'bg-blue-600 text-white'
                    : 'bg-slate-100 text-slate-600 hover:bg-slate-200'
                }`}
              >
                {metric === 'all'
                  ? 'All Metrics'
                  : metric === 'applications'
                  ? 'Applications'
                  : metric === 'outcomes'
                  ? 'Outcomes'
                  : 'Grant Rate'}
              </button>
            )
          )}
        </div>
      </div>

      {/* Main Trends Chart */}
      {(selectedMetric === 'all' || selectedMetric === 'applications') && (
        <ChartCard
          title="Asylum Applications Over Time"
          subtitle="New asylum applications filed each fiscal year"
        >
          <div className="h-96">
            <ResponsiveContainer width="100%" height="100%">
              <ComposedChart data={yearlyTrends}>
                <defs>
                  <linearGradient id="colorApps" x1="0" y1="0" x2="0" y2="1">
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
                  }}
                  formatter={(value: number) => [formatNumber(value), 'Applications']}
                />
                <Area
                  type="monotone"
                  dataKey="applications"
                  stroke="#3b82f6"
                  strokeWidth={2}
                  fill="url(#colorApps)"
                />
                <Line
                  type="monotone"
                  dataKey="applications"
                  stroke="#3b82f6"
                  strokeWidth={3}
                  dot={{ fill: '#3b82f6', r: 5 }}
                />
              </ComposedChart>
            </ResponsiveContainer>
          </div>
          <div className="mt-4 p-4 bg-blue-50 rounded-lg">
            <p className="text-sm text-blue-800">
              <strong>Key Insight:</strong> Asylum applications have increased by{' '}
              {Math.round(
                ((yearlyTrends[yearlyTrends.length - 1].applications -
                  yearlyTrends[0].applications) /
                  yearlyTrends[0].applications) *
                  100
              )}
              % from FY2015 to FY2024. Notable dip in FY2020 due to COVID-19 pandemic
              restrictions.
            </p>
          </div>
        </ChartCard>
      )}

      {/* Outcomes Comparison */}
      {(selectedMetric === 'all' || selectedMetric === 'outcomes') && (
        <ChartCard
          title="Case Outcomes by Year"
          subtitle="Granted vs Denied asylum cases"
        >
          <div className="h-96">
            <ResponsiveContainer width="100%" height="100%">
              <BarChart data={yearlyTrends}>
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
                  }}
                  formatter={(value: number) => formatNumber(value)}
                />
                <Legend />
                <Bar dataKey="granted" name="Granted" fill="#22c55e" radius={[4, 4, 0, 0]} />
                <Bar dataKey="denied" name="Denied" fill="#ef4444" radius={[4, 4, 0, 0]} />
              </BarChart>
            </ResponsiveContainer>
          </div>
        </ChartCard>
      )}

      {/* Grant Rate Trend */}
      {(selectedMetric === 'all' || selectedMetric === 'grantRate') && (
        <ChartCard
          title="Grant Rate Trend"
          subtitle="Percentage of cases granted asylum each year"
        >
          <div className="h-80">
            <ResponsiveContainer width="100%" height="100%">
              <LineChart data={combinedData}>
                <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                <XAxis
                  dataKey="year"
                  tick={{ fill: '#64748b', fontSize: 12 }}
                  axisLine={{ stroke: '#e2e8f0' }}
                />
                <YAxis
                  tick={{ fill: '#64748b', fontSize: 12 }}
                  axisLine={{ stroke: '#e2e8f0' }}
                  domain={[0, 100]}
                  tickFormatter={(value) => `${value}%`}
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
                <Line
                  type="monotone"
                  dataKey="grantRate"
                  name="Grant Rate"
                  stroke="#22c55e"
                  strokeWidth={3}
                  dot={{ fill: '#22c55e', r: 5 }}
                />
                <Line
                  type="monotone"
                  dataKey="denialRate"
                  name="Denial Rate"
                  stroke="#ef4444"
                  strokeWidth={3}
                  dot={{ fill: '#ef4444', r: 5 }}
                />
              </LineChart>
            </ResponsiveContainer>
          </div>
        </ChartCard>
      )}

      {/* Backlog Growth */}
      <ChartCard
        title="Pending Case Backlog Growth"
        subtitle="Cumulative pending cases awaiting decisions"
      >
        <div className="h-80">
          <ResponsiveContainer width="100%" height="100%">
            <ComposedChart data={yearlyTrends}>
              <defs>
                <linearGradient id="colorPending" x1="0" y1="0" x2="0" y2="1">
                  <stop offset="5%" stopColor="#f59e0b" stopOpacity={0.3} />
                  <stop offset="95%" stopColor="#f59e0b" stopOpacity={0} />
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
                }}
                formatter={(value: number) => [formatNumber(value), 'Pending Cases']}
              />
              <Area
                type="monotone"
                dataKey="pending"
                stroke="#f59e0b"
                fill="url(#colorPending)"
              />
              <Line
                type="monotone"
                dataKey="pending"
                stroke="#f59e0b"
                strokeWidth={3}
                dot={{ fill: '#f59e0b', r: 5 }}
              />
            </ComposedChart>
          </ResponsiveContainer>
        </div>
        <div className="mt-4 p-4 bg-orange-50 rounded-lg">
          <p className="text-sm text-orange-800">
            <strong>Critical:</strong> The case backlog has grown from{' '}
            {formatNumber(yearlyTrends[0].pending)} in FY2015 to{' '}
            {formatNumber(yearlyTrends[yearlyTrends.length - 1].pending)} in FY2024,
            an increase of over{' '}
            {Math.round(
              (yearlyTrends[yearlyTrends.length - 1].pending / yearlyTrends[0].pending - 1) *
                100
            )}
            %.
          </p>
        </div>
      </ChartCard>

      {/* Processing Time Trends */}
      <ChartCard
        title="Average Processing Time"
        subtitle="Days to decision by case type"
      >
        <div className="h-80">
          <ResponsiveContainer width="100%" height="100%">
            <LineChart data={processingTimeTrends}>
              <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
              <XAxis
                dataKey="year"
                tick={{ fill: '#64748b', fontSize: 12 }}
                axisLine={{ stroke: '#e2e8f0' }}
              />
              <YAxis
                tick={{ fill: '#64748b', fontSize: 12 }}
                axisLine={{ stroke: '#e2e8f0' }}
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
              <Line
                type="monotone"
                dataKey="affirmative"
                name="Affirmative Cases"
                stroke="#8b5cf6"
                strokeWidth={2}
                dot={{ fill: '#8b5cf6', r: 4 }}
              />
              <Line
                type="monotone"
                dataKey="defensive"
                name="Defensive Cases"
                stroke="#ec4899"
                strokeWidth={2}
                dot={{ fill: '#ec4899', r: 4 }}
              />
            </LineChart>
          </ResponsiveContainer>
        </div>
      </ChartCard>

      {/* Year-over-Year Summary Table */}
      <ChartCard
        title="Year-over-Year Summary"
        subtitle="Detailed breakdown by fiscal year"
      >
        <div className="overflow-x-auto">
          <table className="w-full">
            <thead>
              <tr className="bg-slate-50 border-y border-slate-200">
                <th className="px-4 py-3 text-left text-sm font-semibold text-slate-600">
                  Fiscal Year
                </th>
                <th className="px-4 py-3 text-right text-sm font-semibold text-slate-600">
                  Applications
                </th>
                <th className="px-4 py-3 text-right text-sm font-semibold text-slate-600">
                  Granted
                </th>
                <th className="px-4 py-3 text-right text-sm font-semibold text-slate-600">
                  Denied
                </th>
                <th className="px-4 py-3 text-right text-sm font-semibold text-slate-600">
                  Grant Rate
                </th>
                <th className="px-4 py-3 text-right text-sm font-semibold text-slate-600">
                  Pending
                </th>
                <th className="px-4 py-3 text-right text-sm font-semibold text-slate-600">
                  YoY Change
                </th>
              </tr>
            </thead>
            <tbody>
              {yearlyTrends.map((year, index) => {
                const prevYear = index > 0 ? yearlyTrends[index - 1] : null;
                const yoyChange = prevYear
                  ? ((year.applications - prevYear.applications) /
                      prevYear.applications) *
                    100
                  : 0;

                return (
                  <tr
                    key={year.year}
                    className="border-b border-slate-100 hover:bg-slate-50"
                  >
                    <td className="px-4 py-3 font-medium text-slate-800">
                      FY{year.year}
                    </td>
                    <td className="px-4 py-3 text-right text-slate-600">
                      {formatNumber(year.applications)}
                    </td>
                    <td className="px-4 py-3 text-right text-green-600 font-medium">
                      {formatNumber(year.granted)}
                    </td>
                    <td className="px-4 py-3 text-right text-red-600 font-medium">
                      {formatNumber(year.denied)}
                    </td>
                    <td className="px-4 py-3 text-right">
                      <span
                        className={`px-2 py-0.5 rounded text-sm font-medium ${
                          year.grantRate >= 42
                            ? 'bg-green-100 text-green-700'
                            : year.grantRate >= 38
                            ? 'bg-yellow-100 text-yellow-700'
                            : 'bg-red-100 text-red-700'
                        }`}
                      >
                        {year.grantRate}%
                      </span>
                    </td>
                    <td className="px-4 py-3 text-right text-slate-600">
                      {formatNumber(year.pending)}
                    </td>
                    <td className="px-4 py-3 text-right">
                      {index > 0 && (
                        <span
                          className={`text-sm font-medium ${
                            yoyChange > 0
                              ? 'text-green-600'
                              : yoyChange < 0
                              ? 'text-red-600'
                              : 'text-slate-500'
                          }`}
                        >
                          {yoyChange > 0 ? '+' : ''}
                          {yoyChange.toFixed(1)}%
                        </span>
                      )}
                    </td>
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
      </ChartCard>
    </div>
  );
}
