import { useState, useMemo } from 'react';
import {
  AreaChart,
  Area,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  ResponsiveContainer,
  LineChart,
  Line,
  Legend,
  BarChart,
  Bar,
  ComposedChart,
} from 'recharts';
import ChartCard from '../ui/ChartCard';
import StatCard from '../ui/StatCard';
import { backlogData, courtStats, yearlyTrends } from '../../data/asylumData';
import { Clock, TrendingUp, AlertTriangle, Calendar, Building2 } from 'lucide-react';

const formatNumber = (num: number) => {
  if (num >= 1000000) return (num / 1000000).toFixed(2) + 'M';
  if (num >= 1000) return (num / 1000).toFixed(1) + 'K';
  return num.toString();
};

const monthNames = [
  'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'
];

export default function BacklogTab() {
  const [selectedYear, setSelectedYear] = useState<number>(2024);

  const years = useMemo(() => {
    return [...new Set(backlogData.map((d) => d.year))];
  }, []);

  const filteredMonthlyData = useMemo(() => {
    return backlogData
      .filter((d) => d.year === selectedYear)
      .map((d) => ({
        ...d,
        monthName: monthNames[d.month - 1],
        label: `${monthNames[d.month - 1]} ${d.year}`,
      }));
  }, [selectedYear]);

  const yearlyBacklogData = useMemo(() => {
    return years.map((year) => {
      const yearData = backlogData.filter((d) => d.year === year);
      const lastMonth = yearData[yearData.length - 1];
      const totalFilings = yearData.reduce((sum, d) => sum + d.newFilings, 0);
      const totalCompletions = yearData.reduce((sum, d) => sum + d.completions, 0);
      return {
        year,
        pendingCases: lastMonth?.pendingCases || 0,
        totalFilings,
        totalCompletions,
        netChange: totalFilings - totalCompletions,
      };
    });
  }, [years]);

  const latestBacklog = backlogData[backlogData.length - 1];
  const previousYearBacklog = backlogData.find(
    (d) => d.year === latestBacklog.year - 1 && d.month === latestBacklog.month
  );

  const backlogGrowthRate = previousYearBacklog
    ? ((latestBacklog.pendingCases - previousYearBacklog.pendingCases) /
        previousYearBacklog.pendingCases) *
      100
    : 0;

  const avgMonthlyFilings =
    filteredMonthlyData.reduce((sum, d) => sum + d.newFilings, 0) /
    filteredMonthlyData.length;
  const avgMonthlyCompletions =
    filteredMonthlyData.reduce((sum, d) => sum + d.completions, 0) /
    filteredMonthlyData.length;

  // Calculate years to clear backlog at current rate
  const monthlyNetChange = avgMonthlyFilings - avgMonthlyCompletions;
  const yearsToClear =
    monthlyNetChange < 0
      ? (latestBacklog.pendingCases / Math.abs(monthlyNetChange)) / 12
      : Infinity;

  // Top courts by backlog
  const topBacklogCourts = [...courtStats]
    .sort((a, b) => b.backlog - a.backlog)
    .slice(0, 10);

  const totalBacklog = courtStats.reduce((sum, c) => sum + c.backlog, 0);

  return (
    <div className="space-y-6">
      <div>
        <h2 className="text-2xl font-bold text-slate-800 mb-2">
          Case Backlog Analysis
        </h2>
        <p className="text-slate-500">
          Monitor the immigration court backlog growth and capacity challenges
        </p>
      </div>

      {/* Summary Stats */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        <StatCard
          title="Current Backlog"
          value={formatNumber(latestBacklog.pendingCases)}
          subtitle="Pending asylum cases"
          icon={<Clock className="w-5 h-5" />}
          color="orange"
        />
        <StatCard
          title="YoY Growth"
          value={`+${backlogGrowthRate.toFixed(1)}%`}
          subtitle="Compared to last year"
          icon={<TrendingUp className="w-5 h-5" />}
          trend={{ value: Math.round(backlogGrowthRate), label: 'increase' }}
          color="red"
        />
        <StatCard
          title="Monthly Net Change"
          value={`+${formatNumber(Math.round(monthlyNetChange))}`}
          subtitle={`FY${selectedYear} average`}
          icon={<Calendar className="w-5 h-5" />}
          color="purple"
        />
        <StatCard
          title="Est. Wait Time"
          value={`${(latestBacklog.pendingCases / (avgMonthlyCompletions * 12)).toFixed(1)} yrs`}
          subtitle="At current completion rate"
          icon={<AlertTriangle className="w-5 h-5" />}
          color="red"
        />
      </div>

      {/* Backlog Growth Chart */}
      <ChartCard
        title="Backlog Growth Over Time"
        subtitle="Total pending asylum cases (FY2022-FY2024)"
      >
        <div className="h-80">
          <ResponsiveContainer width="100%" height="100%">
            <AreaChart data={backlogData.map((d) => ({
              ...d,
              label: `${monthNames[d.month - 1]} ${d.year}`,
            }))}>
              <defs>
                <linearGradient id="colorBacklog" x1="0" y1="0" x2="0" y2="1">
                  <stop offset="5%" stopColor="#f59e0b" stopOpacity={0.3} />
                  <stop offset="95%" stopColor="#f59e0b" stopOpacity={0} />
                </linearGradient>
              </defs>
              <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
              <XAxis
                dataKey="label"
                tick={{ fill: '#64748b', fontSize: 10 }}
                interval={5}
                angle={-45}
                textAnchor="end"
                height={60}
              />
              <YAxis
                tick={{ fill: '#64748b', fontSize: 11 }}
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
                dataKey="pendingCases"
                stroke="#f59e0b"
                strokeWidth={2}
                fill="url(#colorBacklog)"
              />
            </AreaChart>
          </ResponsiveContainer>
        </div>
        <div className="mt-4 p-4 bg-red-50 border border-red-200 rounded-lg">
          <p className="text-sm text-red-800">
            <strong>Crisis Level:</strong> The asylum backlog has grown from{' '}
            {formatNumber(backlogData[0].pendingCases)} in January 2022 to{' '}
            {formatNumber(latestBacklog.pendingCases)} today, an increase of{' '}
            {Math.round(
              ((latestBacklog.pendingCases - backlogData[0].pendingCases) /
                backlogData[0].pendingCases) *
                100
            )}
            %. At the current rate, it would take over{' '}
            {yearsToClear === Infinity
              ? 'infinite time (backlog is growing)'
              : `${Math.round(yearsToClear)} years`}{' '}
            to clear the backlog.
          </p>
        </div>
      </ChartCard>

      {/* Monthly Flow Analysis */}
      <ChartCard
        title="Monthly Case Flow"
        subtitle="New filings vs completions"
        actions={
          <select
            value={selectedYear}
            onChange={(e) => setSelectedYear(Number(e.target.value))}
            className="px-3 py-1.5 border border-slate-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
          >
            {years.map((year) => (
              <option key={year} value={year}>
                FY{year}
              </option>
            ))}
          </select>
        }
      >
        <div className="h-80">
          <ResponsiveContainer width="100%" height="100%">
            <ComposedChart data={filteredMonthlyData}>
              <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
              <XAxis
                dataKey="monthName"
                tick={{ fill: '#64748b', fontSize: 12 }}
              />
              <YAxis
                tick={{ fill: '#64748b', fontSize: 11 }}
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
              <Bar
                dataKey="newFilings"
                name="New Filings"
                fill="#ef4444"
                radius={[4, 4, 0, 0]}
              />
              <Bar
                dataKey="completions"
                name="Completions"
                fill="#22c55e"
                radius={[4, 4, 0, 0]}
              />
              <Line
                type="monotone"
                dataKey="netChange"
                name="Net Change"
                stroke="#6366f1"
                strokeWidth={2}
                dot={{ fill: '#6366f1', r: 4 }}
              />
            </ComposedChart>
          </ResponsiveContainer>
        </div>
        <div className="mt-4 grid grid-cols-3 gap-4">
          <div className="p-3 bg-red-50 rounded-lg text-center">
            <p className="text-sm text-red-600">Avg Monthly Filings</p>
            <p className="text-xl font-bold text-red-700">
              {formatNumber(Math.round(avgMonthlyFilings))}
            </p>
          </div>
          <div className="p-3 bg-green-50 rounded-lg text-center">
            <p className="text-sm text-green-600">Avg Monthly Completions</p>
            <p className="text-xl font-bold text-green-700">
              {formatNumber(Math.round(avgMonthlyCompletions))}
            </p>
          </div>
          <div className="p-3 bg-purple-50 rounded-lg text-center">
            <p className="text-sm text-purple-600">Avg Net Change</p>
            <p className="text-xl font-bold text-purple-700">
              +{formatNumber(Math.round(monthlyNetChange))}
            </p>
          </div>
        </div>
      </ChartCard>

      {/* Yearly Trends */}
      <ChartCard
        title="Annual Backlog Trends"
        subtitle="Year-end pending cases and annual flow"
      >
        <div className="h-80">
          <ResponsiveContainer width="100%" height="100%">
            <BarChart data={yearlyBacklogData}>
              <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
              <XAxis
                dataKey="year"
                tick={{ fill: '#64748b', fontSize: 12 }}
                tickFormatter={(value) => `FY${value}`}
              />
              <YAxis
                tick={{ fill: '#64748b', fontSize: 11 }}
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
              <Bar
                dataKey="totalFilings"
                name="Total Filings"
                fill="#ef4444"
                radius={[4, 4, 0, 0]}
              />
              <Bar
                dataKey="totalCompletions"
                name="Total Completions"
                fill="#22c55e"
                radius={[4, 4, 0, 0]}
              />
            </BarChart>
          </ResponsiveContainer>
        </div>
      </ChartCard>

      {/* Backlog by Court */}
      <ChartCard
        title="Backlog Distribution by Court"
        subtitle="Immigration courts with largest pending caseloads"
      >
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          <div className="h-80">
            <ResponsiveContainer width="100%" height="100%">
              <BarChart data={topBacklogCourts} layout="vertical">
                <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                <XAxis
                  type="number"
                  tick={{ fill: '#64748b', fontSize: 11 }}
                  tickFormatter={(value) => formatNumber(value)}
                />
                <YAxis
                  type="category"
                  dataKey="court"
                  tick={{ fill: '#64748b', fontSize: 11 }}
                  width={100}
                />
                <Tooltip
                  contentStyle={{
                    backgroundColor: 'white',
                    border: '1px solid #e2e8f0',
                    borderRadius: '8px',
                  }}
                  formatter={(value: number) => [formatNumber(value), 'Pending Cases']}
                />
                <Bar dataKey="backlog" fill="#f59e0b" radius={[0, 4, 4, 0]} />
              </BarChart>
            </ResponsiveContainer>
          </div>

          <div className="space-y-3">
            <h4 className="font-semibold text-slate-800">
              Top Courts by Backlog Share
            </h4>
            {topBacklogCourts.slice(0, 6).map((court) => {
              const share = (court.backlog / totalBacklog) * 100;
              return (
                <div key={court.court} className="flex items-center gap-3">
                  <div className="flex-1">
                    <div className="flex items-center justify-between mb-1">
                      <span className="text-sm font-medium text-slate-700">
                        {court.court}
                      </span>
                      <span className="text-sm text-slate-500">
                        {formatNumber(court.backlog)} ({share.toFixed(1)}%)
                      </span>
                    </div>
                    <div className="w-full h-2 bg-slate-200 rounded-full overflow-hidden">
                      <div
                        className="h-full bg-orange-500 rounded-full"
                        style={{ width: `${share * 3}%` }}
                      />
                    </div>
                  </div>
                </div>
              );
            })}

            <div className="mt-4 p-4 bg-orange-50 rounded-lg">
              <div className="flex items-center gap-2 mb-2">
                <Building2 className="w-5 h-5 text-orange-600" />
                <span className="font-semibold text-orange-800">
                  Concentration Analysis
                </span>
              </div>
              <p className="text-sm text-orange-700">
                The top 5 courts account for{' '}
                {(
                  (topBacklogCourts.slice(0, 5).reduce((s, c) => s + c.backlog, 0) /
                    totalBacklog) *
                  100
                ).toFixed(1)}
                % of the total backlog. New York City alone holds{' '}
                {((topBacklogCourts[0].backlog / totalBacklog) * 100).toFixed(1)}%
                of all pending cases.
              </p>
            </div>
          </div>
        </div>
      </ChartCard>

      {/* Historical Comparison */}
      <ChartCard
        title="Backlog vs Historical Trends"
        subtitle="Comparing current backlog to historical norms"
      >
        <div className="h-80">
          <ResponsiveContainer width="100%" height="100%">
            <LineChart data={yearlyTrends}>
              <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
              <XAxis
                dataKey="year"
                tick={{ fill: '#64748b', fontSize: 12 }}
                tickFormatter={(value) => `FY${value}`}
              />
              <YAxis
                tick={{ fill: '#64748b', fontSize: 11 }}
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
              <Line
                type="monotone"
                dataKey="applications"
                name="New Applications"
                stroke="#3b82f6"
                strokeWidth={2}
                dot={{ fill: '#3b82f6', r: 4 }}
              />
              <Line
                type="monotone"
                dataKey="pending"
                name="Total Pending"
                stroke="#f59e0b"
                strokeWidth={2}
                dot={{ fill: '#f59e0b', r: 4 }}
              />
            </LineChart>
          </ResponsiveContainer>
        </div>
        <div className="mt-4 p-4 bg-blue-50 rounded-lg">
          <p className="text-sm text-blue-800">
            <strong>Historical Context:</strong> The backlog has grown{' '}
            {Math.round(
              (yearlyTrends[yearlyTrends.length - 1].pending /
                yearlyTrends[0].pending) *
                100 -
                100
            )}
            % since FY2015, while annual applications have increased by{' '}
            {Math.round(
              (yearlyTrends[yearlyTrends.length - 1].applications /
                yearlyTrends[0].applications) *
                100 -
                100
            )}
            %. This gap reflects insufficient capacity to handle the growing caseload.
          </p>
        </div>
      </ChartCard>
    </div>
  );
}
