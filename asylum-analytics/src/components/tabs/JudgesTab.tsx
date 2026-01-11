import { useState, useMemo } from 'react';
import {
  BarChart,
  Bar,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  ResponsiveContainer,
  ScatterChart,
  Scatter,
  ZAxis,
  Cell,
  ReferenceLine,
} from 'recharts';
import ChartCard from '../ui/ChartCard';
import DataTable from '../ui/DataTable';
import StatCard from '../ui/StatCard';
import { judgeStats, courtStats } from '../../data/asylumData';
import { Scale, Users, TrendingUp, AlertCircle } from 'lucide-react';

const formatNumber = (num: number) => {
  if (num >= 1000000) return (num / 1000000).toFixed(2) + 'M';
  if (num >= 1000) return (num / 1000).toFixed(1) + 'K';
  return num.toString();
};

type SortField = 'totalDecisions' | 'grantRate' | 'avgProcessingDays';

export default function JudgesTab() {
  const [sortBy, setSortBy] = useState<SortField>('totalDecisions');
  const [selectedCourt, setSelectedCourt] = useState<string>('all');

  const courts = useMemo(
    () => ['all', ...new Set(judgeStats.map((j) => j.court))].sort(),
    []
  );

  const filteredData = useMemo(() => {
    let data = [...judgeStats];
    if (selectedCourt !== 'all') {
      data = data.filter((j) => j.court === selectedCourt);
    }
    return data.sort((a, b) => {
      if (sortBy === 'grantRate') return b.grantRate - a.grantRate;
      if (sortBy === 'avgProcessingDays') return a.avgProcessingDays - b.avgProcessingDays;
      return b.totalDecisions - a.totalDecisions;
    });
  }, [sortBy, selectedCourt]);

  const avgGrantRate =
    judgeStats.reduce((sum, j) => sum + j.grantRate, 0) / judgeStats.length;

  const highGrantRateJudges = judgeStats.filter((j) => j.grantRate >= 50).length;
  const lowGrantRateJudges = judgeStats.filter((j) => j.grantRate < 25).length;

  const grantRateDistribution = [
    { range: '0-20%', count: judgeStats.filter((j) => j.grantRate < 20).length, color: '#ef4444' },
    { range: '20-35%', count: judgeStats.filter((j) => j.grantRate >= 20 && j.grantRate < 35).length, color: '#f59e0b' },
    { range: '35-50%', count: judgeStats.filter((j) => j.grantRate >= 35 && j.grantRate < 50).length, color: '#84cc16' },
    { range: '50-65%', count: judgeStats.filter((j) => j.grantRate >= 50 && j.grantRate < 65).length, color: '#22c55e' },
    { range: '65%+', count: judgeStats.filter((j) => j.grantRate >= 65).length, color: '#10b981' },
  ];

  const scatterData = judgeStats.map((j) => ({
    x: j.totalDecisions,
    y: j.grantRate,
    name: j.judgeName,
    court: j.court,
  }));

  const topByVolume = [...judgeStats]
    .sort((a, b) => b.totalDecisions - a.totalDecisions)
    .slice(0, 10);

  const getGrantRateColor = (rate: number) => {
    if (rate >= 50) return '#22c55e';
    if (rate >= 35) return '#84cc16';
    if (rate >= 20) return '#f59e0b';
    return '#ef4444';
  };

  const tableColumns = [
    {
      key: 'judgeName',
      header: 'Judge',
      render: (item: typeof judgeStats[0]) => (
        <div>
          <span className="font-medium text-slate-800">{item.judgeName}</span>
          <p className="text-xs text-slate-400">{item.court}</p>
        </div>
      ),
    },
    {
      key: 'court',
      header: 'Court',
      render: (item: typeof judgeStats[0]) => item.court,
    },
    {
      key: 'totalDecisions',
      header: 'Decisions',
      render: (item: typeof judgeStats[0]) => formatNumber(item.totalDecisions),
      className: 'text-right',
    },
    {
      key: 'granted',
      header: 'Granted',
      render: (item: typeof judgeStats[0]) => (
        <span className="text-green-600 font-medium">
          {formatNumber(item.granted)}
        </span>
      ),
      className: 'text-right',
    },
    {
      key: 'denied',
      header: 'Denied',
      render: (item: typeof judgeStats[0]) => (
        <span className="text-red-600 font-medium">
          {formatNumber(item.denied)}
        </span>
      ),
      className: 'text-right',
    },
    {
      key: 'grantRate',
      header: 'Grant Rate',
      render: (item: typeof judgeStats[0]) => (
        <span
          className={`px-2 py-0.5 rounded text-sm font-medium ${
            item.grantRate >= 50
              ? 'bg-green-100 text-green-700'
              : item.grantRate >= 35
              ? 'bg-yellow-100 text-yellow-700'
              : 'bg-red-100 text-red-700'
          }`}
        >
          {item.grantRate}%
        </span>
      ),
      className: 'text-right',
    },
    {
      key: 'avgProcessingDays',
      header: 'Avg Processing',
      render: (item: typeof judgeStats[0]) =>
        `${Math.round(item.avgProcessingDays / 30)} months`,
      className: 'text-right',
    },
  ];

  return (
    <div className="space-y-6">
      <div>
        <h2 className="text-2xl font-bold text-slate-800 mb-2">
          Immigration Judge Statistics
        </h2>
        <p className="text-slate-500">
          Analyze decision patterns and grant rates across immigration judges
        </p>
      </div>

      {/* Summary Stats */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        <StatCard
          title="Judges Tracked"
          value={judgeStats.length}
          icon={<Scale className="w-5 h-5" />}
          color="blue"
        />
        <StatCard
          title="Average Grant Rate"
          value={`${avgGrantRate.toFixed(1)}%`}
          subtitle="Across all judges"
          icon={<TrendingUp className="w-5 h-5" />}
          color="green"
        />
        <StatCard
          title="High Grant Rate"
          value={highGrantRateJudges}
          subtitle="Judges with 50%+ grant rate"
          icon={<Users className="w-5 h-5" />}
          color="green"
        />
        <StatCard
          title="Low Grant Rate"
          value={lowGrantRateJudges}
          subtitle="Judges with <25% grant rate"
          icon={<AlertCircle className="w-5 h-5" />}
          color="red"
        />
      </div>

      {/* Grant Rate Distribution */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <ChartCard
          title="Grant Rate Distribution"
          subtitle="Number of judges by grant rate range"
        >
          <div className="h-80">
            <ResponsiveContainer width="100%" height="100%">
              <BarChart data={grantRateDistribution}>
                <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                <XAxis
                  dataKey="range"
                  tick={{ fill: '#64748b', fontSize: 12 }}
                />
                <YAxis tick={{ fill: '#64748b', fontSize: 12 }} />
                <Tooltip
                  contentStyle={{
                    backgroundColor: 'white',
                    border: '1px solid #e2e8f0',
                    borderRadius: '8px',
                  }}
                  formatter={(value: number) => [`${value} judges`, 'Count']}
                />
                <Bar dataKey="count" radius={[4, 4, 0, 0]}>
                  {grantRateDistribution.map((entry, index) => (
                    <Cell key={index} fill={entry.color} />
                  ))}
                </Bar>
              </BarChart>
            </ResponsiveContainer>
          </div>
        </ChartCard>

        <ChartCard
          title="Top 10 by Decision Volume"
          subtitle="Judges with most asylum decisions"
        >
          <div className="h-80">
            <ResponsiveContainer width="100%" height="100%">
              <BarChart data={topByVolume} layout="vertical">
                <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
                <XAxis
                  type="number"
                  tick={{ fill: '#64748b', fontSize: 11 }}
                  tickFormatter={(value) => formatNumber(value)}
                />
                <YAxis
                  type="category"
                  dataKey="judgeName"
                  tick={{ fill: '#64748b', fontSize: 10 }}
                  width={120}
                />
                <Tooltip
                  contentStyle={{
                    backgroundColor: 'white',
                    border: '1px solid #e2e8f0',
                    borderRadius: '8px',
                  }}
                  formatter={(value: number, name: string, props: { payload: typeof judgeStats[0] }) => [
                    formatNumber(value),
                    `Decisions (${props.payload.grantRate}% grant rate)`,
                  ]}
                />
                <Bar dataKey="totalDecisions" radius={[0, 4, 4, 0]}>
                  {topByVolume.map((entry, index) => (
                    <Cell key={index} fill={getGrantRateColor(entry.grantRate)} />
                  ))}
                </Bar>
              </BarChart>
            </ResponsiveContainer>
          </div>
        </ChartCard>
      </div>

      {/* Scatter: Volume vs Grant Rate */}
      <ChartCard
        title="Decision Volume vs Grant Rate"
        subtitle="Each point represents one immigration judge"
      >
        <div className="h-96">
          <ResponsiveContainer width="100%" height="100%">
            <ScatterChart margin={{ top: 20, right: 20, bottom: 20, left: 20 }}>
              <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
              <XAxis
                type="number"
                dataKey="x"
                name="Decisions"
                tick={{ fill: '#64748b', fontSize: 11 }}
                label={{
                  value: 'Total Decisions',
                  position: 'bottom',
                  fill: '#64748b',
                }}
              />
              <YAxis
                type="number"
                dataKey="y"
                name="Grant Rate"
                tick={{ fill: '#64748b', fontSize: 11 }}
                label={{
                  value: 'Grant Rate (%)',
                  angle: -90,
                  position: 'insideLeft',
                  fill: '#64748b',
                }}
                domain={[0, 80]}
              />
              <ReferenceLine y={avgGrantRate} stroke="#6366f1" strokeDasharray="5 5" />
              <Tooltip
                contentStyle={{
                  backgroundColor: 'white',
                  border: '1px solid #e2e8f0',
                  borderRadius: '8px',
                }}
                content={({ active, payload }) => {
                  if (active && payload && payload.length) {
                    const data = payload[0].payload;
                    return (
                      <div className="bg-white p-3 rounded-lg shadow-lg border border-slate-200">
                        <p className="font-semibold text-slate-800">{data.name}</p>
                        <p className="text-xs text-slate-500">{data.court}</p>
                        <p className="text-sm text-slate-600 mt-1">
                          Decisions: {formatNumber(data.x)}
                        </p>
                        <p className="text-sm text-slate-600">
                          Grant Rate: {data.y}%
                        </p>
                      </div>
                    );
                  }
                  return null;
                }}
              />
              <Scatter data={scatterData} fill="#3b82f6" fillOpacity={0.7}>
                {scatterData.map((entry, index) => (
                  <Cell key={index} fill={getGrantRateColor(entry.y)} />
                ))}
              </Scatter>
            </ScatterChart>
          </ResponsiveContainer>
        </div>
        <div className="mt-4 p-4 bg-purple-50 rounded-lg">
          <p className="text-sm text-purple-800">
            <strong>Note:</strong> The dashed line represents the average grant rate
            ({avgGrantRate.toFixed(1)}%). Grant rates range from{' '}
            {Math.min(...judgeStats.map((j) => j.grantRate))}% to{' '}
            {Math.max(...judgeStats.map((j) => j.grantRate))}%, showing significant
            variation in outcomes based on assigned judge.
          </p>
        </div>
      </ChartCard>

      {/* Full Data Table */}
      <ChartCard
        title="Complete Judge Statistics"
        subtitle="All tracked immigration judges with detailed metrics"
        actions={
          <div className="flex items-center gap-4">
            <div className="flex items-center gap-2">
              <span className="text-sm text-slate-500">Court:</span>
              <select
                value={selectedCourt}
                onChange={(e) => setSelectedCourt(e.target.value)}
                className="px-3 py-1.5 border border-slate-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
              >
                <option value="all">All Courts</option>
                {courts.slice(1).map((court) => (
                  <option key={court} value={court}>
                    {court}
                  </option>
                ))}
              </select>
            </div>
            <div className="flex items-center gap-2">
              <span className="text-sm text-slate-500">Sort by:</span>
              <select
                value={sortBy}
                onChange={(e) => setSortBy(e.target.value as SortField)}
                className="px-3 py-1.5 border border-slate-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
              >
                <option value="totalDecisions">Decision Volume</option>
                <option value="grantRate">Grant Rate</option>
                <option value="avgProcessingDays">Processing Time</option>
              </select>
            </div>
          </div>
        }
      >
        <DataTable
          data={filteredData}
          columns={tableColumns}
          searchPlaceholder="Search judge or court..."
          pageSize={15}
        />
      </ChartCard>
    </div>
  );
}
