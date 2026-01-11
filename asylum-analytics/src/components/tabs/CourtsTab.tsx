import { useState, useMemo } from 'react';
import {
  BarChart,
  Bar,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  ResponsiveContainer,
  ComposedChart,
  Line,
  Cell,
} from 'recharts';
import ChartCard from '../ui/ChartCard';
import DataTable from '../ui/DataTable';
import StatCard from '../ui/StatCard';
import { courtStats } from '../../data/asylumData';
import { Building2, Scale, Clock, AlertTriangle } from 'lucide-react';

const formatNumber = (num: number) => {
  if (num >= 1000000) return (num / 1000000).toFixed(2) + 'M';
  if (num >= 1000) return (num / 1000).toFixed(1) + 'K';
  return num.toString();
};

type SortField = 'totalCases' | 'grantRate' | 'backlog' | 'avgProcessingDays';

export default function CourtsTab() {
  const [sortBy, setSortBy] = useState<SortField>('totalCases');
  const [selectedState, setSelectedState] = useState<string>('all');

  const states = useMemo(
    () => ['all', ...new Set(courtStats.map((c) => c.state))].sort(),
    []
  );

  const filteredData = useMemo(() => {
    let data = [...courtStats];
    if (selectedState !== 'all') {
      data = data.filter((c) => c.state === selectedState);
    }
    return data.sort((a, b) => b[sortBy] - a[sortBy]);
  }, [sortBy, selectedState]);

  const topByVolume = [...courtStats]
    .sort((a, b) => b.totalCases - a.totalCases)
    .slice(0, 10);

  const topByBacklog = [...courtStats]
    .sort((a, b) => b.backlog - a.backlog)
    .slice(0, 10);

  const grantRateComparison = [...courtStats]
    .sort((a, b) => b.grantRate - a.grantRate)
    .map((court) => ({
      ...court,
      shortName: court.court.length > 12 ? court.court.slice(0, 12) + '...' : court.court,
    }));

  const totalCases = courtStats.reduce((sum, c) => sum + c.totalCases, 0);
  const totalBacklog = courtStats.reduce((sum, c) => sum + c.backlog, 0);
  const avgGrantRate =
    courtStats.reduce((sum, c) => sum + c.grantRate, 0) / courtStats.length;
  const avgProcessing =
    courtStats.reduce((sum, c) => sum + c.avgProcessingDays, 0) / courtStats.length;

  const getGrantRateColor = (rate: number) => {
    if (rate >= 50) return '#22c55e';
    if (rate >= 35) return '#84cc16';
    if (rate >= 25) return '#f59e0b';
    return '#ef4444';
  };

  const tableColumns = [
    {
      key: 'court',
      header: 'Immigration Court',
      render: (item: typeof courtStats[0]) => (
        <div>
          <span className="font-medium text-slate-800">{item.court}</span>
          <span className="text-slate-400 ml-2 text-sm">{item.state}</span>
        </div>
      ),
    },
    {
      key: 'totalCases',
      header: 'Total Cases',
      render: (item: typeof courtStats[0]) => formatNumber(item.totalCases),
      className: 'text-right',
    },
    {
      key: 'granted',
      header: 'Granted',
      render: (item: typeof courtStats[0]) => (
        <span className="text-green-600 font-medium">
          {formatNumber(item.granted)}
        </span>
      ),
      className: 'text-right',
    },
    {
      key: 'denied',
      header: 'Denied',
      render: (item: typeof courtStats[0]) => (
        <span className="text-red-600 font-medium">
          {formatNumber(item.denied)}
        </span>
      ),
      className: 'text-right',
    },
    {
      key: 'grantRate',
      header: 'Grant Rate',
      render: (item: typeof courtStats[0]) => (
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
      key: 'backlog',
      header: 'Backlog',
      render: (item: typeof courtStats[0]) => (
        <span className="text-orange-600 font-medium">
          {formatNumber(item.backlog)}
        </span>
      ),
      className: 'text-right',
    },
    {
      key: 'avgProcessingDays',
      header: 'Avg Wait',
      render: (item: typeof courtStats[0]) =>
        `${Math.round(item.avgProcessingDays / 365 * 10) / 10} yrs`,
      className: 'text-right',
    },
  ];

  return (
    <div className="space-y-6">
      <div>
        <h2 className="text-2xl font-bold text-slate-800 mb-2">
          Immigration Court Statistics
        </h2>
        <p className="text-slate-500">
          Analyze case volumes, grant rates, and backlogs across immigration courts
        </p>
      </div>

      {/* Summary Stats */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        <StatCard
          title="Total Immigration Courts"
          value={courtStats.length}
          icon={<Building2 className="w-5 h-5" />}
          color="blue"
        />
        <StatCard
          title="Average Grant Rate"
          value={`${avgGrantRate.toFixed(1)}%`}
          subtitle="Across all courts"
          icon={<Scale className="w-5 h-5" />}
          color="green"
        />
        <StatCard
          title="Total Backlog"
          value={formatNumber(totalBacklog)}
          subtitle="Pending cases"
          icon={<AlertTriangle className="w-5 h-5" />}
          color="orange"
        />
        <StatCard
          title="Avg Processing Time"
          value={`${(avgProcessing / 365).toFixed(1)} yrs`}
          subtitle="To decision"
          icon={<Clock className="w-5 h-5" />}
          color="purple"
        />
      </div>

      {/* Charts Row */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <ChartCard
          title="Courts by Case Volume"
          subtitle="Top 10 immigration courts by total cases"
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
                  formatter={(value: number) => [formatNumber(value), 'Cases']}
                />
                <Bar dataKey="totalCases" fill="#3b82f6" radius={[0, 4, 4, 0]} />
              </BarChart>
            </ResponsiveContainer>
          </div>
        </ChartCard>

        <ChartCard
          title="Courts by Backlog Size"
          subtitle="Top 10 courts with largest pending caseloads"
        >
          <div className="h-80">
            <ResponsiveContainer width="100%" height="100%">
              <BarChart data={topByBacklog} layout="vertical">
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
        </ChartCard>
      </div>

      {/* Grant Rate Comparison */}
      <ChartCard
        title="Grant Rate Comparison Across Courts"
        subtitle="Asylum grant rates vary significantly by location"
      >
        <div className="h-96">
          <ResponsiveContainer width="100%" height="100%">
            <ComposedChart data={grantRateComparison}>
              <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
              <XAxis
                dataKey="shortName"
                tick={{ fill: '#64748b', fontSize: 10 }}
                angle={-45}
                textAnchor="end"
                height={80}
              />
              <YAxis
                tick={{ fill: '#64748b', fontSize: 11 }}
                domain={[0, 70]}
                tickFormatter={(value) => `${value}%`}
              />
              <Tooltip
                contentStyle={{
                  backgroundColor: 'white',
                  border: '1px solid #e2e8f0',
                  borderRadius: '8px',
                }}
                formatter={(value: number, name: string) => [
                  `${value}%`,
                  name === 'grantRate' ? 'Grant Rate' : name,
                ]}
                labelFormatter={(label) => {
                  const court = grantRateComparison.find((c) => c.shortName === label);
                  return court ? `${court.court}, ${court.state}` : label;
                }}
              />
              <Bar dataKey="grantRate" name="Grant Rate">
                {grantRateComparison.map((entry, index) => (
                  <Cell key={index} fill={getGrantRateColor(entry.grantRate)} />
                ))}
              </Bar>
              <Line
                type="monotone"
                dataKey={() => avgGrantRate}
                stroke="#6366f1"
                strokeWidth={2}
                strokeDasharray="5 5"
                name="Average"
                dot={false}
              />
            </ComposedChart>
          </ResponsiveContainer>
        </div>
        <div className="mt-4 p-4 bg-purple-50 rounded-lg">
          <p className="text-sm text-purple-800">
            <strong>Disparity Alert:</strong> Grant rates range from{' '}
            {Math.min(...courtStats.map((c) => c.grantRate))}% to{' '}
            {Math.max(...courtStats.map((c) => c.grantRate))}% across courts. The
            same asylum case can have vastly different outcomes depending on court
            assignment.
          </p>
        </div>
      </ChartCard>

      {/* Full Data Table */}
      <ChartCard
        title="Complete Court Statistics"
        subtitle="All immigration courts with detailed metrics"
        actions={
          <div className="flex items-center gap-4">
            <div className="flex items-center gap-2">
              <span className="text-sm text-slate-500">State:</span>
              <select
                value={selectedState}
                onChange={(e) => setSelectedState(e.target.value)}
                className="px-3 py-1.5 border border-slate-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
              >
                <option value="all">All States</option>
                {states.slice(1).map((state) => (
                  <option key={state} value={state}>
                    {state}
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
                <option value="totalCases">Case Volume</option>
                <option value="grantRate">Grant Rate</option>
                <option value="backlog">Backlog</option>
                <option value="avgProcessingDays">Processing Time</option>
              </select>
            </div>
          </div>
        }
      >
        <DataTable
          data={filteredData}
          columns={tableColumns}
          searchPlaceholder="Search court..."
          pageSize={15}
        />
      </ChartCard>
    </div>
  );
}
