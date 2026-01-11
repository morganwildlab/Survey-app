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
} from 'recharts';
import { Search } from 'lucide-react';
import ChartCard from '../ui/ChartCard';
import DataTable from '../ui/DataTable';
import { nationalityStats } from '../../data/asylumData';

const formatNumber = (num: number) => {
  if (num >= 1000000) return (num / 1000000).toFixed(2) + 'M';
  if (num >= 1000) return (num / 1000).toFixed(1) + 'K';
  return num.toString();
};

type SortField = 'totalCases' | 'grantRate' | 'representationRate';

export default function NationalityTab() {
  const [sortBy, setSortBy] = useState<SortField>('totalCases');
  const [searchTerm, setSearchTerm] = useState('');

  const sortedData = useMemo(() => {
    let data = [...nationalityStats];

    if (searchTerm) {
      data = data.filter((item) =>
        item.nationality.toLowerCase().includes(searchTerm.toLowerCase())
      );
    }

    return data.sort((a, b) => b[sortBy] - a[sortBy]);
  }, [sortBy, searchTerm]);

  const topByVolume = [...nationalityStats]
    .sort((a, b) => b.totalCases - a.totalCases)
    .slice(0, 10);

  const topByGrantRate = [...nationalityStats]
    .sort((a, b) => b.grantRate - a.grantRate)
    .slice(0, 10);

  const scatterData = nationalityStats.map((nat) => ({
    x: nat.representationRate,
    y: nat.grantRate,
    z: nat.totalCases,
    name: nat.nationality,
  }));

  const getGrantRateColor = (rate: number) => {
    if (rate >= 60) return '#22c55e';
    if (rate >= 40) return '#84cc16';
    if (rate >= 25) return '#f59e0b';
    return '#ef4444';
  };

  const tableColumns = [
    {
      key: 'nationality',
      header: 'Nationality',
      render: (item: typeof nationalityStats[0]) => (
        <span className="font-medium text-slate-800">{item.nationality}</span>
      ),
    },
    {
      key: 'totalCases',
      header: 'Total Cases',
      render: (item: typeof nationalityStats[0]) => formatNumber(item.totalCases),
      className: 'text-right',
    },
    {
      key: 'granted',
      header: 'Granted',
      render: (item: typeof nationalityStats[0]) => (
        <span className="text-green-600 font-medium">
          {formatNumber(item.granted)}
        </span>
      ),
      className: 'text-right',
    },
    {
      key: 'denied',
      header: 'Denied',
      render: (item: typeof nationalityStats[0]) => (
        <span className="text-red-600 font-medium">
          {formatNumber(item.denied)}
        </span>
      ),
      className: 'text-right',
    },
    {
      key: 'grantRate',
      header: 'Grant Rate',
      render: (item: typeof nationalityStats[0]) => (
        <span
          className={`px-2 py-0.5 rounded text-sm font-medium ${
            item.grantRate >= 50
              ? 'bg-green-100 text-green-700'
              : item.grantRate >= 30
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
      render: (item: typeof nationalityStats[0]) =>
        `${Math.round(item.avgProcessingDays / 30)} months`,
      className: 'text-right',
    },
    {
      key: 'representationRate',
      header: 'Representation',
      render: (item: typeof nationalityStats[0]) => (
        <div className="flex items-center gap-2 justify-end">
          <div className="w-16 h-2 bg-slate-200 rounded-full overflow-hidden">
            <div
              className="h-full bg-blue-500 rounded-full"
              style={{ width: `${item.representationRate}%` }}
            />
          </div>
          <span className="text-sm text-slate-600 w-12 text-right">
            {item.representationRate}%
          </span>
        </div>
      ),
      className: 'text-right',
    },
  ];

  return (
    <div className="space-y-6">
      <div>
        <h2 className="text-2xl font-bold text-slate-800 mb-2">
          Asylum Statistics by Nationality
        </h2>
        <p className="text-slate-500">
          Compare grant rates, case volumes, and representation across nationalities
        </p>
      </div>

      {/* Charts Row */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <ChartCard
          title="Top 10 by Case Volume"
          subtitle="Nationalities with most asylum cases"
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
                  formatter={(value: number) => [formatNumber(value), 'Cases']}
                />
                <Bar dataKey="totalCases" fill="#3b82f6" radius={[0, 4, 4, 0]} />
              </BarChart>
            </ResponsiveContainer>
          </div>
        </ChartCard>

        <ChartCard
          title="Top 10 by Grant Rate"
          subtitle="Nationalities with highest approval rates"
        >
          <div className="h-80">
            <ResponsiveContainer width="100%" height="100%">
              <BarChart data={topByGrantRate} layout="vertical">
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
                  formatter={(value: number) => [`${value}%`, 'Grant Rate']}
                />
                <Bar dataKey="grantRate" radius={[0, 4, 4, 0]}>
                  {topByGrantRate.map((entry, index) => (
                    <Cell key={index} fill={getGrantRateColor(entry.grantRate)} />
                  ))}
                </Bar>
              </BarChart>
            </ResponsiveContainer>
          </div>
        </ChartCard>
      </div>

      {/* Scatter Plot: Representation vs Grant Rate */}
      <ChartCard
        title="Representation Rate vs Grant Rate"
        subtitle="Bubble size represents total case volume"
      >
        <div className="h-96">
          <ResponsiveContainer width="100%" height="100%">
            <ScatterChart margin={{ top: 20, right: 20, bottom: 20, left: 20 }}>
              <CartesianGrid strokeDasharray="3 3" stroke="#e2e8f0" />
              <XAxis
                type="number"
                dataKey="x"
                name="Representation"
                tick={{ fill: '#64748b', fontSize: 11 }}
                label={{
                  value: 'Legal Representation Rate (%)',
                  position: 'bottom',
                  fill: '#64748b',
                }}
                domain={[20, 90]}
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
                domain={[10, 95]}
              />
              <ZAxis type="number" dataKey="z" range={[100, 1000]} name="Cases" />
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
                        <p className="text-sm text-slate-600">
                          Representation: {data.x}%
                        </p>
                        <p className="text-sm text-slate-600">
                          Grant Rate: {data.y}%
                        </p>
                        <p className="text-sm text-slate-600">
                          Cases: {formatNumber(data.z)}
                        </p>
                      </div>
                    );
                  }
                  return null;
                }}
              />
              <Scatter data={scatterData} fill="#3b82f6" fillOpacity={0.6} />
            </ScatterChart>
          </ResponsiveContainer>
        </div>
        <div className="mt-4 p-4 bg-blue-50 rounded-lg">
          <p className="text-sm text-blue-800">
            <strong>Correlation:</strong> Higher legal representation rates generally
            correlate with higher grant rates across nationalities. Nationalities with
            representation rates above 60% have an average grant rate of 62%, compared
            to 28% for those below 40%.
          </p>
        </div>
      </ChartCard>

      {/* Full Data Table */}
      <ChartCard
        title="Complete Nationality Statistics"
        subtitle="All tracked nationalities with detailed metrics"
        actions={
          <div className="flex items-center gap-4">
            <div className="flex items-center gap-2">
              <span className="text-sm text-slate-500">Sort by:</span>
              <select
                value={sortBy}
                onChange={(e) => setSortBy(e.target.value as SortField)}
                className="px-3 py-1.5 border border-slate-200 rounded-lg text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
              >
                <option value="totalCases">Case Volume</option>
                <option value="grantRate">Grant Rate</option>
                <option value="representationRate">Representation</option>
              </select>
            </div>
          </div>
        }
      >
        <DataTable
          data={sortedData}
          columns={tableColumns}
          searchPlaceholder="Search nationality..."
          pageSize={15}
        />
      </ChartCard>
    </div>
  );
}
