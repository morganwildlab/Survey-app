import type { ReactNode } from 'react';
import { TrendingUp, TrendingDown, Minus } from 'lucide-react';

interface StatCardProps {
  title: string;
  value: string | number;
  subtitle?: string;
  icon?: ReactNode;
  trend?: {
    value: number;
    label: string;
  };
  color?: 'blue' | 'green' | 'red' | 'orange' | 'purple' | 'slate';
}

const colorClasses = {
  blue: {
    bg: 'bg-blue-50',
    icon: 'text-blue-600',
    border: 'border-blue-100',
  },
  green: {
    bg: 'bg-green-50',
    icon: 'text-green-600',
    border: 'border-green-100',
  },
  red: {
    bg: 'bg-red-50',
    icon: 'text-red-600',
    border: 'border-red-100',
  },
  orange: {
    bg: 'bg-orange-50',
    icon: 'text-orange-600',
    border: 'border-orange-100',
  },
  purple: {
    bg: 'bg-purple-50',
    icon: 'text-purple-600',
    border: 'border-purple-100',
  },
  slate: {
    bg: 'bg-slate-50',
    icon: 'text-slate-600',
    border: 'border-slate-100',
  },
};

export default function StatCard({
  title,
  value,
  subtitle,
  icon,
  trend,
  color = 'blue',
}: StatCardProps) {
  const colors = colorClasses[color];

  return (
    <div className={`bg-white rounded-xl border ${colors.border} p-5 shadow-sm`}>
      <div className="flex items-start justify-between">
        <div className="flex-1">
          <p className="text-sm font-medium text-slate-500 mb-1">{title}</p>
          <p className="text-2xl font-bold text-slate-800">{value}</p>
          {subtitle && (
            <p className="text-sm text-slate-500 mt-1">{subtitle}</p>
          )}
          {trend && (
            <div className="flex items-center gap-1 mt-2">
              {trend.value > 0 ? (
                <TrendingUp className="w-4 h-4 text-green-500" />
              ) : trend.value < 0 ? (
                <TrendingDown className="w-4 h-4 text-red-500" />
              ) : (
                <Minus className="w-4 h-4 text-slate-400" />
              )}
              <span
                className={`text-sm font-medium ${
                  trend.value > 0
                    ? 'text-green-600'
                    : trend.value < 0
                    ? 'text-red-600'
                    : 'text-slate-500'
                }`}
              >
                {trend.value > 0 ? '+' : ''}
                {trend.value}%
              </span>
              <span className="text-sm text-slate-400">{trend.label}</span>
            </div>
          )}
        </div>
        {icon && (
          <div className={`p-3 ${colors.bg} rounded-lg`}>
            <div className={colors.icon}>{icon}</div>
          </div>
        )}
      </div>
    </div>
  );
}
