import {
  LayoutDashboard,
  TrendingUp,
  Globe2,
  Building2,
  Scale,
  FileText,
  Users,
  Clock,
} from 'lucide-react';
import type { TabType } from '../types';

interface SidebarProps {
  activeTab: TabType;
  setActiveTab: (tab: TabType) => void;
  isOpen: boolean;
}

const tabs = [
  { id: 'overview' as TabType, label: 'Overview', icon: LayoutDashboard },
  { id: 'trends' as TabType, label: 'Yearly Trends', icon: TrendingUp },
  { id: 'nationality' as TabType, label: 'By Nationality', icon: Globe2 },
  { id: 'courts' as TabType, label: 'Immigration Courts', icon: Building2 },
  { id: 'judges' as TabType, label: 'Judge Statistics', icon: Scale },
  { id: 'proceedings' as TabType, label: 'Court Proceedings', icon: FileText },
  { id: 'representation' as TabType, label: 'Legal Representation', icon: Users },
  { id: 'backlog' as TabType, label: 'Case Backlog', icon: Clock },
];

export default function Sidebar({ activeTab, setActiveTab, isOpen }: SidebarProps) {
  if (!isOpen) return null;

  return (
    <aside className="fixed left-0 top-[73px] h-[calc(100vh-73px)] w-64 bg-white border-r border-slate-200 overflow-y-auto">
      <nav className="p-4">
        <div className="space-y-1">
          {tabs.map((tab) => {
            const Icon = tab.icon;
            const isActive = activeTab === tab.id;

            return (
              <button
                key={tab.id}
                onClick={() => setActiveTab(tab.id)}
                className={`w-full flex items-center gap-3 px-3 py-2.5 rounded-lg text-left transition-colors ${
                  isActive
                    ? 'bg-blue-50 text-blue-700 font-medium'
                    : 'text-slate-600 hover:bg-slate-50 hover:text-slate-900'
                }`}
              >
                <Icon
                  className={`w-5 h-5 ${
                    isActive ? 'text-blue-600' : 'text-slate-400'
                  }`}
                />
                <span>{tab.label}</span>
              </button>
            );
          })}
        </div>

        <div className="mt-8 pt-6 border-t border-slate-200">
          <h3 className="px-3 text-xs font-semibold text-slate-400 uppercase tracking-wider mb-3">
            Quick Stats
          </h3>
          <div className="space-y-3 px-3">
            <div className="flex justify-between text-sm">
              <span className="text-slate-500">Total Pending</span>
              <span className="font-semibold text-slate-700">2.46M</span>
            </div>
            <div className="flex justify-between text-sm">
              <span className="text-slate-500">FY2024 Grant Rate</span>
              <span className="font-semibold text-green-600">44.2%</span>
            </div>
            <div className="flex justify-between text-sm">
              <span className="text-slate-500">Avg Wait Time</span>
              <span className="font-semibold text-orange-600">4.3 yrs</span>
            </div>
          </div>
        </div>
      </nav>
    </aside>
  );
}
