import { Menu, BarChart3, Database, ExternalLink } from 'lucide-react';

interface HeaderProps {
  sidebarOpen: boolean;
  setSidebarOpen: (open: boolean) => void;
}

export default function Header({ sidebarOpen, setSidebarOpen }: HeaderProps) {
  return (
    <header className="bg-white border-b border-slate-200 sticky top-0 z-50">
      <div className="flex items-center justify-between px-4 py-3">
        <div className="flex items-center gap-4">
          <button
            onClick={() => setSidebarOpen(!sidebarOpen)}
            className="p-2 rounded-lg hover:bg-slate-100 transition-colors"
            aria-label="Toggle sidebar"
          >
            <Menu className="w-5 h-5 text-slate-600" />
          </button>

          <div className="flex items-center gap-3">
            <div className="p-2 bg-blue-600 rounded-lg">
              <BarChart3 className="w-6 h-6 text-white" />
            </div>
            <div>
              <h1 className="text-xl font-bold text-slate-800">
                Asylum Data Analytics
              </h1>
              <p className="text-sm text-slate-500">
                UK Home Office & Tribunal Data Dashboard
              </p>
            </div>
          </div>
        </div>

        <div className="flex items-center gap-4">
          <div className="flex items-center gap-2 px-3 py-1.5 bg-green-50 border border-green-200 rounded-full">
            <Database className="w-4 h-4 text-green-600" />
            <span className="text-sm font-medium text-green-700">
              Data: 2015-2024
            </span>
          </div>

          <a
            href="https://www.gov.uk/government/collections/immigration-statistics-quarterly-release"
            target="_blank"
            rel="noopener noreferrer"
            className="flex items-center gap-2 px-3 py-1.5 text-sm text-slate-600 hover:text-blue-600 hover:bg-slate-50 rounded-lg transition-colors"
          >
            <span>Home Office Data</span>
            <ExternalLink className="w-4 h-4" />
          </a>
        </div>
      </div>
    </header>
  );
}
