import { dataSources } from '../data/asylumData';
import { ExternalLink, Info } from 'lucide-react';

export default function DataSourcesFooter() {
  return (
    <footer className="bg-slate-800 text-slate-300 mt-8">
      <div className="max-w-7xl mx-auto px-6 py-8">
        <div className="flex items-start gap-3 mb-6">
          <Info className="w-5 h-5 text-blue-400 mt-0.5" />
          <div>
            <h3 className="text-white font-semibold mb-2">Data Sources & Methodology</h3>
            <p className="text-sm text-slate-400 max-w-3xl">
              This dashboard presents asylum data compiled from publicly available sources including
              EOIR (Executive Office for Immigration Review) statistics obtained through FOIA requests,
              TRAC Immigration reports, USCIS asylum division data, and DHS immigration statistics.
              Data is aggregated and may not reflect real-time case status.
            </p>
          </div>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
          {dataSources.map((source) => (
            <div key={source.name} className="bg-slate-700/50 rounded-lg p-4">
              <h4 className="text-white font-medium mb-1">{source.name}</h4>
              <p className="text-sm text-slate-400 mb-3">{source.description}</p>
              <div className="flex items-center justify-between">
                <span className="text-xs text-slate-500">
                  Updated: {source.lastUpdated}
                </span>
                <a
                  href={source.url}
                  target="_blank"
                  rel="noopener noreferrer"
                  className="flex items-center gap-1 text-xs text-blue-400 hover:text-blue-300"
                >
                  Visit <ExternalLink className="w-3 h-3" />
                </a>
              </div>
            </div>
          ))}
        </div>

        <div className="mt-8 pt-6 border-t border-slate-700 text-center text-sm text-slate-500">
          <p>
            Asylum Data Analytics Dashboard | Data for educational and research purposes only
          </p>
          <p className="mt-1">
            For official statistics, please refer to the original data sources listed above.
          </p>
        </div>
      </div>
    </footer>
  );
}
