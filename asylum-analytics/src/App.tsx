import { useState } from 'react';
import type { TabType } from './types';
import Header from './components/Header';
import Sidebar from './components/Sidebar';
import OverviewTab from './components/tabs/OverviewTab';
import TrendsTab from './components/tabs/TrendsTab';
import NationalityTab from './components/tabs/NationalityTab';
import CourtsTab from './components/tabs/CourtsTab';
import JudgesTab from './components/tabs/JudgesTab';
import ProceedingsTab from './components/tabs/ProceedingsTab';
import RepresentationTab from './components/tabs/RepresentationTab';
import BacklogTab from './components/tabs/BacklogTab';
import DataSourcesFooter from './components/DataSourcesFooter';

function App() {
  const [activeTab, setActiveTab] = useState<TabType>('overview');
  const [sidebarOpen, setSidebarOpen] = useState(true);

  const renderTab = () => {
    switch (activeTab) {
      case 'overview':
        return <OverviewTab />;
      case 'trends':
        return <TrendsTab />;
      case 'nationality':
        return <NationalityTab />;
      case 'courts':
        return <CourtsTab />;
      case 'judges':
        return <JudgesTab />;
      case 'proceedings':
        return <ProceedingsTab />;
      case 'representation':
        return <RepresentationTab />;
      case 'backlog':
        return <BacklogTab />;
      default:
        return <OverviewTab />;
    }
  };

  return (
    <div className="min-h-screen bg-slate-50">
      <Header sidebarOpen={sidebarOpen} setSidebarOpen={setSidebarOpen} />

      <div className="flex">
        <Sidebar
          activeTab={activeTab}
          setActiveTab={setActiveTab}
          isOpen={sidebarOpen}
        />

        <main
          className={`flex-1 transition-all duration-300 ${
            sidebarOpen ? 'ml-64' : 'ml-0'
          }`}
        >
          <div className="p-6">
            {renderTab()}
          </div>

          <DataSourcesFooter />
        </main>
      </div>
    </div>
  );
}

export default App;
