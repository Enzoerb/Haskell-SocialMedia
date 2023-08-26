import LeftPanel from '@/components/Panels/Left';
import MainPanel from '@/components/Panels/Main';
import RightPanel from '@/components/Panels/Right';

export default function Home() {
  return (
    <main className="flex flex-row overflow-hidden">
      <div className="w-1/4 border-r-2">
        <RightPanel />
      </div>

      <div className="w-1/2 h-screen overflow-y-scroll hide-scrollbar">
        <div className="w-full h-[25vh] border-b-2">

        </div>
        <MainPanel />
      </div>

      <div className="w-1/4 border-l-2">
        <LeftPanel />
      </div>
    </main>
  );
}
