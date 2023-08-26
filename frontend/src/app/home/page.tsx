import LeftPanel from '@/components/Panels/Left';
import MainPanel from '@/components/Panels/Main';
import RightPanel from '@/components/Panels/Right';
import TweetForm from '@/components/TweetForm';


export default function Home() {
  return (
    <main className="flex flex-row overflow-hidden">
      <div className="w-1/4 border-r-2">
        <RightPanel />
      </div>

      <div className="w-1/2 h-screen overflow-y-scroll hide-scrollbar">
        <TweetForm />
        <MainPanel />
      </div>

      <div className="w-1/4 border-l-2">
        <LeftPanel />
      </div>
    </main>
  );
}
