import SignIn from '@/components/SignIn';

export default function Login() {
  return (
    <main className="flex flex-row">
      {/* <div className="w-1/2 bg-black h-screen">foo</div> */}

      <div className="w-1/2  border-r-2 h-screen">
        <svg
          viewBox="0 0 24 24"
          aria-hidden="true"
          fill="#69196e"
          className="r-18jsvk2 r-4qtqp9 r-yyyyoo r-rxcuwo r-1777fci r-m327ed r-dnmrzs r-494qqr r-bnwqim r-1plcrui r-lrvibr"
        >
          <g>
            <path d="m 8 2 L 23 22 H 16 l -5.214 -6.817 L 4.99 21.75 H 1.68 l 7.32 -8.75 L 1 2 H 8 l 15 20 z m 9 18 h 2 L 7.084 4.126 H 5.117 z"></path>
          </g>
        </svg>
      </div>

      <div className="w-1/2 h-screen items-center flex flex-row justify-center">
        <SignIn />
      </div>
    </main>
  );
}
