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
            <path d="M18.244 2.25h3.308l-7.227 8.26 8.502 11.24H16.17l-5.214-6.817L4.99 21.75H1.68l7.73-8.835L1.254 2.25H8.08l4.713 6.231zm-1.161 17.52h1.833L7.084 4.126H5.117z"></path>
          </g>
        </svg>
      </div>

      <div className="w-1/2 h-screen items-center flex flex-row justify-center">
        <SignIn />
      </div>
    </main>
  );
}
