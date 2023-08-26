import SignIn from "@/components/SignIn";

export default function Login() {
  return (
    <main className="flex flex-row">
      <div className="w-1/2 bg-black h-screen">foo</div>

      <div className="w-1/2 h-screen items-center flex flex-row justify-center">
        <SignIn />
      </div>
    </main>
  );
}
