import LoginForm from "@/components/LoginForm";

export default function Login() {
  return (
    <main className="flex flex-row">
      <div className="w-1/2 bg-black h-screen">foo</div>

      <div className="w-1/2 h-screen items-center flex flex-row justify-center">
        <LoginForm />
      </div>
    </main>
  );
}
