'use client';

import { hash } from '@/components/SignIn/LoginForm';
import { User } from '@/context/user.context';
import { useRouter } from 'next/navigation';
import { SubmitHandler, useForm } from 'react-hook-form';
import fetchFallbackURL from '@/services/fetchFallback';

type Inputs = {
  email: string;
  password: string;
};

export default function User({ params }: { params: { id: string } & unknown }) {
  const { register, handleSubmit } = useForm<Inputs>();
  const router = useRouter();

  const onSubmit: SubmitHandler<Inputs> = async ({ email, password }) => {
    const res = await fetchFallbackURL(`/user/email/${email}`, {
      cache: 'no-store',
    });

    if (!res.ok) {
      throw new Error('Failed to get data');
    }

    const data = (await res.json()) as User;

    const RecoveryHash = await hash(`${email}${data?.userUserId}`);

    if (RecoveryHash != params.id) {
      alert('Link inválido para alterar esse endereço de email!');
      return;
    }

    const headers = new Headers();
    headers.append('Content-Type', 'application/json');

    var requestOptions: RequestInit = {
      method: 'PATCH',
      headers: headers,
      body: JSON.stringify({
        recoveryUserId: data?.userUserId,
        recoveryNewPassword: password,
      }),
    };

    fetchFallbackURL('/recovery', requestOptions)
      .then((response) => response.text())
      .then((result) => console.log(result))
      .then(() => alert('Senha alterada com sucesso!'))
      .then(() => {
        router.push('/');
      })
      .catch((error) => {
        console.log('error', error);
        alert('Erro ao recuperar a usuário. Tente novamente.');
      });
  };

  return (
    <main className="flex flex-row overflow-hidden">
      {/* <p>{params.id}</p> */}

      <form
        className="space-y-4 w-1/4 mx-auto my-12"
        onSubmit={handleSubmit(onSubmit)}
      >
        <div>
          <h2 className="text-lg font-bold mb-4">Recupere sua senha!</h2>
          <label className="block mb-2 text-sm font-medium text-gray-900">
            Seu e-mail
          </label>
          <input
            type="email"
            id="email"
            className="bg-gray-50 border border-gray-300 text-gray-900 sm:text-sm rounded-lg block w-full p-2.5"
            placeholder="email@email.com"
            {...register('email', { required: true })}
          />
        </div>
        <div>
          <label className="block mb-2 text-sm font-medium text-gray-900">
            Digite a nova senha
          </label>
          <input
            type="password"
            id="password"
            placeholder="••••••••"
            className="bg-gray-50 border border-gray-300 text-gray-900 sm:text-sm rounded-lg block w-full p-2.5"
            {...register('password', { required: true })}
          />
        </div>
        <button
          type="submit"
          className="w-full text-white bg-primary-600 hover:bg-primary-700 font-medium rounded-lg text-sm px-5 py-2.5 text-center"
        >
          Alterar senha
        </button>
      </form>
    </main>
  );
}
