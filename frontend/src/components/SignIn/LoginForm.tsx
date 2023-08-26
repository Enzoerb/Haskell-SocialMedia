'use client';

import { useRouter } from 'next/navigation';
import { Dispatch, SetStateAction } from 'react';
import { useForm, SubmitHandler } from 'react-hook-form';

type Inputs = {
  email: string
  password: string
}

type User = {
  email: string
  firstName: string
  lastName: string
  password: string
  userCreatedAt: string
  userUpdatedAt: string
  userUserId: string
  username: string
}

async function hash(str: string) {
  const utf8 = new TextEncoder().encode(str);
  const hashBuffer = await crypto.subtle.digest('SHA-256', utf8);
  const hashArray = Array.from(new Uint8Array(hashBuffer));
  const hashHex = hashArray
    .map((bytes) => bytes.toString(16).padStart(2, '0'))
    .join('');
  return hashHex;
}


export default function LoginForm({
  setState,
}: {
  setState: Dispatch<SetStateAction<'LOGIN' | 'REGISTER'>>;
}) {
  const router = useRouter();

  const { register, handleSubmit } = useForm<Inputs>();
  const onSubmit: SubmitHandler<Inputs> = async ({email, password}) => {
    const res = await fetch(`http://localhost:8080/user/email/${email}`, { cache: 'no-store'})

    if (!res.ok) {
      throw new Error('Failed to fetch data');
    }

    const data = await res.json() as User | null;

    if (!data) {
      alert(`Usuário o endereço de e-mail: ${email}, não foi encontrado! Verifique suas informações.`)
      return
    }

    const passwordHash = await hash(password)

    if (data?.password === passwordHash) {
      console.log("Login realizado com sucesso!")

      return router.push('/home')
    } else {
      alert('Senha incorreta! Tente novamente.')
      return
    }
  }

  return (
    <form className="space-y-4 w-1/2" onSubmit={handleSubmit(onSubmit)}>
      <div>
        <label className="block mb-2 text-sm font-medium text-gray-900">
          Seu e-mail
        </label>
        <input
          type="email"
          id="email"
          className="bg-gray-50 border border-gray-300 text-gray-900 sm:text-sm rounded-lg block w-full p-2.5"
          placeholder="email@email.com"
          {...register('email', {required: true})}
        />
      </div>
      <div>
        <label className="block mb-2 text-sm font-medium text-gray-900">
          Senha
        </label>
        <input
          type="password"
          id="password"
          placeholder="••••••••"
          className="bg-gray-50 border border-gray-300 text-gray-900 sm:text-sm rounded-lg block w-full p-2.5"
          {...register('password', {required: true})}
        />
      </div>
      <button
        type="submit"
        className="w-full text-white bg-primary-600 hover:bg-primary-700 font-medium rounded-lg text-sm px-5 py-2.5 text-center"
      >
        Entrar
      </button>
      <p className="text-sm font-light text-gray-500">
        Ainda não possui uma conta?{' '}
        <span
          className="font-medium text-primary-600 hover:underline hover:cursor-pointer"
          onClick={() => setState('REGISTER')}
        >
          Cadastre-se
        </span>
      </p>
    </form>
  );
}
