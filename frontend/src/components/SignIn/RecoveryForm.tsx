'use client';

import { User } from '@/context/user.context';
import { useState } from 'react';
import { SubmitHandler, useForm } from 'react-hook-form';

type Inputs = {
  email: string;
  password: string;
};

async function hash(str: string) {
  const utf8 = new TextEncoder().encode(str);
  const hashBuffer = await crypto.subtle.digest('SHA-256', utf8);
  const hashArray = Array.from(new Uint8Array(hashBuffer));
  const hashHex = hashArray
    .map((bytes) => bytes.toString(16).padStart(2, '0'))
    .join('');
  return hashHex;
}

export default function LoginForm() {
  const [sent, setSent] = useState(false);

  const { register, handleSubmit } = useForm<Inputs>();
  const onSubmit: SubmitHandler<Inputs> = async ({ email, password }) => {
    const res = await fetch(`http://localhost:8080/user/email/${email}`, {
      cache: 'no-store',
    });

    if (!res.ok) {
      throw new Error('Failed to fetch data');
    }

    const data = (await res.json()) as User;

    if (!data) {
      alert(
        `Usuário o endereço de e-mail: ${email}, não foi encontrado! Verifique suas informações.`
      );
      return;
    }

    const recoveryRes = await fetch(
      `http://localhost:8080/recovery/${email}/${data.userUserId}`,
      {
        cache: 'no-store',
      }
    );

    if (!recoveryRes.ok) {
      alert('Erro ao recuperar a senha, tente mais tarde!');
      throw new Error('Failed to fetch data');
    }

    setSent(true);

    alert(
      'Link de redefinição de senha enviado! Verifique seu email, inclusive o spam.'
    );
  };

  return (
    <form className="space-y-4 w-full p-4" onSubmit={handleSubmit(onSubmit)}>
      <div>
        <label className="block mb-2 text-sm font-medium text-gray-900">
          Digite seu email cadastrado!
        </label>
        <input
          type="email"
          id="email"
          className="bg-gray-50 border border-gray-300 text-gray-900 sm:text-sm rounded-lg block w-full p-2.5"
          placeholder="email@email.com"
          {...register('email', { required: true })}
        />
      </div>
      <button
        type="submit"
        className="w-full text-white bg-primary-600 hover:bg-primary-700 font-medium rounded-lg text-sm px-5 py-2.5 text-center"
        disabled={sent}
      >
        Enviar email
      </button>
    </form>
  );
}
