'use client';
import { Dispatch, SetStateAction, useState } from 'react';
import { SubmitHandler, useForm } from 'react-hook-form';
import LoadingSpinner from '../LoadingSpinner';

export type RegisterInputs = {
  insertEmail: string;
  insertFirstName: string;
  insertLastName: string;
  insertPassword: string;
  insertUsername: string;
};

export default function RegisterForm({
  setState,
}: {
  setState: Dispatch<SetStateAction<'LOGIN' | 'REGISTER'>>;
}) {
  const [isLoading, setIsLoading] = useState(false);

  const { register, handleSubmit } = useForm<RegisterInputs>();
  const onSubmit: SubmitHandler<RegisterInputs> = (data) => {
    setIsLoading(true);

    const headers = new Headers();
    headers.append('Content-Type', 'application/json');

    var requestOptions: RequestInit = {
      method: 'PUT',
      headers: headers,
      body: JSON.stringify(data),
      redirect: 'follow',
    };

    fetch('http://localhost:8080/user', requestOptions)
      .then((response) => response.text())
      .then((result) => console.log(result))
      .then(() => setIsLoading(false))
      .then(() =>
        alert(
          'Usuário cadastrado com sucesso! Você será redirecionado para a tela de login.'
        )
      )
      .then(() => setState('LOGIN'))
      .catch((error) => {
        console.log('error', error);
        alert('Erro ao cadastrar usuário. Tente novamente.');
      });
  };

  if (isLoading) {
    return <LoadingSpinner />;
  }

  return (
    <form className="space-y-4 w-1/2" onSubmit={handleSubmit(onSubmit)}>
      <div className="flex flex-row space-x-4">
        <div>
          <label className="block mb-2 text-sm font-medium text-gray-900">
            Digite seu nome
          </label>
          <input
            type="text"
            id="firstName"
            className="bg-gray-50 border border-gray-300 text-gray-900 sm:text-sm rounded-lg block w-full p-2.5"
            placeholder="Nome"
            {...register('insertFirstName', { required: true })}
          />
        </div>

        <div>
          <label className="block mb-2 text-sm font-medium text-gray-900">
            Digite seu sobrenome
          </label>
          <input
            type="text"
            id="lastName"
            className="bg-gray-50 border border-gray-300 text-gray-900 sm:text-sm rounded-lg block w-full p-2.5"
            placeholder="Sobrenome"
            {...register('insertLastName', { required: true })}
          />
        </div>
      </div>

      <div>
        <label className="block mb-2 text-sm font-medium text-gray-900">
          Digite seu nome de usuário
        </label>
        <input
          type="text"
          id="username"
          className="bg-gray-50 border border-gray-300 text-gray-900 sm:text-sm rounded-lg block w-full p-2.5"
          placeholder="username"
          {...register('insertUsername', { required: true })}
        />
      </div>

      <div>
        <label className="block mb-2 text-sm font-medium text-gray-900">
          Seu e-mail
        </label>
        <input
          type="email"
          id="email"
          className="bg-gray-50 border border-gray-300 text-gray-900 sm:text-sm rounded-lg block w-full p-2.5"
          placeholder="email@email.com"
          {...register('insertEmail', { required: true })}
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
          {...register('insertPassword', { required: true })}
        />
      </div>
      <button
        type="submit"
        className="w-full text-white bg-primary-600 hover:bg-primary-700 font-medium rounded-lg text-sm px-5 py-2.5 text-center"
      >
        Cadastrar
      </button>
      <p className="text-sm font-light text-gray-500">
        Já possui uma conta?{' '}
        <span
          className="font-medium text-primary-600 hover:underline hover:cursor-pointer"
          onClick={() => setState('LOGIN')}
        >
          Faça login
        </span>
      </p>
    </form>
  );
}
