'use client';
import LoadingSpinner from '@/components/LoadingSpinner';
import { User, UserContext } from '@/context/user.context';
import { useRouter } from 'next/navigation';
import { startTransition, useContext, useEffect, useState } from 'react';
import { SubmitHandler, useForm } from 'react-hook-form';

export type RegisterInputs = {
  oldUserId: string;
  newEmail: string;
  newFirstName: string;
  newLastName: string;
  newPassword: string;
  newUsername: string;
};

export default function EditForm() {
  const [isLoading, setIsLoading] = useState(false);

  const { user, setUser } = useContext(UserContext);
  const router = useRouter();

  const { register, handleSubmit, setValue } = useForm<RegisterInputs>();

  useEffect(() => {
    if (user) {
      setValue('oldUserId', user.userUserId);
      setValue('newEmail', user.email);
      setValue('newFirstName', user.firstName);
      setValue('newLastName', user.lastName);
      setValue('newUsername', user.username);
    }
  }, [setValue, user]);

  const onSubmit: SubmitHandler<RegisterInputs> = (data) => {
    setIsLoading(true);

    const headers = new Headers();
    headers.append('Content-Type', 'application/json');
    headers.append('Origin', 'http://127.0.0.1');

    var requestOptions: RequestInit = {
      method: 'PATCH',
      headers: headers,
      body: JSON.stringify(data),
    };

    fetch('http://localhost:8080/user', requestOptions)
      .then((response) => response.text())
      .then((result) => console.log(result))
      .then(() => alert('Informações alteradas com sucesso.'))
      .then(() => {
        const newUser = {
          ...user,
          email: data.newEmail,
          firstName: data.newFirstName,
          lastName: data.newLastName,
          username: data.newUsername,
        } as User;

        setUser(newUser);
      })
      .then(() => {
        startTransition(() => {
          router.refresh();
        });
      })
      .catch((error) => {
        console.log('error', error);
        alert('Erro ao cadastrar usuário. Tente novamente.');
      })
      .finally(() => {
        setIsLoading(false);
      });
  };

  if (isLoading) {
    return <LoadingSpinner />;
  }

  return (
    <form className="space-y-4 w-full p-4" onSubmit={handleSubmit(onSubmit)}>
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
            {...register('newFirstName', { required: true })}
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
            {...register('newLastName', { required: true })}
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
          {...register('newUsername', { required: true })}
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
          {...register('newEmail', { required: true })}
        />
      </div>

      <div>
        <label className="block mb-2 text-sm font-medium text-gray-900">
          Alterar senha
        </label>
        <input
          type="password"
          id="password"
          placeholder="••••••••"
          className="bg-gray-50 border border-gray-300 text-gray-900 sm:text-sm rounded-lg block w-full p-2.5"
          {...register('newPassword', { required: true })}
        />
      </div>
      <button
        type="submit"
        className="w-full text-white bg-primary-600 hover:bg-primary-700 font-medium rounded-lg text-sm px-5 py-2.5 text-center"
      >
        Salvar alterações
      </button>
    </form>
  );
}
