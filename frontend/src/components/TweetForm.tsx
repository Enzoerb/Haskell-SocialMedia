'use client';

import { useContext, useTransition } from 'react';
import { UserContext } from '@/context/user.context';
import { useForm, SubmitHandler } from 'react-hook-form';
import { useRouter } from 'next/navigation';

type InputTweet = {
  insertContent: string;
  insertPostUserId: string;
};

export default function TweetForm() {
  const router = useRouter();
  const { user } = useContext(UserContext);

  const [isPending, startTransition] = useTransition();

  const { register, handleSubmit, reset } = useForm<InputTweet>();
  const onSubmit: SubmitHandler<InputTweet> = (data) => {
    if (!user) {
      alert('Você precisa estar logado para postar uma λ');
      return;
    }

    const headers = new Headers();
    headers.append('Content-Type', 'application/json');

    var requestOptions: RequestInit = {
      method: 'PUT',
      headers: headers,
      body: JSON.stringify({
        ...data,
        insertPostUserId: user?.userUserId,
        insertPostType: 'Default',
      }),
      redirect: 'follow',
    };

    fetch('http://localhost:8080/post', requestOptions)
      .then((response) => response.text())
      .then((result) => console.log(result))
      .then(() => {
        reset();
      })
      .then(() => {
        startTransition(() => {
          router.refresh();
        });
      })
      .catch((error) => {
        console.log('error', error);
        alert('Erro ao postar uma λ. Tente novamente.');
      });
  };

  return (
    <form
      className="w-full h-[25vh] border-b-2 flex flex-col"
      onSubmit={handleSubmit(onSubmit)}
    >
      <textarea
        className="outline-none w-full resize-none p-4 h-full"
        placeholder="O que você está pensando?"
        {...register('insertContent', { required: true })}
      ></textarea>

      <div className="flex">
        <button
          type="submit"
          className="ml-auto mb-3 mr-3 text-white bg-primary-600 hover:bg-primary-700 font-medium rounded-lg text-sm px-5 py-2.5 text-center"
        >
          Postar
        </button>
      </div>
    </form>
  );
}
