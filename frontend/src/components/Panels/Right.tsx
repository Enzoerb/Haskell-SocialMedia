'use client';
import { useContext } from 'react';
import { UserContext } from '@/context/user.context';

export default function Right() {
  const { user } = useContext(UserContext);

  return (
    <div className="flex h-screen p-4 flex-col">
      <p className="font-bold text-3xl mb-8">λ-Social</p>

      <div className="border-2 p-2 rounded-lg hover:cursor-pointer hover:bg-gray-100 transition-all duration-300">
        <p className="font-bold">
          {user?.firstName} {user?.lastName}
        </p>
        <p className="font-light text-sm">@{user?.username}</p>
      </div>

      <div className="flex-1" />

      <button className="w-full ml-auto mb-3 mr-3 text-white bg-primary-600 hover:bg-primary-700 font-medium rounded-lg text-sm px-5 py-2.5 text-center">
        Postar nova λ
      </button>
    </div>
  );
}
