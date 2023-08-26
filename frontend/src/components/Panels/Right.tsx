'use client';
import { useContext, useState } from 'react';
import { UserContext } from '@/context/user.context';
import TweetForm from '@/components/TweetForm';

export default function Right() {
  const [showModal, setShowModal] = useState(false);

  const { user } = useContext(UserContext);

  return (
    <>
      {showModal ? (
        <>
          <div className="justify-center items-center flex overflow-x-hidden overflow-y-auto fixed inset-0 z-50">
            <div className="relative w-2/5 my-6 mx-auto">
              <div className="border-0 rounded-md shadow-lg relative flex flex-col w-full bg-white">
                <div className="flex items-start justify-between px-5 pb-5">
                  <button
                    className="ml-auto bg-transparent border-0 text-black float-right text-3xl leading-none font-semibold"
                    onClick={() => setShowModal(false)}
                  >
                    <span className="bg-transparent text-black h-6 w-6 text-2xl block">
                      ×
                    </span>
                  </button>
                </div>
                <TweetForm />
              </div>
            </div>
          </div>
          <div className="opacity-25 fixed inset-0 z-40 bg-black"></div>
        </>
      ) : null}

      <div className="flex h-screen p-4 flex-col">
        <p className="font-bold text-3xl mb-8">λ-Social</p>

        <div className="border-2 p-2 rounded-lg hover:cursor-pointer hover:bg-gray-100 transition-all duration-300">
          <p className="font-bold">
            {user?.firstName} {user?.lastName}
          </p>
          <p className="font-light text-sm">@{user?.username}</p>
        </div>

        <div className="flex-1" />

        <button
          className="w-full ml-auto mb-3 mr-3 text-white bg-primary-600 hover:bg-primary-700 font-medium rounded-lg text-sm px-5 py-2.5 text-center"
          onClick={() => setShowModal(true)}
        >
          Postar nova λ
        </button>
      </div>
    </>
  );
}
