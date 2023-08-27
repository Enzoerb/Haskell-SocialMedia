'use client';
import { useContext, useState } from 'react';
import { UserContext } from '@/context/user.context';
import TweetForm from '@/components/TweetForm';
import Link from 'next/link';
import Modal from '@/components/Modal';

export default function Right() {
  const [showModal, setShowModal] = useState(false);

  const { user } = useContext(UserContext);

  return (
    <>
      {showModal ? (
        <Modal setShowModal={setShowModal}>
          <TweetForm />
        </Modal>
      ) : null}

      <div className="flex h-screen p-4 flex-col">
        <Link href="/home" className="font-bold text-3xl mb-8">
          λ-Social
        </Link>

        <Link
          href={`/user/${user?.userUserId}`}
          className="border-2 p-2 rounded-lg hover:cursor-pointer hover:bg-gray-100 transition-all duration-300"
        >
          <p className="font-bold">
            {user?.firstName} {user?.lastName}
          </p>
          <p className="font-light text-sm">@{user?.username}</p>
        </Link>

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
