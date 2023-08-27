'use client';
import EditForm from '@/components/EditForm';
import Modal from '@/components/Modal';
import { UserContext } from '@/context/user.context';
import { useContext, useState } from 'react';

export default function Actions({ userId }: { userId: string }) {
  const [showModal, setShowModal] = useState(false);

  const { user } = useContext(UserContext);

  return (
    <>
      {showModal ? (
        <Modal setShowModal={setShowModal}>
          <EditForm />
        </Modal>
      ) : null}

      <div className="mt-8">
        {user?.userUserId === userId ? (
          <button
            className="ml-auto mb-3 mr-3 text-white bg-primary-600 hover:bg-primary-700 font-medium rounded-lg text-sm px-5 py-2.5 text-center"
            onClick={() => setShowModal(true)}
          >
            Editar perfil
          </button>
        ) : (
          <>
            <button className="ml-auto mb-3 mr-3 text-white bg-primary-600 hover:bg-primary-700 font-medium rounded-lg text-sm px-5 py-2.5 text-center">
              Enviar mensagem
            </button>
            <button className="ml-auto mb-3 mr-3 text-white bg-primary-600 hover:bg-primary-700 font-medium rounded-lg text-sm px-5 py-2.5 text-center">
              Seguir
            </button>
          </>
        )}
      </div>
    </>
  );
}
