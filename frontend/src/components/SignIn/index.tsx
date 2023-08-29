'use client';
import Modal from '@/components/Modal';
import { useState } from 'react';
import LoginForm from './LoginForm';
import RecoveryForm from './RecoveryForm';
import RegisterForm from './RegisterForm';

export default function SignIn() {
  const [showModal, setShowModal] = useState(false);

  const [currentForm, setCurrentForm] = useState<'LOGIN' | 'REGISTER'>('LOGIN');

  return (
    <>
      {showModal ? (
        <Modal setShowModal={setShowModal}>
          <RecoveryForm />
        </Modal>
      ) : null}

      {currentForm == 'LOGIN' ? (
        <LoginForm setState={setCurrentForm} />
      ) : (
        <RegisterForm setState={setCurrentForm} />
      )}
      <button className="mt-8 text-primary" onClick={() => setShowModal(true)}>
        Esqueci minha senha!
      </button>
    </>
  );
}
