'use client';
import { useState } from 'react';
import LoginForm from './LoginForm';
import RegisterForm from './RegisterForm';

export default function SignIn() {
  const [currentForm, setCurrentForm] = useState<'LOGIN' | 'REGISTER'>('LOGIN');

  return (
    <>
      {currentForm == 'LOGIN' ? (
        <LoginForm setState={setCurrentForm} />
      ) : (
        <RegisterForm setState={setCurrentForm} />
      )}
    </>
  );
}
