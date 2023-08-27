/* eslint-disable @next/next/no-img-element */
'use client';
import { useEffect, useState } from 'react';
import { getProfileImage } from './getProfileImage';

export default function Photo({ id }: { id: string }) {
  const [base64Photo, setBase64Photo] = useState('');

  useEffect(() => {
    getProfileImage(id).then((res) => setBase64Photo(res.replaceAll('"', '')));

    return () => {};
  }, [id]);

  return (
    <div className="flex items-stretch justify-stretch">
      <img
        className="flex-grow border"
        src={base64Photo}
        alt="Foto de perfil"
      />
    </div>
  );
}
