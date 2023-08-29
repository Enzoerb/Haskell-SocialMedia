'use client';

import Photo from '@/components/Profile/Photo/Photo.client';
import Link from 'next/link';
import { useEffect, useState } from 'react';

import { User } from '@/context/user.context';
import fetchFallbackURL from '@/services/fetchFallback';

export async function getUserInfo(id: string) {
  const res = await fetchFallbackURL(`/user/id/${id}`, {
    cache: 'no-store',
  });

  if (!res.ok) {
    throw new Error('Failed to get data');
  }

  return res.json() as Promise<User>;
}

export default function ChatRow({ msg }: { msg: string }) {
  const [userId, setUserId] = useState('');
  const [textMessage, setTextMessage] = useState('');
  const [datetime, setDateTime] = useState('');
  const [userName, setUserName] = useState('');
  const [userUsername, setUserUsername] = useState('');

  useEffect(() => {
    if (userId) {
      getUserInfo(userId)
        .then((user) => {
          setUserName(user?.firstName || '');
          setUserUsername(user?.username || '');
        })
        .catch((error) => {
          console.log('error: ', error);
        });
    }
  }, [userId]);

  useEffect(() => {
    if (msg) {
      try {
        const data = JSON.parse(msg);

        data?.clientId && setUserId(data?.clientId);
        data?.message && setTextMessage(data?.message);
        data?.datetime && setDateTime(data?.datetime);
      } catch (error) {
        console.error('JSON Decode error', error);
      }
    }
  }, [msg]);

  return (
    <div className="w-full p-4 border-b-2">
      {userId && (
        <>
          <Link
            href={`/user/${userId}`}
            className="font-bold hover:underline flex items-center mb-2"
          >
            <div className="w-5 h-5 mr-3">
              <Photo id={userId} />
            </div>
            {userName}
            <span className="font-light text-sm ml-1">(@{userUsername})</span>
          </Link>

          <p>{textMessage}</p>

          <p className="font-body text-xs text-right italic">
            {new Date(datetime).toLocaleString('pt-br')}
          </p>
        </>
      )}
    </div>
  );
}
