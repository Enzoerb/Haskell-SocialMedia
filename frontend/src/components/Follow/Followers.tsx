'use client';

import Photo from '@/components/Profile/Photo/Photo.client';
import { User } from '@/context/user.context';
import fetchFallbackURL from '@/services/fetchFallback';
import Link from 'next/link';
import { useEffect, useState } from 'react';

async function getFollowers(id: string) {
  const res = await fetchFallbackURL(`/follows/followers/${id}`);

  if (!res.ok) {
    throw new Error('Failed to get data');
  }

  return res.json() as Promise<User[]>;
}

export default function Followers({ id }: { id: string }) {
  const [followers, setFollowers] = useState<User[]>([]);

  useEffect(() => {
    getFollowers(id).then((res) => setFollowers(res));

    return () => {};
  }, [id]);

  return (
    <>
      <h2 className="px-4 font-bold text-2xl"> Seguidores:</h2>
      <div className="flex items-stretch justify-stretch p-4">
        {followers.map((user) => (
          <div key={user?.userUserId} className="flex flex-row">
            {user?.userUserId && (
              <Link
                href={`/user/${user?.userUserId}`}
                className="font-bold hover:underline flex items-center"
              >
                <div className="w-5 h-5 mr-3">
                  <Photo id={user?.userUserId} />
                </div>
                {user?.firstName}{' '}
                <span className="font-light text-sm ml-1">
                  (@{user?.username})
                </span>
              </Link>
            )}
          </div>
        ))}

        {followers.length === 0 && <p>Você ainda não tem seguidores!</p>}
      </div>
    </>
  );
}
