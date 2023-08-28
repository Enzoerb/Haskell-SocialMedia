'use client';

import Photo from '@/components/Profile/Photo/Photo.client';
import { User } from '@/context/user.context';
import Link from 'next/link';
import { useEffect, useState } from 'react';

async function getFollowing(id: string) {
  const res = await fetch(`http://localhost:8080/follows/following/${id}`);

  if (!res.ok) {
    throw new Error('Failed to fetch data');
  }

  return res.json() as Promise<User[]>;
}

export default function Following({ id }: { id: string }) {
  const [following, setFollowing] = useState<User[]>([]);

  useEffect(() => {
    getFollowing(id).then((res) => setFollowing(res));

    return () => {};
  }, [id]);

  return (
    <>
      <h2 className="px-4 font-bold text-2xl"> Seguindo:</h2>
      <div className="flex items-stretch justify-stretch p-4">
        {following.map((user) => (
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

        {following.length === 0 && <p>Você ainda não está seguindo ninguém!</p>}
      </div>
    </>
  );
}
