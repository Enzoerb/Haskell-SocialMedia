'use client';
import EditForm from '@/components/EditForm';
import Modal from '@/components/Modal';
import { User, UserContext } from '@/context/user.context';
import { useContext, useEffect, useState } from 'react';

async function getFollowing(userId: string, profileUserId: string) {
  const res = await fetch(`http://localhost:8080/follows/following/${userId}`, {
    cache: 'no-store',
  });

  if (!res.ok) {
    throw new Error('Failed to fetch data');
  }

  const data = (await res.json()) as User[];
  // check if profile user is in following list
  if (!data.length) return false;

  const following = data.find((user) => user?.userUserId === profileUserId);

  return following ? true : false;
}

export default function Actions({ userId }: { userId: string }) {
  const [showModal, setShowModal] = useState(false);
  const [following, setFollowing] = useState(false);

  const { user } = useContext(UserContext);

  useEffect(() => {
    if (user?.userUserId && userId) {
      getFollowing(user?.userUserId, userId)
        .then((res) => {
          setFollowing(res);
        })
        .catch((err) => {
          console.log(err);
        });
    }
  }, [user?.userUserId, userId]);

  const followUser = async () => {
    const headers = new Headers();
    headers.append('Content-Type', 'application/json');

    var requestOptions: RequestInit = {
      method: 'PUT',
      headers: headers,
      body: JSON.stringify({
        insertUserFollowedId: userId,
        insertUserFollowerId: user?.userUserId,
      }),
      redirect: 'follow',
    };

    fetch('http://localhost:8080/follow', requestOptions)
      .then((response) => response.text())
      .then((result) => console.log(result))
      .then(() => setFollowing(true))
      .catch((error) => console.log('error', error));
  };

  const unfollowUser = async () => {
    if (confirm('Tem certeza que deseja Deixar de seguir esse usuário?')) {
      const headers = new Headers();
      headers.append('Content-Type', 'application/json');

      var requestOptions: RequestInit = {
        method: 'DELETE',
        headers: headers,
        redirect: 'follow',
      };

      fetch(
        `http://localhost:8080/follow?user_followed=${userId}&user_follower=${user?.userUserId}`,
        requestOptions
      )
        .then((response) => response.text())
        .then((result) => console.log(result))
        .then(() => setFollowing(false))
        .catch((error) => console.log('error', error));
    }
  };

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
            {following ? (
              <button
                onClick={() => unfollowUser()}
                className="ml-auto mb-3 mr-3 text-white bg-red-600 hover:bg-red-700 font-medium rounded-lg text-sm px-5 py-2.5 text-center"
              >
                Deixar de seguir
              </button>
            ) : (
              <button
                onClick={() => followUser()}
                className="ml-auto mb-3 mr-3 text-white bg-primary-600 hover:bg-primary-700 font-medium rounded-lg text-sm px-5 py-2.5 text-center"
              >
                Seguir
              </button>
            )}
          </>
        )}
      </div>
    </>
  );
}
