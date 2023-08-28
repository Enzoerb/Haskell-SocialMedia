import LeftPanel from '@/components/Panels/Left';
import { Tweet } from '@/components/Panels/Main';
import RightPanel from '@/components/Panels/Right';
import Actions from '@/components/Profile/Actions';
import Photo from '@/components/Profile/Photo/Photo.server';
import TweetCard from '@/components/TweetCard';
import { User } from '@/context/user.context';

async function getUserInfo(id: string) {
  const res = await fetch(`http://backend:8080/user/id/${id}`, {
    cache: 'no-store',
  });

  if (!res.ok) {
    throw new Error('Failed to fetch data');
  }

  return res.json() as Promise<User>;
}

async function getUserTweets(id: string) {
  const res = await fetch(`http://backend:8080/posts/user/${id}`, {
    cache: 'no-store',
  });

  if (!res.ok) {
    throw new Error('Failed to fetch data');
  }

  return res.json() as Promise<Tweet[]>;
}

export default async function User({
  params,
}: {
  params: { id: string } & unknown;
}) {
  const user = await getUserInfo(params?.id);
  const tweets = await getUserTweets(params?.id);

  if (!user) {
    return <>Usuário nao encontrado</>;
  }

  return (
    <main className="flex flex-row overflow-hidden">
      <div className="w-1/4 border-r-2">
        <RightPanel />
      </div>

      <div className="w-1/2 h-screen overflow-y-scroll hide-scrollbar">
        <div className="w-full h-[35vh] border-b-2 p-4">
          <div className="flex flex-row">
            <div className="w-[30vh] h-[30vh]">
              <Photo id={params?.id} />
            </div>

            <div className="pl-8 flex flex-col">
              <h3 className="font-bold text-2xl mt-4">
                {user?.firstName} {user?.lastName}
              </h3>
              <p className="font-light text-lg">@{user?.username}</p>

              <p className="text-xs mt-2">
                Usuário desde:{' '}
                {new Date(user?.userCreatedAt).toLocaleString('pt-br')}
              </p>

              <p className="text-xs mt-2">
                Atualizado:{' '}
                {new Date(user?.userUpdatedAt).toLocaleString('pt-br')}
              </p>

              <Actions userId={user.userUserId} />
            </div>
          </div>
        </div>

        <div className="flex flex-col">
          {tweets?.map((tweet) => (
            <TweetCard
              key={tweet?.postId}
              content={tweet?.content}
              userId={tweet?.postUserId}
              createdAt={tweet?.postCreatedAt}
              name={tweet?.userFirstName}
              username={tweet?.userUsername}
            />
          ))}
        </div>
      </div>

      <div className="w-1/4 border-l-2">
        <LeftPanel />
      </div>
    </main>
  );
}
