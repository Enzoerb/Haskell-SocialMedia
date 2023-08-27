import LeftPanel from '@/components/Panels/Left';
import { Tweet } from '@/components/Panels/Main';
import RightPanel from '@/components/Panels/Right';
import { User } from '@/context/user.context';
import TweetCard from '@/components/TweetCard';

async function getUserInfo(id: string) {
  const res = await fetch(`http://localhost:8080/user/id/${id}`, {
    next: { revalidate: 600 },
  });

  if (!res.ok) {
    throw new Error('Failed to fetch data');
  }

  return res.json() as Promise<User>;
}

async function getUserTweets(id: string) {
  const res = await fetch(`http://localhost:8080/posts/user/${id}`, {
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
            <div className="w-[30vh] h-[30vh] bg-black rounded-lg" />

            <div className="pl-8 flex flex-col">
              <h3 className="font-bold text-2xl mt-4">
                {user?.firstName} {user?.lastName}
              </h3>
              <p className="font-light text-lg">@{user?.username}</p>

              <p className="text-xs mt-2">
                Usuário desde:{' '}
                {new Date(user?.userCreatedAt).toLocaleString('pt-br')}
              </p>

              <div className="mt-8">
                <button className="ml-auto mb-3 mr-3 text-white bg-primary-600 hover:bg-primary-700 font-medium rounded-lg text-sm px-5 py-2.5 text-center">
                  Enviar mensagem
                </button>
                <button className="ml-auto mb-3 mr-3 text-white bg-primary-600 hover:bg-primary-700 font-medium rounded-lg text-sm px-5 py-2.5 text-center">
                  Seguir
                </button>
              </div>
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
