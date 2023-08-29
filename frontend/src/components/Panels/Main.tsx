import TweetCard from '@/components/TweetCard';
import fetchFallbackURL from '@/services/fetchFallback';

export interface Tweet {
  content: string;
  postCreatedAt: string;
  postId: string;
  postType: string;
  postUpdatedAt: string;
  postUserId: string;
  userFirstName: string;
  userUsername: string;
}

async function getTweets() {
  const res = await fetchFallbackURL('/posts', { cache: 'no-store' });

  if (!res.ok) {
    throw new Error('Failed to get data');
  }

  return res.json() as Promise<Tweet[]>;
}

export default async function Main() {
  const tweets = await getTweets();

  return (
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
  );
}
