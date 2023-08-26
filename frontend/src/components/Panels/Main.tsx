import TweetCard from '@/components/TweetCard';

interface Tweet {
  content: string;
  postCreatedAt: string;
  postId: string;
  postType: string;
  postUpdatedAt: string;
  postUserId: string;
}

async function getTweets() {
  const res = await fetch('http://localhost:8080/posts', { cache: 'no-store' });

  if (!res.ok) {
    throw new Error('Failed to fetch data');
  }

  return res.json() as Promise<Tweet[]>;
}

export default async function Main() {
  const tweets = await getTweets();

  console.log(tweets)

  return (
    <div className="flex flex-col">
      {tweets?.map((tweet) => (
        <TweetCard
          key={tweet?.postId}
          id={tweet?.postId}
          content={tweet?.content}
          user={tweet?.postUserId}
          createdAt={tweet?.postCreatedAt}
        />
      ))}
    </div>
  );
}
