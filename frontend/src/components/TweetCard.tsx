import Link from 'next/link';

export default function TweetCard({
  content,
  userId,
  createdAt,
  username,
  name,
}: {
  content: string;
  userId: string;
  createdAt: string;
  username: string;
  name: string;
}) {
  return (
    <div className="px-6 pt-6 pb-3 border-b-2 hover:cursor-pointer hover:bg-gray-100 transition-all duration-300">
      <Link href={`/user/${userId}`} className="font-bold hover:underline">
        {name}
        {' '}
        <span className="font-light text-sm">
          (@{username})
        </span>
      </Link>

      <p className="my-4">{content}</p>

      <p className="font-body text-xs text-right italic">
        Publicado: {new Date(createdAt).toLocaleString('pt-br')}
      </p>
    </div>
  );
}
