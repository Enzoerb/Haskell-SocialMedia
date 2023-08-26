import Link from 'next/link'

export default function TweetCard({id, content, user, createdAt}: {id: string, content: string, user: string, createdAt: string}) {
  return (
    <div className="px-6 pt-6 pb-3 border-b-2 hover:cursor-pointer hover:bg-gray-100 transition-all duration-300">
      <Link href="/" className='font-bold text-blue-500 hover:underline'>@{user}</Link>

      <p className='my-4'>{content}</p>

      <p className='font-body text-xs text-right'>Publicado: {new Date(createdAt).toLocaleString('pt-br')}</p>
    </div>
  )
}