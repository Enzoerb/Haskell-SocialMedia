'use client';
import ChatRow from '@/components/ChatRow';
import { UserContext } from '@/context/user.context';
import { useContext, useEffect, useState } from 'react';
import useWebSocket from 'react-use-websocket';

const CONNECTION_URL = 'ws://localhost:9160';

export default function Left() {
  const { user } = useContext(UserContext);

  const [messages, setMessages] = useState<string[]>([]);
  const [input, setInput] = useState<string>('');

  const { sendMessage, lastMessage } = useWebSocket(
    `${CONNECTION_URL}/${user?.userUserId}`
  );

  useEffect(() => {
    if (lastMessage) {
      setMessages((prev) => [...prev, lastMessage.data]);
    }
  }, [lastMessage]);

  if (!user?.userUserId) {
    return <p>Faça login novamente para visualizar</p>;
  }

  return (
    <div className="flex  flex-col h-screen space-y-2">
      <h2 className="text-lg font-bold m-2">Chat Global</h2>
      <div className="h-full flex-1 w-full overflow-y-scroll overflow-x-hidden hide-scrollbar">
        {messages.map((message, index) => (
          <ChatRow key={index} msg={message} />
        ))}
      </div>

      <div className="p-4 space-y-4">
        <input
          className="bg-gray-50 border border-gray-300 text-gray-900 sm:text-sm rounded-lg block w-full p-2.5"
          value={input}
          onChange={(e) => setInput(e.target.value)}
          placeholder="Escreva uma mensagem"
        />
        <button
          className="w-full text-white bg-primary-600 hover:bg-primary-700 font-medium rounded-lg text-sm px-5 py-2.5 text-center"
          onClick={() => {
            sendMessage(input);
            setInput('');
          }}
        >
          Enviar
        </button>
      </div>
    </div>
  );
}
