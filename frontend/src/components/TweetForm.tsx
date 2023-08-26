export default function TweetForm() {
  return (
    <div className="w-full h-[25vh] border-b-2 flex flex-col">
      <textarea
        className="outline-none w-full resize-none p-4 h-full"
        placeholder="O que você está pensando?"
      ></textarea>

      <div className="flex">
        <button
          type="submit"
          className="ml-auto mb-3 mr-3 text-white bg-primary-600 hover:bg-primary-700 font-medium rounded-lg text-sm px-5 py-2.5 text-center"
        >
          Postar
        </button>
      </div>
    </div>
  );
}
