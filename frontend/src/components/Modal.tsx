import { Dispatch, ReactNode, SetStateAction } from 'react';

export default function Modal({
  children,
  setShowModal,
}: {
  children: ReactNode;
  setShowModal: Dispatch<SetStateAction<boolean>>;
}) {
  return (
    <>
      <div className="justify-center items-center flex overflow-x-hidden overflow-y-auto fixed inset-0 z-50">
        <div className="relative w-2/5 my-6 mx-auto">
          <div className="border-0 rounded-md shadow-lg relative flex flex-col w-full bg-white">
            <div className="flex items-start justify-between px-5 pb-5">
              <button
                className="ml-auto bg-transparent border-0 text-black float-right text-3xl leading-none font-semibold"
                onClick={() => setShowModal(false)}
              >
                <span className="bg-transparent text-black h-6 w-6 text-2xl block">
                  ×
                </span>
              </button>
            </div>
            {children}
          </div>
        </div>
      </div>
      <div className="opacity-25 fixed inset-0 z-40 bg-black"></div>
    </>
  );
}
