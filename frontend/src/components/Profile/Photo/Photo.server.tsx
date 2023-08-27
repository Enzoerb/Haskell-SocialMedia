/* eslint-disable @next/next/no-img-element */
import { getProfileImage } from './getProfileImage';

export default async function Photo({ id }: { id: string }) {
  const base64Photo = (await getProfileImage(id)).replaceAll('"', '');

  return (
    <div className=" flex items-stretch justify-stretch">
      <img
        className="flex-grow border-2 rounded-lg"
        src={base64Photo}
        alt="Foto de perfil"
      />
    </div>
  );
}
