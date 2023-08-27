export async function getProfileImage(id: string) {
  const res = await fetch(`http://localhost:8080/identicon/${id}`);

  if (!res.ok) {
    throw new Error('Failed to fetch data');
  }

  return res.text();
}
