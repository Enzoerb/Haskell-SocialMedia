import fetchFallbackURL from "@/services/fetchFallback";

export async function getProfileImage(id: string) {
  const res = await fetchFallbackURL(`/identicon/${id}`);

  if (!res.ok) {
    throw new Error('Failed to get data');
  }

  return res.text();
}
