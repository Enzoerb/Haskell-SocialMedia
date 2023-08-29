const FALLBACK_URL = 'http://backend:8080';
const BASE_URL = 'http://localhost:8080';

export default async function fetchFallbackURL(path: string, requestOptions?: RequestInit): Promise<Response> {
  return new Promise((resolve, reject) => {
    fetch(`${BASE_URL}${path}`, requestOptions)
      .then(async (response) => {
        if (response.ok) {
          resolve(response);
        } else {
          const fallbackResponse = await fetch(`${FALLBACK_URL}${path}`, requestOptions);
          if (fallbackResponse.ok) {
            resolve(fallbackResponse);
          } else {
            reject(fallbackResponse);
          }
        }
      })
      .catch(async (error) => {
        const fallbackResponse = await fetch(`${FALLBACK_URL}${path}`, requestOptions);
        if (fallbackResponse.ok) {
          resolve(fallbackResponse);
        } else {
          reject(fallbackResponse);
        }
      });
  })
}