export default function User({params}: {params: {id: string} & unknown}) {
  return <div>Perfil do usuário: {params.id}</div>
}