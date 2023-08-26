'use client'
import { useContext } from 'react'
import { UserContext } from '@/context/user.context'

export default function Left() {
  const {user} = useContext(UserContext)

  return (
    <div className="flex h-screen">
      Ola mundo {user?.firstName}
    </div>
  )
}