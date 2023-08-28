"use client";

import React, { ReactNode, createContext, useState } from 'react';

export type User = {
  email: string;
  firstName: string;
  lastName: string;
  password: string;
  userCreatedAt: string;
  userUpdatedAt: string;
  userUserId: string;
  username: string;
} | null;

type UserContextType = {
  user: User;
  setUser: React.Dispatch<React.SetStateAction<User>>;
};

export const UserContext = createContext<UserContextType>({
  user: null,
  setUser: () => {},
});

const UserProvider: React.FC<{children: ReactNode}> = ({ children }) => {
  const [user, setUser] = useState<User>(null);

  return (
    <UserContext.Provider value={{ user, setUser }}>
      {children}
    </UserContext.Provider>
  );
};

export default UserProvider;