FROM node:18

RUN mkdir /app

WORKDIR /app/frontend

COPY package*.json ./

RUN npm install

COPY . .

RUN npm run build

CMD ["yarn", "start" ]
