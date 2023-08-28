FROM node:18

RUN mkdir /app

WORKDIR /app/frontend

COPY package*.json /app/frontend

RUN npm install

COPY . /app/frontend

# EXPOSE 3000