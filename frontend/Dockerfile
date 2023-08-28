FROM node:18

WORKDIR /app/frontend

COPY . /app/frontend

ENV NEXT_PUBLIC_API_URL=http://backend:8080

# Install dependencies and start the application with the until loop
CMD sh -c "npm install && until curl -sSf http://backend:8080/healthcheck; do sleep 1; done && npm run build && npm run start"

EXPOSE 3000
