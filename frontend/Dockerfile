FROM node:14-alpine

# Install required dependencies
RUN apk add --no-cache \
    curl \
    tar \
    git

# Install Spago
RUN npm install -g spago@0.21.0

# Set the working directory
RUN mkdir /app
WORKDIR /app/frontend

# Copy package.json and spago.dhall files
COPY packages.dhall spago.dhall /app/frontend

# Install npm dependencies
RUN spago install

# Copy the rest of the frontend code
COPY . /app/frontend

# Build the PureScript code using Spago
RUN spago build

# Expose the necessary port(s) for your frontend
EXPOSE 3000

# Start the frontend server
CMD ["spago", "run"]
