# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny-verse:latest

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Copy project files
COPY server.R ./server.R
COPY ui.R ./ui.R
COPY global.R ./global.R
COPY config/ ./config/
COPY renv.lock ./renv.lock
COPY www/ ./www/
COPY utils/ ./utils/

# Install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'

# Expose port
EXPOSE 80

CMD ["R", "-e", "shiny::runApp(host = '0.0.0.0', port = 80)"]