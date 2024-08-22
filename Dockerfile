# Base image https://hub.docker.com/u/rocker/
# Match R version in renv.lock
FROM rocker/r-ver:3.6.2

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    default-jdk

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Copy project files
COPY server.R ./server.R
COPY ui.R ./ui.R
COPY global.R ./global.R
COPY config.yaml ./config.yaml
COPY renv.lock ./renv.lock
COPY www/ ./www/
COPY utils/ ./utils/

# Install renv & restore packages
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN Rscript -e 'renv::consent(provided=TRUE)'
RUN Rscript -e 'renv::restore()'

# Expose port
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp(host = '0.0.0.0', port = 3838)"]
