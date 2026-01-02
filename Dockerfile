# Base image https://hub.docker.com/u/rocker/
# Match R version in renv.lock
FROM rocker/r-ver:4.5.2 AS deps
WORKDIR /code
# Install system dependencies
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
    default-jdk \
    cmake

# Update all system packages
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# Copy project files
RUN mkdir -p renv
COPY renv.lock ./renv.lock
COPY renv/activate.R renv/activate.R
COPY renv/settings.dcf renv/settings.dcf

# change default location of cache to project folder
RUN mkdir renv/.cache
ENV RENV_PATHS_CACHE=renv/.cache

# Install renv & restore packages
RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN Rscript -e 'renv::consent(provided=TRUE)'
ENV RENV_CONFIG_REPOS_OVERRIDE=https://packagemanager.rstudio.com/cran/latest
RUN Rscript -e 'renv::restore()'

FROM deps
COPY --from=deps /code .
COPY server.R ./server.R
COPY ui.R ./ui.R
COPY global.R ./global.R
COPY config.yaml ./config.yaml
COPY www/ ./www/
COPY utils/ ./utils/

# Expose port
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp(host = '0.0.0.0', port = 3838)"]
