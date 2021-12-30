FROM rocker/shiny-verse:4.1.2
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*
ENV RENV_VERSION 0.14.0
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"
COPY ./renv.lock .
RUN Rscript -e "renv::restore()"
RUN addgroup --system docker \
    && adduser --system --ingroup shiny docker
RUN chown shiny:docker -R /srv/shiny-server/