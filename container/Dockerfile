FROM docker.io/rocker/r-ver:4.4.1

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update -qy && \
    apt-get install -qy \
      curl \
      gdal-bin \
      gdal-data \
      git \
      libcurl4-openssl-dev \
      libgdal-dev \
      libgeos-dev \
      libicu-dev \
      libpng-dev \
      libproj-dev \
      libsqlite3-dev \
      libssl-dev \
      libudunits2-dev \
      libxt6 \
      make \
      pandoc \
      zlib1g-dev \
      libsodium-dev \
      pkg-config \
      cmake \
      libfontconfig1-dev \
      libfreetype6-dev \
      libharfbuzz-dev \
      libfribidi-dev \
      libgsl-dev \
      tcl8.6-dev \
      tk8.6-dev \
      libgit2-dev \
      default-jdk \
      libbz2-dev \
    && apt-get clean


# R packages
RUN Rscript -e 'install.packages("remotes")' && \
    Rscript -e 'remotes::install_version("renv", version = "1.0.7")'
    

COPY renv.lock renv.lock
RUN Rscript -e 'renv::restore()'


# install INLA
RUN Rscript -e 'install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)' \
    && Rscript -e 'install.packages("MatrixModels", type="source")' \
    && find /usr/local/lib/R/site-library/INLA/ -type f -perm -u+x -exec chmod a+x {} \;


# install KrigR
RUN export R_REMOTES_NO_ERRORS_FROM_WARNINGS="true" \
    && Rscript -e 'install.packages(c("devtools"), repos="https://cloud.r-project.org")' \
    && Rscript -e 'devtools::install_github("ErikKusch/KrigR", dependencies=FALSE)'


# Docker
RUN apt-get update -qy && \
    apt-get install -qy \
        docker.io \
        && \
    apt-get clean


ENTRYPOINT ["Rscript"]
CMD ["--help"]
