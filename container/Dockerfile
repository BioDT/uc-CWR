ARG CONDA_VERSION=py311_23.9.0-0

#########################################
# Base
#########################################
FROM docker.io/opensuse/leap:15.4 AS base

# Install general utilities:
#   - R depends on which
#   - R help needs less
#   - r-devtools needs tar etc
RUN zypper refresh && \
    zypper --non-interactive install \
        which less \
        tar gzip unzip \
        && \
    zypper clean --all


#########################################
# Conda environment
#########################################
FROM base AS conda

# Install conda
ARG CONDA_VERSION
RUN curl https://repo.anaconda.com/miniconda/Miniconda3-$CONDA_VERSION-Linux-x86_64.sh -o conda.sh && \
    bash conda.sh -b -p /conda && \
    rm conda.sh && \
    /conda/bin/conda clean -afy

# Create base R environment
ARG R_VERSION
RUN . /conda/etc/profile.d/conda.sh && \
    conda create -p /conda/env -c conda-forge --override-channels --no-default-packages \
        r-base=$R_VERSION \
        && \
    /conda/bin/conda clean -afy

# Install common conda-available packages
RUN . /conda/etc/profile.d/conda.sh && \
    conda activate /conda/env && \
    conda install -c conda-forge --override-channels \
        r-devtools \
        r-remotes \
        r-rcpp \
        r-pbapply \
        r-tidyverse \
        r-sf \
        r-terra \
        && \
    /conda/bin/conda clean -afy

# Install other conda-available packages
RUN . /conda/etc/profile.d/conda.sh && \
    conda activate /conda/env && \
    conda install -c conda-forge --override-channels \
        r-rcpparmadillo \
        r-iterators \
        r-sp \
        r-raster \
        r-rgbif \
        r-rnaturalearth \
        r-ncdf4 \
        r-epi \
        r-png \
        r-keyring \
        r-codetools \
        r-geometries \
        r-jsonify \
        r-rapidjsonr \
        r-sfheaders \
        r-countrycode \
        r-geojsonsf \
        r-inlabru \
        r-r.devices \
        r-dbi \
        r-rlang \
        r-glue \
        r-withr \
        r-fmesher \
        r-rgdal \
        r-doParallel \
        r-foreach \
        r-doSNOW \
        r-automap \
        r-fasterize \
        r-stars \
        r-ggplot2 \
        r-ggpubr \
        r-tidyr \
        r-viridis \
        r-cowplot \
        r-ggpmisc \
        r-gridExtra \
        # Packages for capfitogen
        r-sp \
        r-raster \
        r-maptools \
        r-dismo \
        r-rgdal \
        r-rgeos \
        r-RJSONIO \
        r-googleVis \
        r-cluster \
        r-ade4 \
        r-labdsv \
        r-mclust \
        r-clustvarsel \
        r-randomForest \
        r-modeltools \
        r-lattice \
        r-flexmix \
        r-fpc \
        r-vegan \
        r-adegenet \
        # Packages for INLA
        r-locfit \
        r-ash \
        r-multicool \
        r-pracma \
        r-hdrcde \
        r-ks \
        r-bitops \
        r-rainbow \
        r-RCurl \
        r-fds \
        r-deSolve \
        r-fda \
        r-jpeg \
        r-TeachingDemos \
        r-coda \
        r-mcmc \
        r-dotCall64 \
        r-rbibutils \
        r-MCMCpack \
        r-gtools \
        r-spam \
        r-maps \
        r-dfidx \
        r-Formula \
        r-lmtest \
        r-Rdpack \
        r-mnormt \
        r-Deriv \
        r-evd \
        r-fields \
        r-gsl \
        r-markdown \
        r-mlogit \
        r-rgl \
        r-sn \
        r-splancs \
        && \
    /conda/bin/conda clean -afy

# Set default CRAN repository
RUN echo 'options(repos=c(CRAN="https://cloud.r-project.org"))' > /conda/env/lib/R/etc/Rprofile.site

# Install non-conda-available packages
RUN . /conda/etc/profile.d/conda.sh && \
    conda activate /conda/env && \
    Rscript -e 'install.packages(c( \
        "blockCV", \
        "ecmwfr", \
        "giscoR", \
        "tidyterra", \
        "geodata", \
        "usdm" \
        ))' && \
    /conda/bin/conda clean -afy

# Install INLA
# See https://www.r-inla.org/download-install
RUN . /conda/etc/profile.d/conda.sh && \
    conda activate /conda/env && \
    Rscript -e 'install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)' && \
    # Bugfix: https://github.com/hrue/r-inla/issues/86#issuecomment-1837534618
    Rscript -e 'install.packages("MatrixModels", type="source")' && \
    # Fix permissions
    find /conda/env/lib/R/library/INLA/ -type f -perm -u+x -exec chmod a+x {} \; && \
    /conda/bin/conda clean -afy

# Install intSDM etc
RUN . /conda/etc/profile.d/conda.sh && \
    conda activate /conda/env && \
    Rscript -e 'devtools::install_github("PhilipMostert/PointedSDMs", dependencies=FALSE)' && \
    # Bugfix: Installing intSDM directly from github doesn't work so we install it first from cran
    Rscript -e 'install.packages("intSDM")' && \
    Rscript -e 'devtools::install_github("PhilipMostert/intSDM", dependencies=FALSE)' && \
    /conda/bin/conda clean -afy

# Install non-conda-available packages
RUN . /conda/etc/profile.d/conda.sh && \
    conda activate /conda/env && \
    Rscript -e 'install.packages(c( \
        "mmap", \
        "sdm" \
        ))' && \
    /conda/bin/conda clean -afy



# Install mraster
RUN . /conda/etc/profile.d/conda.sh && \
    conda activate /conda/env && \
    Rscript -e 'devtools::install_github("babaknaimi/mraster", dependencies=FALSE)' && \
    /conda/bin/conda clean -afy


# Install KrigR
RUN . /conda/etc/profile.d/conda.sh && \
    conda activate /conda/env && \
    export R_REMOTES_NO_ERRORS_FROM_WARNINGS="true" && \
    # dependencies=FALSE so that RcppArmadillo doesn't get reinstalled
    Rscript -e 'devtools::install_github("ErikKusch/KrigR", dependencies=FALSE)' && \
    /conda/bin/conda clean -afy

# ENTRYPOINT ["bash"]

# Clean files not needed runtime
RUN find -L /conda/env/ -type f -name '*.a' -delete -print && \
    find -L /conda/env/ -type f -name '*.js.map' -delete -print


#########################################
# Final container image
#########################################
FROM base

COPY --from=conda /conda/env/ /conda/env/

ENV PROJ_DATA=/conda/env/share/proj \
    PATH=/conda/env/bin:$PATH \
    R_KEYRING_BACKEND=env \
    LC_ALL=C.UTF-8

ENTRYPOINT ["Rscript"]
CMD ["--help"]
