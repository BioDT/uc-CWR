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
        r-ggplot2 \
        r-lattice \
        r-DescTools \
        r-sp \
        r-raster \
        r-rgbif \
        r-rnaturalearth \
        r-ncdf4 \
        r-keyring \
        r-stringr \
        r-rgdal \
        r-doParallel \
        r-foreach \
        r-doSNOW \
        r-automap \
        r-lubridate \
        r-fasterize \
        r-stars \
        r-httr \
        r-ggpubr \
        r-tidyr \
        r-viridis \
        r-cowplot \
        r-ggpmisc \
        r-gridExtra \
        && \
    /conda/bin/conda clean -afy

# Install non-conda-available packages
RUN . /conda/etc/profile.d/conda.sh && \
    conda activate /conda/env && \
    Rscript -e 'install.packages(c( \
        "overlapping", \
        "ecmwfr", \
        "sdm", \
        "usdm" \
        ), repos="https://cloud.r-project.org")' && \
    /conda/bin/conda clean -afy

RUN . /conda/etc/profile.d/conda.sh && \
    conda activate /conda/env && \
    export R_REMOTES_NO_ERRORS_FROM_WARNINGS="true" && \
    # dependencies=FALSE so that RcppArmadillo doesn't get reinstalled
    Rscript -e 'devtools::install_github("ErikKusch/KrigR", dependencies=FALSE)' && \
    /conda/bin/conda clean -afy

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
