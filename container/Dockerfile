ARG CONDA_VERSION=py312_24.7.1-0

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
        r-devtools=2.4.5 \
        r-remotes=2.4.2.1 \
        r-rcpp=1.0.12 \
        r-pbapply=1.7-2 \
        r-tidyverse=2.0.0 \
        r-terra=1.7-65 \
        r-sf=1.0-15 \
        && \
    /conda/bin/conda clean -afy

# Install other conda-available packages
RUN . /conda/etc/profile.d/conda.sh && \
    conda activate /conda/env && \
    conda install -c conda-forge --override-channels \
        r-rcpparmadillo=14.2.2-1 \
        r-iterators=1.0.14 \
        r-sp=2.1-3 \
        r-raster=3.6-26 \
        r-rgbif=3.8.0 \
        r-rnaturalearth=1.0.1 \
        r-rnaturalearthdata=1.0.0 \
        r-ncdf4=1.22 \
        r-epi=2.47.1 \
        r-png=0.1-8 \
        r-keyring=1.3.2 \
        r-codetools=0.2-19 \
        r-geometries=0.2-4 \
        r-jsonify=1.2.2 \
        r-rapidjsonr=1.2.0 \
        r-sfheaders=0.4.4 \
        r-countrycode=1.6.0 \
        r-geojsonsf=2.0.3 \
        r-r.devices=2.17.2 \
        r-dbi=1.2.3 \
        r-rlang=1.1.3 \
        r-glue=1.8.0 \
        r-withr=3.0.2 \
        r-fmesher=0.2.0 \
        r-rgdal=1.6-7 \
        r-doParallel=1.0.17 \
        r-foreach=1.5.2 \
        r-doSNOW=1.0.20 \
        r-automap=1.1-12 \
        r-fasterize=1.1.0 \
        r-stars=0.6-6 \
        r-ggplot2=3.5.1 \
        r-ggpubr=0.6.0 \
        r-tidyr=1.3.1 \
        r-viridis=0.6.5 \
        r-cowplot=1.1.3 \
        r-ggpmisc=0.6.1 \
        r-gridExtra=2.3 \
# Packages for capfitogen
        r-maptools=1.1-8 \
        r-dismo=1.3-16 \
        r-rgeos=0.6-4 \
        r-RJSONIO=1.3-1.9 \
        r-googleVis=0.7.3 \
        r-cluster=2.1.8 \
        r-ade4=1.7-22 \
        r-labdsv=2.1-0 \
        r-mclust=6.1.1 \
        r-clustvarsel=2.3.4 \
        r-randomForest=4.7-1.2 \
        r-modeltools=0.2-23 \
        r-lattice=0.22-6 \
        r-flexmix=2.3-19 \
        r-fpc=2.2-13 \
        r-vegan=2.6-8 \
        r-adegenet=2.1.10 \
# Packages for sdm
        r-dismo=1.3-16 \
        r-gbm=2.2.2 \
        r-rjava=1.0-11 \
        r-glmnet=4.1-8 \
        r-tree=1.0-44 \
        r-mda=0.5-5 \
        r-plotrix=3.8-4 \
        r-plotmo=3.6.4 \
        r-earth=5.3.4 \
        r-ranger=0.17.0 \
        r-shinybs=0.61.1 \
        r-crosstalk=1.2.1 \
        r-rsnns=0.4-17 \
        r-leaflet=2.2.2 \
        r-leaflet.providers=2.0.0 \
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
        "mmap", \
        "sdm", \
        "usdm" \
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
    Rscript -e 'devtools::install_github("ErikKusch/KrigR", ref="Development", dependencies=FALSE)' && \
    /conda/bin/conda clean -afy

# Install remaining sdm dependencies (should be none)
RUN . /conda/etc/profile.d/conda.sh && \
    conda activate /conda/env && \
    Rscript -e 'sdm::installAll() ' && \
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
