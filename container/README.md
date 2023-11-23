# Container for crop wild relatives

## Usage on LUMI

### Pulling the image

Pull the pre-built image (replace `VERSION` with the desired version number):

    singularity pull --docker-login docker://ghcr.io/biodt/cwr:VERSION

This creates singularity image file `cwr_VERSION.sif`.

Note that the image is for now private, which means that login is required.
Follow [these instructions](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token#creating-a-personal-access-token-classic)
and create a classic personal access token with scope 'read:packages'.
Then, use your GitHub username and the created token in the login prompt of `singularity pull`.

### Running the container

Submit example job script

    sbatch -A <project> submit.lumi.sh


## Building a new image

Follow these instructions if you need to update the container image based on `Dockerfile`.

**First:** Update the image version number in `Makefile`.

Then, build and push the new image on a local machine following the instructions below.

### Ubuntu

If you don't have docker or podman, install using

    sudo apt install podman-docker

If using podman, define

    export BUILDAH_FORMAT=docker

Build the image

    make build

Login using GitHub Personal Access Token in order to be able to push:

    docker login ghcr.io

Push the image

    make push

For testing, you can also convert the local image to singularity:

    make singularity
