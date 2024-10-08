FROM rocker/r-ver:4.1.2

RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

WORKDIR /project
COPY renv.lock renv.lock
RUN ls
RUN cd usr
RUN cd home
RUN ls

# approach one
# ENV RENV_PATHS_LIBRARY renv/library

# approach two
# RUN mkdir -p renv
# COPY .Rprofile .Rprofile
# COPY renv/activate.R renv/activate.R
# COPY renv/settings.json renv/settings.json

# RUN R -e "renv::restore()"

# create a snapshot of the current project
COPY . .
RUN R -e 'renv::restore(lockfile = "./renv.lock")'
