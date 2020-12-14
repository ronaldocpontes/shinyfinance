FROM rocker/shiny-verse:latest
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libssh2-1-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*

# Add R packages to Docker cache
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e "install.packages(c('devtools', 'usethis', 'testthat','knitr','rmarkdown','rhub', 'xml2'))"
RUN R -e "install.packages(c('tidyverse','glue','lubridate','plotly','RColorBrewer','ggthemes'))"
RUN R -e "install.packages(c('golem','config','shiny','processx','attempt','htmltools','shinydashboard','shinyWidgets','DT'))"
RUN R -e "install.packages(c('tidyquant','BatchGetSymbols','xts','DEoptim','tseries','PerformanceAnalytics'))"

# Enable SSH
RUN apt update && apt install  openssh-server sudo -y
RUN useradd -rm -d /home/ubuntu -s /bin/bash -g root -G sudo -u 1000 test
RUN  echo 'test:test' | chpasswd
RUN service ssh start
EXPOSE 22

# Copy package to container
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone

# Install package and any remaining dependencies
RUN R -e "install.packages('remotes'); remotes::install_deps(dependencies = TRUE); remotes::install_local();"
RUN R -e "devtools::check(error_on = c('error'))"

CMD R -e "options('shiny.port'=$PORT,shiny.host='0.0.0.0');shinyfinance::run_app()"
