FROM ubuntu:latest
WORKDIR /usr/local/app

RUN mkdir /var/repo
COPY ./marutter_pubkey.asc /var/repo/

RUN apt update -y && apt install -y --no-install-recommends software-properties-common dirmngr
RUN cat /var/repo/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
RUN add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
RUN apt install -y gdebi-core qpdf devscripts ghostscript
RUN apt update -y && apt install -y r-base r-base-dev sudo

RUN apt install libcurl4-openssl-dev 
RUN Rscript -e "install.packages(\"pak\")"

COPY ./ /var/repo
RUN Rscript -e "pak::local_install_dev_deps(\"/var/repo\")"
RUN Rscript -e "options(repos = c(CRAN = \"https://cran.rstudio.com/\", pik = \"https://rse.pik-potsdam.de/r/packages\")); pak::pkg_install(c(\"lucode2\", \"covr\", \"madrat\", \"magclass\", \"citation\", \"gms\", \"goxygen\", \"GDPuc\"))"

RUN apt install -y python3 python3-dev python3-pip git

RUN /var/repo/fix-pip.sh
RUN python3 -m pip install pre-commit

RUN rm -rf /var/repo
