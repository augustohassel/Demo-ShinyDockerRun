FROM augustohassel/r-shiny

# Cargo librerias
RUN R --slave -e "install.packages('remotes')"
RUN R --slave -e "remotes::install_github('rstudio/httpuv')"

# Llevo la aplicacion
RUN mkdir -p /srv/shiny-server
RUN rm -rf /srv/shiny-server/*
COPY app /srv/shiny-server/app

# Modifico la configuracion
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Creo el ejecutable
COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod +x /usr/bin/shiny-server.sh

RUN sudo usermod -a -G staff shiny

RUN mkdir -p /home/shiny/paquetes
COPY paquetes.R /home/shiny/paquetes/

RUN Rscript /home/shiny/paquetes/paquetes.R

EXPOSE 8080

CMD ["/usr/bin/shiny-server.sh"]
