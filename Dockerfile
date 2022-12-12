FROM rocker/tidyverse:4.0.3
RUN install2.r rsconnect shiny reactable Rcpp shinyjs gtools httr jpeg openxlsx readxl imager
RUN  R -e 'remotes::install_github("daqana/dqshiny")'
WORKDIR /home/lima
COPY app.R app.R
COPY exportExcel.R exportExcel.R
COPY logo_stzh_stat_sw_pos_1.png logo_stzh_stat_sw_pos_1.png
COPY prepareData.R prepareData.R
COPY sszDownload.R sszDownload.R
COPY sszTheme.css sszTheme.css
COPY Titelblatt.xlsx Titelblatt.xlsx
COPY deploy.R deploy.R
CMD Rscript deploy.R