FROM rocker/tidyverse:4.2.1
RUN install2.r rsconnect shiny reactable Rcpp shinyjs gtools httr jpeg openxlsx readxl kableExtra
RUN  R -e 'remotes::install_github("daqana/dqshiny")'
RUN  R -e 'remotes::install_github("StatistikStadtZuerich/zuericssstyle")'
WORKDIR /home/lima
COPY app.R app.R
COPY exportExcel.R exportExcel.R
COPY logo_stzh_stat_sw_pos_1.png logo_stzh_stat_sw_pos_1.png
COPY DataLoad.R DataLoad.R
COPY sszThemeShiny.css sszThemeShiny.css
COPY Titelblatt.xlsx Titelblatt.xlsx
COPY deploy.R deploy.R
CMD Rscript deploy.R