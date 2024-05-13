# cmdstan周りは https://github.com/JBris/stan-cmdstanr-docker/blob/main/Dockerfile （MIT license）を参考しました。
# cmdstanのバージョンを新しくしました

FROM rocker/tidyverse

# レポジトリの中のファイルを全てコンテナ内のRstudioがアクセスできる/home/rstudioに持っていく

COPY . /home/rstudio

RUN apt-get update -y \
  && apt-get install -y \
  mecab \
  libmecab-dev \
  mecab-ipadic-utf8 \
  git \
  # githubのためのsshキー周り
  openssh-server \
  openssh-client \
  # cmdstanなどのコンパイル用
  libglpk-dev \
  clang-3.6 \
  # 言語
  locales-all \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*
  
RUN Rscript -e 'install.packages("RMeCab", repos = "http://rmecab.jp/R")'
    
RUN mkdir -p $HOME/.R/ \ 
  && echo "CXX=clang++ -stdlib=libc++ -fsanitize=address,undefined -fno-sanitize=float-divide-by-zero -fno-omit-frame-pointer -fsanitize-address-use-after-scope -fno-sanitize=alignment -frtti" >> $HOME/.R/Makevars \
  && echo "CC=clang -fsanitize=address,undefined -fno-sanitize=float-divide-by-zero -fno-omit-frame-pointer -fsanitize-address-use-after-scope -fno-sanitize=alignment" >> $HOME/.R/Makevars \
  && echo "CFLAGS=-O3 -Wall -pedantic -mtune=native" >> $HOME/.R/Makevars \
  && echo "FFLAGS=-O2 -mtune=native" >> $HOME/.R/Makevars \
  && echo "FCFLAGS=-O2 -mtune=native" >> $HOME/.R/Makevars \
  && echo "CXXFLAGS=-O3 -march=native -mtune=native -fPIC" >> $HOME/.R/Makevars \
  && echo "MAIN_LD=clang++ -stdlib=libc++ -fsanitize=undefined,address" >> $HOME/.R/Makevars \
  && echo "rstan::rstan_options(auto_write = TRUE)" >> /home/rstudio/.Rprofile \
  && echo "options(mc.cores = parallel::detectCores())" >> /home/rstudio/.Rprofile

RUN Rscript -e 'Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1); install.packages("rstan")'

ENV CMDSTAN /usr/share/.cmdstan

RUN cd /usr/share/ \
  && wget --progress=dot:mega https://github.com/stan-dev/cmdstan/releases/download/v2.34.1/cmdstan-2.34.1.tar.gz \
  && tar -zxpf cmdstan-2.34.1.tar.gz && mv cmdstan-2.34.1 .cmdstan \
  && ln -s .cmdstan cmdstan && cd .cmdstan && echo "CXX = clang++" >> make/local \
  && make build

RUN Rscript -e 'install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))' 

RUN Rscript -e 'install.packages(c("quanteda", "future", "furrr", "shiny", "plumber", "plotly", "distrom", "stm", "keyATM", "grf"), dependencies = TRUE, Ncpus = parallel::detectCores())'


ENV LANG ja_JP.UTF-8
