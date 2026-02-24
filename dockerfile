# Use rocker/tidyverse as base image
FROM rocker/tidyverse:latest

# Install system dependencies for PDF graphics and fmsb
RUN apt-get update && apt-get install -y \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libwebp-dev \
    libbz2-dev \
    pkg-config \
    git \
    && rm -rf /var/lib/apt/lists/*

# Set working directory inside the container
WORKDIR /home/rstudio/snv_benchmark

# Copy the project files into the container
COPY . .

# Install optional R packages (like fmsb)
RUN R -e "install.packages('fmsb', repos='https://cloud.r-project.org')"

# Default command: run the plotting script
CMD ["Rscript", "src/snv_plotting.r"]
