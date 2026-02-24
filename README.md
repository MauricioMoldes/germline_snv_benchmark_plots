# germline_snv_benchmark_plots

# Build the container
docker build -t snv_benchmark:rocker .

# Run and map results folder to host
docker run --rm -v $(pwd)/results:/home/rstudio/snv_benchmark/results snv_benchmark:rocker

