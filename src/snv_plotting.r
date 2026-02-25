# ===========================================
# SNV Benchmarking Visualization Script
# Generates 13+ publication-ready figures with legends and descriptions
# ===========================================

# -----------------------
# Load Libraries
# -----------------------
library(stringr)
library(lubridate)
library(scales)
library(tidyverse)
# Optional (for radar chart)
if(requireNamespace("fmsb", quietly = TRUE)) library(fmsb)

# -----------------------
# Dataset
# -----------------------
bench_raw <- tribble(
  ~Acronym, ~version, ~n_samples, ~infra, ~walltime, ~node_hours, ~cpu_time, ~gpu_time, ~cpu_alloc,
  "NGC-MDx1-M","5.7",7,"queue","33h56m06s",33.94,"601h09m10s","0","per_process",
  "NGC-MDx1-S","5.7",7,"local","31h02m25s",31.04,"617h50m38s","0","50",
  "NGC-MDx2-M","5.7",7,"queue","27h45m58s",27.75,"371h57m45s","0","per_process",
  "NGC-MDx2-S","5.7",7,"local","23h58m51s",23.96,"347h10m39s","0","50",
  "GeG-PB1-M","4.6.0-1",7,"queue","0h26m40s",1.84,"206","14.7","112",
  "GeG-PB2-M","4.6.0-1",7,"queue","0h23m19s",1.72,"193","13.8","112",
  "GeG-NFCS1-M","3.7.0",7,"queue","6h30m09s",22.39,"0h37m58s","0","112",
  "GeG-NFCS2-M","3.7.0",7,"queue","7h08m29s",19.72,"0h39m22s","0","112",
  "GeG-NFCS3-M","3.7.0",7,"queue","7h25m24s",22.78,"0h47m27s","0","112",
  "GeG-NFCS4-M","3.7.0",7,"queue","6h37m34s",20.47,"0h37m43s","0","112",
  "GeG-NFCS5-M","3.7.0",7,"queue","4h35m13s",18.39,"0h36m19s","147.12","112",
  "GeC-NFCS1-M","3.7.0",7,"queue","5h18m44s",22.21,"0h22m28s","0","96",
  "GeC-NFCS2-M","3.7.0",7,"queue","6h07m01s",22.48,"0h24m11s","0","96",
  "NGC-NFCS1-M","3.7.1",7,"queue","4h53m00s",10.08,"32h16m06s","0","variable",
  "NGC-NFCS2-M","3.7.1",7,"queue","3h35m00s",7.92,"25h21m54s","0","variable",
  "NGC-SC-S","3.7.1",7,"queue","0h14m00s",1.63,"4h12m19s","0","96"
)

# -----------------------
# Helper Functions
# -----------------------
convert_to_hours <- function(x) {
  sapply(x, function(xi) {
    if(str_detect(xi, "h|m|s")) {
      h <- as.numeric(str_extract(xi, "\\d+(?=h)"))
      m <- as.numeric(str_extract(xi, "\\d+(?=m)"))
      s <- as.numeric(str_extract(xi, "\\d+(?=s)"))
      h[is.na(h)] <- 0; m[is.na(m)] <- 0; s[is.na(s)] <- 0
      return(h + m/60 + s/3600)
    } else return(as.numeric(xi))
  })
}

# -----------------------
# Clean + Derived Metrics
# -----------------------
bench <- bench_raw %>%
  mutate(
    walltime_h = convert_to_hours(walltime),
    cpu_h      = convert_to_hours(cpu_time),
    gpu_h      = convert_to_hours(gpu_time),
    node_h     = as.numeric(node_hours),
    walltime_per_sample = walltime_h / n_samples,
    node_per_sample     = node_h / n_samples,
    is_gpu = gpu_h > 0,
    cpu_alloc_num = as.numeric(ifelse(cpu_alloc %in% c("per_process","variable"), NA, cpu_alloc))
  )

# Speedup relative to slowest pipeline
baseline <- max(bench$walltime_h)
bench <- bench %>% mutate(speedup = baseline / walltime_h)

# Efficiency metric
bench <- bench %>% mutate(
  efficiency = ifelse(!is.na(cpu_alloc_num), cpu_h / (walltime_h * cpu_alloc_num), NA)
)

# Family grouping
bench <- bench %>%
  mutate(family = case_when(
    str_detect(Acronym, "NGC") ~ "NGC",
    str_detect(Acronym, "GeG") ~ "GeG",
    str_detect(Acronym, "GeC") ~ "GeC",
    TRUE ~ "Other"
  ))

# -----------------------
# Theme
# -----------------------
theme_paper <- theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face="bold"),
    axis.text.x = element_text(angle=45, hjust=1)
  )

# =========================
# FIGURES
# =========================

# 1) Walltime Comparison
ggplot(bench, aes(x = reorder(Acronym, walltime_h), y = walltime_h, fill = infra)) +
  geom_col() + coord_flip() +
  labs(title="Walltime Comparison", x="Pipeline", y="Walltime (hours)", fill="Infrastructure Type") +
  theme_paper
ggsave("figure_walltime.pdf", width=8, height=6)
# Description: Shows how long each pipeline runs. Queue vs Local can affect runtime.

# 2) Total Node Hours
ggplot(bench, aes(x = reorder(Acronym, node_h), y = node_h, fill = is_gpu)) +
  geom_col() + coord_flip() +
  labs(title="Total Node Hours (Compute Cost)", x="Pipeline", y="Node Hours", fill="GPU Used") +
  theme_paper
ggsave("figure_node_hours.pdf", width=8, height=6)
# Analysis: Highlights pipelines with high compute cost; GPU usage indicated.

# 3) CPU vs GPU Time
ggplot(bench, aes(x = cpu_h, y = gpu_h, size=node_h, color=infra)) +
  geom_point(alpha=0.7) +
  labs(title="CPU vs GPU Time", x="CPU Hours", y="GPU Hours", color="Infrastructure", size="Node Hours") +
  theme_paper
ggsave("figure_cpu_vs_gpu.pdf", width=8, height=6)
# Analysis: Shows resource allocation patterns; GPU pipelines are faster but node-heavy.

# 4) Walltime per Sample
ggplot(bench, aes(x = reorder(Acronym, walltime_per_sample), y = walltime_per_sample, fill=infra)) +
  geom_col() + coord_flip() +
  labs(title="Walltime per Sample", x="Pipeline", y="Hours per Sample", fill="Infrastructure") +
  theme_paper
ggsave("figure_walltime_per_sample.pdf", width=8, height=6)
# Description: Useful for scaling estimates to more samples.

# 5) Speedup relative to slowest
ggplot(bench, aes(x = reorder(Acronym, speedup), y = speedup)) +
  geom_col(fill="steelblue") + coord_flip() +
  labs(title="Speedup Relative to Slowest Pipeline", x="Pipeline", y="Speedup (×)") +
  theme_paper
ggsave("figure_speedup.pdf", width=8, height=6)
# Analysis: Highlights the relative speed of pipelines.

# 6) Infrastructure Effect (Queue vs Local)
ggplot(bench, aes(x = infra, y = walltime_h, fill=infra)) +
  geom_boxplot() +
  labs(title="Infrastructure Effect on Walltime", x="Infrastructure Type", y="Walltime (hours)", fill="Infra") +
  theme_paper
ggsave("figure_infra_effect.pdf", width=8, height=6)
# Description: Boxplot shows impact of cluster type on walltime.

# 7) Cost vs Turnaround
ggplot(bench, aes(x=node_h, y=walltime_h, color=is_gpu)) +
  geom_point(size=4) +
  labs(title="Cost vs Turnaround Time", x="Node Hours (Proxy for Cost)", y="Walltime (hours)", color="GPU Used") +
  theme_paper
ggsave("figure_cost_vs_time.pdf", width=8, height=6)
# Analysis: Shows trade-off between runtime and compute cost.

# 8) Efficiency
ggplot(bench %>% filter(!is.na(efficiency)), aes(x=reorder(Acronym, efficiency), y=efficiency, fill=infra)) +
  geom_col() + coord_flip() +
  labs(title="Parallel Efficiency", x="Pipeline", y="Efficiency (CPU hours / allocated CPU-hours)", fill="Infrastructure") +
  theme_paper
ggsave("figure_efficiency.pdf", width=8, height=6)
# Analysis: Higher values indicate better CPU utilization.

# 9) Projected Walltime for 100 samples
bench <- bench %>% mutate(walltime_100 = walltime_per_sample * 100)
ggplot(bench, aes(x=reorder(Acronym, walltime_100), y=walltime_100, fill=infra)) +
  geom_col() + coord_flip() +
  labs(title="Projected Walltime for 100 Samples", x="Pipeline", y="Walltime (hours)", fill="Infrastructure") +
  theme_paper
ggsave("figure_walltime_100_samples.pdf", width=8, height=6)
# Description: Extrapolates runtime to larger studies.

# 10) GPU Speedup Factor
bench <- bench %>% mutate(gpu_speedup = ifelse(is_gpu, cpu_h / gpu_h, NA))
ggplot(bench %>% filter(!is.na(gpu_speedup)), aes(x=reorder(Acronym, gpu_speedup), y=gpu_speedup, fill=infra)) +
  geom_col() + coord_flip() +
  labs(title="GPU Speedup Factor", x="Pipeline", y="CPU / GPU Walltime", fill="Infrastructure") +
  theme_paper
ggsave("figure_gpu_speedup.pdf", width=8, height=6)
# Analysis: Shows how much faster GPU pipelines are relative to CPU-only.

# 11) Radar Plot (if fmsb installed)
if("fmsb" %in% rownames(installed.packages())){
  radar_data <- bench %>%
    select(Acronym, walltime_h, node_h, cpu_h, gpu_h, walltime_per_sample) %>%
    column_to_rownames("Acronym")
  radar_norm <- as.data.frame(lapply(radar_data, function(x) x/max(x, na.rm=TRUE)))
  radar_norm <- rbind(rep(1,ncol(radar_norm)), rep(0,ncol(radar_norm)), radar_norm)
  pdf("figure_radar_plot.pdf", width=8, height=8)
  radarchart(radar_norm, axistype=1,
             pcol=rainbow(nrow(radar_norm)-2),
             pfcol=rainbow(nrow(radar_norm)-2, alpha=0.2),
             plwd=2, cglcol="grey", cglty=1,
             axislabcol="grey", vlcex=0.8,
             title="Multi-metric Radar Plot of Pipelines")
  dev.off()
}

# 12) Heatmap
heat_data <- bench %>%
  select(Acronym, walltime_h, node_h, cpu_h, gpu_h) %>%
  column_to_rownames("Acronym") %>%
  as.matrix()
pdf("figure_heatmap.pdf", width=8, height=6)
heatmap(heat_data, Rowv=NA, Colv=NA, col=colorRampPalette(c("white","steelblue"))(50),
        scale="column", margins=c(8,8), main="Pipeline Resource Heatmap")
dev.off()
# Description: Visual comparison of resource intensity across pipelines.

# 13) Cumulative Node Hours by Family
ggplot(bench, aes(x=family, y=node_h, fill=family)) +
  geom_bar(stat="identity") +
  labs(title="Cumulative Node Hours by Pipeline Family", x="Pipeline Family", y="Total Node Hours", fill="Pipeline Family") +
  theme_paper
ggsave("figure_cumulative_node_hours.pdf", width=6, height=6)
# Analysis: Shows which pipeline family dominates total compute usage.


# 13) Node Hours vs Walltime
ggplot(bench, aes(x = walltime_h, y = node_h, color = infra, shape = is_gpu)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(
    title = "Node Hours vs Walltime",
    x = "Walltime (hours)",
    y = "Node Hours",
    color = "Infrastructure",
    shape = "GPU Used"
  ) +
  theme_paper

ggsave("figure_node_vs_walltime.pdf", width = 8, height = 6)

# 14 )Node Hours vs CPU Hours
ggplot(bench, aes(x = cpu_h, y = node_h, color = infra, shape = is_gpu)) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(
    title = "Node Hours vs CPU Hours",
    x = "CPU Hours",
    y = "Node Hours",
    color = "Infrastructure",
    shape = "GPU Used"
  ) +
  theme_paper

ggsave("figure_node_vs_cpu.pdf", width = 8, height = 6)

# ===========================================
# End of Script
# ===========================================