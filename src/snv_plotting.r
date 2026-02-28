# ===========================================
# SNV Benchmarking Visualization Script
# Generates 14+ publication-ready figures with legends and descriptions
# ===========================================

# -----------------------
# Load Libraries
# -----------------------
library(stringr)
library(lubridate)
library(scales)
library(tidyverse)
if(requireNamespace("fmsb", quietly = TRUE)) library(fmsb)  # optional for radar chart

library(patchwork)

bench_raw <- tribble(
  ~Acronym,       ~version,  ~n_samples, ~infra,               ~walltime,     ~node_hours, ~cpu_time,        ~gpu_time, ~cpu_alloc,
  
  "MDx1-NGC-M",   "5.7",     7,          "queue/flexible",     "6h04m01s",    3.27,       "627h41m53s",    "0",         "per_process",
  "MDx1-NGC-S",   "5.7",     7,          "local/fixed",        "5h48m08s",    3.23,       "619h43m02s",    "0",         "192",
  "MDx2-NGC-M",   "5.7",     7,          "queue/flexible",     "4h24m38s",    1.91,       "366h06m41s",    "0",         "per_process",
  "MDx2-NGC-S",   "5.7",     7,          "local/fixed",        "3h58m54s",    1.89,       "362h33m24s",    "0",         "192",
  
  "PB1-GeG-M",    "4.6.0-1", 7,          "queue/flexible",     "0h25m40s",    1.84,       "169h40m48s",    "12h07m12s", "112",
  "PB2-GeG-M",    "4.6.0-1", 7,          "queue/flexible",     "0h23m19s",    1.72,       "163h21m54s",    "11h40m12s", "112",
  "NFCS1-GeG-M",  "3.7.0",   7,          "queue/flexible",     "6h30m09s",    22.39,      "2278h18m00s",   "0",         "112",
  "NFCS2-GeG-M",  "3.7.0",   7,          "queue/flexible",     "7h08m29s",    19.72,      "2362h42m00s",   "0",         "112",
  "NFCS3-GeG-M",  "3.7.0",   7,          "queue/flexible",     "7h25m24s",    22.78,      "2847h06m00s",   "0",         "112",
  "NFCS4-GeG-M",  "3.7.0",   7,          "queue/flexible",     "6h37m34s",    20.47,      "2263h48m00s",   "0",         "112",
  
  # ---- Corrected CPU times below ----
  "NFCS5-GeG-M",  "3.7.0",   7,          "queue/flexible",     "4h35m13s",    18.39,      "2179h24m00s",   "14h49m12s", "112",
  "NFCS6-GeC-M",  "3.7.0",   7,          "queue/flexible",     "5h18m44s",    22.21,      "1348h00m00s",   "0",         "96",
  "NFCS7-GeC-M",  "3.7.0",   7,          "queue/flexible",     "6h07m01s",    22.48,      "1451h06m00s",   "0",         "96",
  
  "NFCS8-NGC-M",  "3.7.1",   7,          "queue/flexible",     "4h53m12s",    10.08,      "1936h06m00s",   "0",         NA,
  "NFCS9-NGC-M",  "3.7.1",   7,          "queue/flexible",     "3h35m46s",    7.92,       "1521h54m00s",   "0",         "variable",
  "SC-NGC-M",     "3.7.1",   7,          "queue/flexible",     "0h16m28s",    1.92,       "368h51m12s",    "0",         "96",
  
  "DR-DOP-M",     "4.3.13",  7,          "on-premise server",  "1h06m00s",    NA,         "52h48m00s",              NA,        NA
)

# -----------------------
# Helper Function
# -----------------------
convert_to_hours <- function(x) {
  sapply(x, function(xi) {
    if(is.na(xi)) return(NA)
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
    is_gpu = ifelse(is.na(gpu_h), FALSE, gpu_h > 0),
    cpu_alloc_num = as.numeric(ifelse(cpu_alloc %in% c("per_process","variable"), NA, cpu_alloc))
  )

# Speedup relative to slowest
baseline <- max(bench$walltime_h, na.rm = TRUE)
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
    str_detect(Acronym, "DR")  ~ "ICA",
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
walltime <- ggplot(bench, aes(x = reorder(Acronym, walltime_h), y = walltime_h, fill = infra)) +
  geom_col(na.rm=TRUE) + coord_flip() +
  labs(title="Walltime Comparison", x="Pipeline", y="Walltime (hours)", fill="Infrastructure Type") +
  theme_paper
ggsave("figure_walltime.pdf", width=8, height=6)
ggsave("figure_walltime.png", width = 8, height = 6, dpi = 300)

# 2) Total Node Hours
ggplot(bench, aes(x = reorder(Acronym, node_h), y = node_h, fill = is_gpu)) +
  geom_col(na.rm=TRUE) + coord_flip() +
  labs(title="Total Node Hours (Compute Cost)", x="Pipeline", y="Node Hours", fill="GPU Used") +
  theme_paper
ggsave("figure_node_hours.pdf", width=8, height=6)

# 3) CPU vs GPU Time
ggplot(bench, aes(x = cpu_h, y = gpu_h, size=node_h, color=infra)) +
  geom_point(alpha=0.7, na.rm=TRUE) +
  labs(title="CPU vs GPU Time", x="CPU Hours", y="GPU Hours", color="Infrastructure", size="Node Hours") +
  theme_paper
ggsave("figure_cpu_vs_gpu.pdf", width=8, height=6)

# 4) Walltime per Sample
ggplot(bench, aes(x = reorder(Acronym, walltime_per_sample), y = walltime_per_sample, fill=infra)) +
  geom_col(na.rm=TRUE) + coord_flip() +
  labs(title="Walltime per Sample", x="Pipeline", y="Hours per Sample", fill="Infrastructure") +
  theme_paper
ggsave("figure_walltime_per_sample.pdf", width=8, height=6)

# 5) Speedup relative to slowest
speedup <- ggplot(bench, aes(x = reorder(Acronym, speedup), y = speedup)) +
  geom_col(fill="steelblue", na.rm=TRUE) + coord_flip() +
  labs(title="Speedup Relative to Slowest Pipeline", x="Pipeline", y="Speedup (×)") +
  theme_paper
ggsave("figure_speedup.pdf", width=8, height=6)
ggsave("figure_speedup.png", width = 8, height = 6, dpi = 300)

# 6) Infrastructure Effect (Queue vs Local)
ggplot(bench, aes(x = infra, y = walltime_h, fill=infra)) +
  geom_boxplot(na.rm=TRUE) +
  labs(title="Infrastructure Effect on Walltime", x="Infrastructure Type", y="Walltime (hours)", fill="Infra") +
  theme_paper
ggsave("figure_infra_effect.pdf", width=8, height=6)

# 7) Cost vs Turnaround
ggplot(bench, aes(x=node_h, y=walltime_h, color=is_gpu)) +
  geom_point(size=4, na.rm=TRUE) +
  labs(title="Cost vs Turnaround Time", x="Node Hours (Proxy for Cost)", y="Walltime (hours)", color="GPU Used") +
  theme_paper
ggsave("figure_cost_vs_time.pdf", width=8, height=6)

# 8) Efficiency
ggplot(bench %>% filter(!is.na(efficiency)), aes(x=reorder(Acronym, efficiency), y=efficiency, fill=infra)) +
  geom_col(na.rm=TRUE) + coord_flip() +
  labs(title="Parallel Efficiency", x="Pipeline", y="Efficiency (CPU hours / allocated CPU-hours)", fill="Infrastructure") +
  theme_paper
ggsave("figure_efficiency.pdf", width=8, height=6)

# 9) Projected Walltime for 100 samples
bench <- bench %>% mutate(walltime_100 = walltime_per_sample * 100)
ggplot(bench, aes(x=reorder(Acronym, walltime_100), y=walltime_100, fill=infra)) +
  geom_col(na.rm=TRUE) + coord_flip() +
  labs(title="Projected Walltime for 100 Samples", x="Pipeline", y="Walltime (hours)", fill="Infrastructure") +
  theme_paper
ggsave("figure_walltime_100_samples.pdf", width=8, height=6)

# 10) GPU Speedup Factor
bench <- bench %>% mutate(gpu_speedup = ifelse(is_gpu, cpu_h / gpu_h, NA))
ggplot(bench %>% filter(!is.na(gpu_speedup)), aes(x=reorder(Acronym, gpu_speedup), y=gpu_speedup, fill=infra)) +
  geom_col(na.rm=TRUE) + coord_flip() +
  labs(title="GPU Speedup Factor", x="Pipeline", y="CPU / GPU Walltime", fill="Infrastructure") +
  theme_paper
ggsave("figure_gpu_speedup.pdf", width=8, height=6)

# 11) Radar Plot
if("fmsb" %in% rownames(installed.packages())){
  radar_data <- bench %>%
    select(Acronym, walltime_h, node_h, cpu_h, gpu_h, walltime_per_sample) %>%
    replace_na(list(walltime_h=0, node_h=0, cpu_h=0, gpu_h=0, walltime_per_sample=0)) %>%
    column_to_rownames("Acronym")
  
  radar_norm <- as.data.frame(lapply(radar_data, function(x) x/max(x, na.rm=TRUE)))
  radar_norm <- rbind(rep(1,ncol(radar_norm)), rep(0,ncol(radar_norm)), radar_norm)
  pipeline_names <- rownames(radar_norm)[-c(1,2)]
  colors <- rainbow(length(pipeline_names))
  
  radarchart(radar_norm, axistype=1,
             pcol=colors, pfcol=adjustcolor(colors, alpha.f=0.2),
             plwd=2, cglcol="grey", cglty=1, axislabcol="grey", vlcex=0.8,
             title="Multi-metric Radar Plot of Pipelines")
  legend("topright", legend=pipeline_names, col=colors, lty=1, lwd=2, cex=0.8, bty="n")
  
  pdf("figure_radar_plot.pdf", width=8, height=8)
  radarchart(radar_norm, axistype=1,
             pcol=colors, pfcol=adjustcolor(colors, alpha.f=0.2),
             plwd=2, cglcol="grey", cglty=1, axislabcol="grey", vlcex=0.8,
             title="Multi-metric Radar Plot of Pipelines")
  legend("topright", legend=pipeline_names, col=colors, lty=1, lwd=2, cex=0.8, bty="n")
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

# 13) Cumulative Node Hours by Family
ggplot(bench, aes(x=family, y=node_h, fill=family)) +
  geom_bar(stat="identity", na.rm=TRUE) +
  labs(title="Cumulative Node Hours by Pipeline Family", x="Pipeline Family", y="Total Node Hours", fill="Pipeline Family") +
  theme_paper
ggsave("figure_cumulative_node_hours.pdf", width=6, height=6)

# 14) Node Hours vs Walltime
ggplot(bench, aes(x = walltime_h, y = node_h, color = infra, shape = is_gpu)) +
  geom_point(size = 4, na.rm=TRUE) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", na.rm=TRUE) +
  labs(title = "Node Hours vs Walltime", x = "Walltime (hours)", y = "Node Hours", color = "Infrastructure", shape = "GPU Used") +
  theme_paper
ggsave("figure_node_vs_walltime.pdf", width = 8, height = 6)

# 15) Node Hours vs CPU Hours
ggplot(bench, aes(x = cpu_h, y = node_h, color = infra, shape = is_gpu)) +
  geom_point(size = 4, na.rm=TRUE) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", na.rm=TRUE) +
  labs(title = "Node Hours vs CPU Hours", x = "CPU Hours", y = "Node Hours", color = "Infrastructure", shape = "GPU Used") +
  theme_paper
ggsave("figure_node_vs_cpu.pdf", width = 8, height = 6)

# 17) Combined Figures 

# Combine the plots vertically
combined <- walltime / speedup  

# Save combined figure
ggsave("figure_combined.pdf", combined, width = 10, height = 12)
ggsave("figure_combined.png", combined, width = 10, height = 12, dpi = 300)


library(ggplot2)
library(dplyr)
library(tidyr)


bar_data <- bench %>%
  select(Acronym, walltime_h, node_h, cpu_h, gpu_h) %>%
  pivot_longer(cols = -Acronym,
               names_to = "Resource",
               values_to = "Value")

ggplot(bar_data, aes(x = reorder(Acronym, Value), y = Value, fill = Resource)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Pipeline Resource Usage",
       x = "Pipeline",
       y = "Resource Usage (hours)",
       fill = "Resource Type") +
  theme_paper
ggsave("resources_group.pdf", width = 8, height = 6)
ggsave("resources_group.png", width = 8, height = 6, dpi = 300)

ggplot(bar_data, aes(x = reorder(Acronym, Value), y = Value)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  facet_wrap(~ Resource, scales = "free_x") +
  labs(title = "Pipeline Resource Usage by Type",
       x = "Pipeline",
       y = "Hours") +
  theme_paper
ggsave("resources_faceted.pdf", width = 8, height = 6)
ggsave("resources_faceted.png", width = 8, height = 6, dpi = 300)

ggplot(bar_data, aes(x = reorder(Acronym, Value), y = Value, fill = Resource)) +
  geom_col() +
  coord_flip() +
  labs(title = "Total Resource Composition per Pipeline",
       x = "Pipeline",
       y = "Total Hours",
       fill = "Resource Type") +
  theme_paper
ggsave("resources_stacked.pdf", width = 8, height = 6)
ggsave("resources_stacked.png", width = 8, height = 6, dpi = 300)


# ordered by walltime 


library(dplyr)
library(tidyr)
library(ggplot2)

# Pivot to long format
bar_data <- bench %>%
  select(Acronym, walltime_h, cpu_h) %>%
  pivot_longer(
    cols = -Acronym,
    names_to = "Metric",
    values_to = "Value"
  )

# Keep only the two metrics (optional)
bar_data_subset <- bar_data %>%
  filter(Metric %in% c("walltime_h", "cpu_h"))

# 1️⃣ Order pipelines by walltime
# This sets the same order in both facets
bar_data_subset$Acronym <- factor(
  bar_data_subset$Acronym,
  levels = bench$Acronym[order(bench$walltime_h)]
)

# 2️⃣ Order facets so Walltime is on the left
bar_data_subset$Metric <- factor(
  bar_data_subset$Metric,
  levels = c("walltime_h", "cpu_h"),
  labels = c("Walltime (hours)", "CPU time (hours)")
)

# 3️⃣ Plot faceted horizontal bars
ggplot(bar_data_subset, aes(x = Acronym, y = Value)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  facet_wrap(~ Metric, scales = "free_x") +
  labs(title = "Walltime and CPU Usage by Pipeline",
       x = "Pipeline",
       y = "Hours") +
  theme_paper

# 4️⃣ Save
ggsave("walltime_cpu_faceted.pdf", width = 8, height = 6)
ggsave("walltime_cpu_faceted.png", width = 8, height = 6, dpi = 300)




# ===========================================
# End of Script
# ===========================================
