# Combine CapturyLive CSV exports (one file per skeleton) into one table
# Works with Captury Live 1.0.x exports: semicolon-separated, 5 header rows,
# camera positions at the bottom of each file.

library(tidyverse)

# ---- 1. Point this at the folder that holds Alexia.csv, Bahar.csv, ... ----
folder <- "."
fps    <- 50   # from the file header: "Frame Rate;50"

# ---- 2. Reader for one Captury file ----
read_captury <- function(path) {
  lines <- readLines(path)
  names_row <- strsplit(lines[3], ";", fixed = TRUE)[[1]]   # joint names
  units_row <- strsplit(lines[5], ";", fixed = TRUE)[[1]]   # mm / deg

  # data rows start with a frame number; this drops the camera rows at the end
  data_lines <- lines[-(1:5)]
  data_lines <- data_lines[grepl("^[0-9]", data_lines)]
  df <- read_delim(I(data_lines), delim = ";", col_names = FALSE,
                   col_types = cols(.default = col_character()),
                   trim_ws = TRUE, progress = FALSE)

  # every X/Y/Z group starts at column 4, 7, 10, ...; keep positions (mm)
  starts <- seq(4, 187, by = 3)
  starts <- starts[units_row[starts] == "mm"]
  joints <- sub("Positions?$", "", names_row[starts])
  keep   <- !duplicated(joints)        # Captury lists RShoulder twice
  starts <- starts[keep]; joints <- joints[keep]

  frame <- as.integer(df[[1]])
  map2(starts, joints, \(s, j) tibble(
    frame = frame,
    joint = j,
    x = as.numeric(df[[s]]),
    y = as.numeric(df[[s + 1]]),
    z = as.numeric(df[[s + 2]])
  )) |>
    list_rbind() |>
    mutate(skeleton = tools::file_path_sans_ext(basename(path)),
           time_s = frame / fps, .before = 1)
}

# ---- 3. Read all skeletons and stack them ----
files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
files                                   # check: should list your 4 files

all_pos <- map(files, read_captury) |> list_rbind()
count(all_pos, skeleton)                # rows per skeleton

# optional: save the combined table
write_csv(all_pos, "all_skeletons_positions.csv")

# ---- 4. Example: distance between every pair of performers ----
# Y is vertical in Captury, so floor distance uses X and Z only
cog <- all_pos |>
  filter(joint == "CenterOfGravity") |>
  select(frame, time_s, skeleton, x, z)

pair_dist <- cog |>
  inner_join(cog, by = c("frame", "time_s"), suffix = c("_a", "_b"),
             relationship = "many-to-many") |>
  filter(skeleton_a < skeleton_b) |>
  mutate(pair = paste(skeleton_a, skeleton_b, sep = "–"),
         dist_m = sqrt((x_a - x_b)^2 + (z_a - z_b)^2) / 1000)

ggplot(pair_dist, aes(time_s, dist_m, colour = pair)) +
  geom_line() +
  labs(x = "Time (s)", y = "Floor distance (m)", colour = NULL)

# ---- 5. Example: where everyone moved in the room (top view) ----
ggplot(cog, aes(x, z, colour = skeleton)) +
  geom_path(alpha = 0.6) +
  coord_equal() +
  labs(x = "X (mm)", y = "Z (mm)", colour = NULL)
