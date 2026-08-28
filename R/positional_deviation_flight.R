# Positional-deviation analysis for one radar/drone flight.
# Uses the PI-approved common-grid alignment preserved from the archived July implementation.
# This function is used for all behavior types (foraging, soaring, chasing, transiting, and cube)
# This function takes in a single flight (both radar and drone tracks) and returns a list of the following:
# Data: paired_data, rmse_data, bias_data, corrected_rmse_data(rmse with systematic bias removed)
# Plots: boxplot, scatterplot, offsetplot, yoffsetplot
# Flight ID Info (in a dataframe): flight id, flight date, flight time

# note that although it's called "foraging" function and there a references to foraging throughout this file,
# it has been re-purposed to work for all flight types (universally adapted)

analyze_positional_deviation_flight <- function(
    radar_df,
    drone_df,

    offset_GPS,
    radar_y_offset,

    foraging_id,
    foraging_date,
    foraging_time,

    drone_indices,

    start_time_str,
    end_time_str,

    radar_type
) {

  FT_TO_M <- 0.3048
  options(digits = 15)

  ########################################
  # Optional Histogram
  rbind.data.frame (calculate_stats_mph_to_mps(drone_df, "speed.mph.", "Flight X (XX-XXpm)"))
  #hist(drone_df$speed.mph.*0.44704) #X m/s [mode]

  ############################################################
  # Flight analysis code


  #################################Set flight-specific parameters#################################
  # Data Input
  radar_9090_df <- radar_df %>%
    filter(str_detect(TrackId, paste0("^A:", radar_type, ":")))


  radar_data <- radar_9090_df      # Radar data for TrackId 9090
  drone_data <- drone_df    # Drone data for flight

  # Convert drone altitude from feet to meters (assumes radar measures altitude in meters above sea level)
  drone_data <- drone_data %>%
    mutate(
      altitude_ft = as.numeric(altitude_above_seaLevel.feet.),  # Convert altitude to numeric
      altitude_m = altitude_ft * FT_TO_M  # Convert feet to meters
    )

  #################################Drone Data Processing#################################
  # Manually remove non-foraging portion of drone track (adjust indices as needed)
  drone_foraging_df <- drone_data[drone_indices, ]
  foraging0x_final_drone <- drone_foraging_df

  #################################Radar Data Processing#################################
  # Extract variables of interest from radar data
  radar_foraging_df <- radar_data %>%
    select(
      TrackId,
      SendTime,
      StartTime = trk_start_time_1,
      UpdateTime = update_time_1,
      XPos = long,
      YPos = lat,
      ZPos = alt,
      Speed = Speed__m_s_,
      Heading = heading,
      lat,
      long,
      alt
    )

  # Convert UpdateTime to POSIXct (turns string into datetime object) for accurate filtering
  radar_foraging_df <- radar_foraging_df %>%
    mutate(
      UpdateTime = as.POSIXct(UpdateTime, format="%m/%d/%Y %H:%M:%OS", tz="UTC")
    )

  # Define start and end times for filtering
  start_time <- as.POSIXct(start_time_str, format="%Y-%m-%d %H:%M:%OS", tz="UTC")
  end_time <- as.POSIXct(end_time_str, format="%Y-%m-%d %H:%M:%OS", tz="UTC")

  # Filter radar data based on UpdateTime
  radar_foraging_df <- radar_foraging_df %>%
    filter(UpdateTime >= start_time & UpdateTime <= end_time)

  # Store final radar foraging data
  foraging0x_final_radar <- radar_foraging_df  # For preservation of data

  #################################Generate Drone Spatial Data Frame#################################

  # Prepare data for plotting
  drone_plot_df <- data.frame(
    timestamp = as.POSIXct(drone_foraging_df$datetime.utc., format="%Y-%m-%d %H:%M:%S", tz="UTC"),
    longitude = as.numeric(drone_foraging_df$longitude),
    latitude = as.numeric(drone_foraging_df$latitude),
    altitude = as.numeric(drone_foraging_df$altitude_m)
  )

  # Calculate milliseconds elapsed from start time
  drone_plot_df <- drone_plot_df %>%
    mutate(
      milliseconds = rep(c(900, 100, 300, 500, 700), length.out = n()),
      milliseconds_elapsed = as.numeric(difftime(timestamp, start_time, units = "secs"))
    )

  # Rename columns to match what 'amt' package expects
  colnames(drone_plot_df) <- c("t_", "x_", "y_", "z_", "milliseconds", "milliseconds_elapsed")

  #################################Generate Radar Spatial Data Frame#################################

  # Prepare data for 3D plotting
  radar_plot_df <- data.frame(
    timestamp = radar_foraging_df$UpdateTime,
    longitude = radar_foraging_df$XPos,
    latitude = radar_foraging_df$YPos,
    altitude = radar_foraging_df$ZPos
  )

  # Adjust timestamps for GPS offset if necessary
  radar_plot_df <- radar_plot_df %>%
    mutate(
      timestamp = as.POSIXct(timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC") - offset_GPS,
      milliseconds = as.integer(round(1000 * (as.numeric(timestamp) %% 1)))
      #milliseconds = as.numeric(sub(".*\\.(\\d{3}).*", "\\1", timestamp))
    )

  # Rename columns to match what 'amt' package expects
  colnames(radar_plot_df) <- c("t_", "x_", "y_", "z_", "milliseconds")

  # Calculate milliseconds elapsed since start time
  radar_plot_df <- radar_plot_df %>%
    mutate(
      milliseconds_elapsed = as.numeric(difftime(t_, start_time, units = "secs"))
    )

  # Add TrackId for hover text
  radar_plot_df$TrackID <- radar_foraging_df$TrackId

  # Filter out data before the start time
  radar_plot_df <- radar_plot_df %>% filter(milliseconds_elapsed > 0)

  #################################Merge Drone and Radar Data by Millisecond#################################
  # makes it so we can compare every row by millisec like this:
  # t = 12.3s
  # radar position → (x, y, z)
  # drone position → (x, y, z)

  # Assign unique TrackID to each point
  drone_plot_df <- drone_plot_df %>%
    mutate(TrackID = row_number())

  # Define the time interval for alignment (every 100 ms)
  regular_times <- data.frame(
    timestamps = seq(start_time, max(drone_plot_df$t_), by = 0.1)
  ) %>%
    mutate(
      milliseconds_elapsed = as.numeric(difftime(timestamps, start_time, units = "secs"))
    )
  regular_times$milliseconds_elapsed <- round(regular_times$milliseconds_elapsed,1)


  # Prepare drone data for merging
  drone_merge_df <- drone_plot_df %>%
    select(milliseconds_elapsed, x_, y_, z_, TrackID) %>%

    # round time bins
    mutate(milliseconds_elapsed = round(milliseconds_elapsed, 1)) %>%

    # remove duplicates before approx()
    group_by(milliseconds_elapsed) %>%
    summarise(
      x_ = mean(x_, na.rm = TRUE),
      y_ = mean(y_, na.rm = TRUE),
      z_ = mean(z_, na.rm = TRUE),
      TrackID = first(TrackID),
      .groups = "drop"
    ) %>%

    mutate(TrackID = as.factor(TrackID))

  # Interpolate drone data to match regular times
  drone_interpolated <- full_join(regular_times, drone_merge_df, by = "milliseconds_elapsed") %>%
    arrange(milliseconds_elapsed) %>%
    mutate(
      x_ = approx(milliseconds_elapsed, x_, milliseconds_elapsed)$y,
      y_ = approx(milliseconds_elapsed, y_, milliseconds_elapsed)$y,
      z_ = approx(milliseconds_elapsed, z_, milliseconds_elapsed)$y
    ) %>%
    fill(TrackID, .direction = "downup")

  drone_interpolated$device <- "Drone"
  drone_interpolated$device <- as.factor(drone_interpolated$device)

  # Prepare radar data for merging
  radar_merge_df <- radar_plot_df %>%
    select(milliseconds_elapsed, x_, y_, z_, TrackID)
  radar_merge_df$milliseconds_elapsed <- round(radar_merge_df$milliseconds_elapsed, 1)
  radar_merge_df$TrackID <- as.factor(radar_merge_df$TrackID)

  # Forward-fill radar data to match regular times
  radar_interpolated <- full_join(regular_times, radar_merge_df, by = "milliseconds_elapsed") %>%
    arrange(milliseconds_elapsed) %>%
    fill(x_, y_, z_, .direction = "down")
  radar_interpolated$device <- "Radar"
  radar_interpolated$device <- as.factor(radar_interpolated$device)

  # Combine interpolated drone and radar data
  combined_data00 <- bind_rows(drone_interpolated, radar_interpolated)

  # Separate combined data into drone and radar datasets
  drone_final01 <- combined_data00 %>%
    filter(device == "Drone") %>%
    select(timestamps, milliseconds_elapsed, x_, y_, z_, TrackID, device)

  radar_final00 <- combined_data00 %>%
    filter(device == "Radar") %>%
    select(timestamps, milliseconds_elapsed, x_, y_, z_, TrackID, device)

  radar_final01 <- radar_final00 %>% na.omit()

  # Need to combine the drone and radar DFs in a way that we can only retain the paired points and other milliseconds are removed
  combined_data01 <- merge (radar_final01, drone_final01, by = "milliseconds_elapsed", all.x = TRUE)
  combined_data02 <- combined_data01 %>% na.omit()

  # Separate combined data into drone and radar datasets
  radar_final <- combined_data02 %>%
    filter(device.x == "Radar") %>%
    select(timestamps.x, milliseconds_elapsed, x_.x, y_.x, z_.x, TrackID.x, device.x)
  radar_final <- radar_final %>%
    rename(timestamps = timestamps.x, x_ = x_.x, y_ = y_.x, z_ = z_.x, TrackID = TrackID.x, device = device.x)

  drone_final <- combined_data02 %>%
    filter(device.y == "Drone") %>%
    select(timestamps.y, milliseconds_elapsed, x_.y, y_.y, z_.y, TrackID.y, device.y)
  drone_final <- drone_final %>%
    rename(timestamps = timestamps.y, x_ = x_.y, y_ = y_.y, z_ = z_.y, TrackID = TrackID.y, device = device.y)

  #################################Spatial Deviation Between Radar and Drone Points#################################
  # Ensure radar and drone data are in the same structure and transform to UTM for meter-based calculations

  # convert radar positions to UTM
  radar_final_sf <- radar_final %>%
    st_as_sf(coords = c("x_", "y_"), crs = 4326) %>%
    st_transform(crs = 32611) %>%
    mutate(
      X_m_radar = st_coordinates(.)[, 1],
      Y_m_radar = st_coordinates(.)[, 2] + radar_y_offset
    ) %>%
    st_drop_geometry()

  # convert drone positions to UTM
  drone_final_sf <- drone_final %>%
    st_as_sf(coords = c("x_", "y_"), crs = 4326) %>%
    st_transform(crs = 32611) %>%
    mutate(
      X_m_drone = st_coordinates(.)[, 1],
      Y_m_drone = st_coordinates(.)[, 2]
    ) %>%
    st_drop_geometry()

  # both radar and drone still have duplicate milliseconds_elapsed values, so the join is no longer one-to-one
  # need to fix this by doing the following:
  radar_final_sf <- radar_final_sf %>%
    group_by(milliseconds_elapsed) %>%
    summarise(
      X_m_radar = mean(X_m_radar, na.rm = TRUE),
      Y_m_radar = mean(Y_m_radar, na.rm = TRUE),
      z_ = mean(z_, na.rm = TRUE),
      .groups = "drop"
    )
  drone_final_sf <- drone_final_sf %>%
    group_by(milliseconds_elapsed) %>%
    summarise(
      X_m_drone = mean(X_m_drone, na.rm = TRUE),
      Y_m_drone = mean(Y_m_drone, na.rm = TRUE),
      z_ = mean(z_, na.rm = TRUE),
      .groups = "drop"
    )

  # merge radar and drone UTM coordinates based on milliseconds_elapsed
  paired_final_sf <- radar_final_sf %>%
    inner_join(
      drone_final_sf,
      by = "milliseconds_elapsed",
      suffix = c("_radar", "_drone")  # Use 'suffix' instead of 'suffixes'
    ) %>%
    mutate(
      dev_x = X_m_radar - X_m_drone,
      dev_y = Y_m_radar - Y_m_drone,
      dev_z = z__radar - z__drone,
      euclidean_distance = sqrt(dev_x^2 + dev_y^2 + dev_z^2),
      abs_dev_x = abs(dev_x),
      abs_dev_y = abs(dev_y),
      abs_dev_z = abs(dev_z)
    )

  ### Save systematic bias for later analytics ###
  bias_data <- data.frame(
    measure = c("bias_x", "bias_y", "bias_z"),
    bias = c(
      mean(paired_final_sf$dev_x, na.rm = TRUE),
      mean(paired_final_sf$dev_y, na.rm = TRUE),
      mean(paired_final_sf$dev_z, na.rm = TRUE)
    )
  )

  # Remove systematic bias from each axis
  paired_final_sf <- paired_final_sf %>%
    mutate(
      dev_x_corrected = dev_x - mean(dev_x, na.rm = TRUE),
      dev_y_corrected = dev_y - mean(dev_y, na.rm = TRUE),
      dev_z_corrected = dev_z - mean(dev_z, na.rm = TRUE),

      euclidean_distance_corrected =
        sqrt(dev_x_corrected^2 + dev_y_corrected^2 + dev_z_corrected^2)
    )

  paired_final_sf$ID <- as.factor(seq_len(nrow(paired_final_sf)))
  paired_final_sf$flight_id <- foraging_id

  rmse_x <- calculate_rmse(paired_final_sf$dev_x)
  rmse_y <- calculate_rmse(paired_final_sf$dev_y)
  rmse_z <- calculate_rmse(paired_final_sf$dev_z)
  rmse_euclidean <- calculate_rmse(paired_final_sf$euclidean_distance)

  rmse_data <- data.frame(
    measure = c("dev_x", "dev_y", "dev_z", "euclidean_distance"),
    RMSE = c(rmse_x, rmse_y, rmse_z, rmse_euclidean)
  )

  paired_final_sf_foraging0x <- paired_final_sf

  # RMSE after removing systematic bias
  rmse_x_corrected <- calculate_rmse(paired_final_sf$dev_x_corrected)
  rmse_y_corrected <- calculate_rmse(paired_final_sf$dev_y_corrected)
  rmse_z_corrected <- calculate_rmse(paired_final_sf$dev_z_corrected)
  rmse_euclidean_corrected <- calculate_rmse(
    paired_final_sf$euclidean_distance_corrected
  )

  corrected_rmse_data <- data.frame(
    measure = c(
      "dev_x_corrected",
      "dev_y_corrected",
      "dev_z_corrected",
      "euclidean_distance_corrected"
    ),
    RMSE = c(
      rmse_x_corrected,
      rmse_y_corrected,
      rmse_z_corrected,
      rmse_euclidean_corrected
    )
  )

  ################################################################################
  # ALL PLOTS
  ########################################

  # extra plot to check for dev_y over time
  p_yoff <- ggplot(paired_final_sf, aes(x = milliseconds_elapsed, y = dev_y)) +
    geom_point(alpha = 0.3) +
    geom_smooth(se = FALSE, color = "red") +
    theme_minimal()

  # extra plot to check where smallest rmse is (THIS SHOULD BE = TO YOUR OFFSET)
  lag_results <- data.frame()

  for (lag in seq(-10, 10, by = 0.5)) {
    radar_shifted <- radar_plot_df %>% mutate(milliseconds_elapsed_shifted = milliseconds_elapsed + lag)
    radar_merge_df <- radar_shifted %>% select(milliseconds_elapsed_shifted, x_, y_, z_, TrackID)

    colnames(radar_merge_df)[1] <- "milliseconds_elapsed"
    combined_test <- merge(radar_merge_df, drone_merge_df, by = "milliseconds_elapsed", all.x = TRUE) %>% na.omit()

    if(nrow(combined_test) > 10){
      rmse_test <- sqrt(mean((combined_test$y_.x - combined_test$y_.y)^2, na.rm = TRUE))
      lag_results <- rbind(lag_results, data.frame(lag = lag, rmse_y = rmse_test))
    }
  }

  p_off <- ggplot(lag_results, aes(x = lag, y = rmse_y)) + geom_line() + geom_point() + theme_minimal()

  #################################
  # plot to show deviation of position error (rmse)
  #################################
  custom_colors <- c(
    "X Error" = "#a6cee3",
    "Y Error" = "#1f78b4",
    "Z Error" = "#b2df8a",
    "Euclidean Error" = "#33a02c"
  )

  # reshape data to long format
  deviation_data_long <- paired_final_sf %>%
    select(ID, dev_x, dev_y, dev_z, euclidean_distance) %>%
    pivot_longer(
      cols = -ID,
      names_to = "measure",
      values_to = "value"
    )

  deviation_data_long <- paired_final_sf %>%
    #filter(distance_to_radar <= 750) %>%   # if you want to limit outliers
    select(ID, dev_x, dev_y, dev_z, euclidean_distance) %>%
    pivot_longer(
      cols = -ID,
      names_to = "measure",
      values_to = "value"
    )

  deviation_data_long$measure <- recode(
    deviation_data_long$measure,
    "dev_x" = "X Error",
    "dev_y" = "Y Error",
    "dev_z" = "Z Error",
    "euclidean_distance" = "Euclidean Error"
  )

  deviation_data_long$measure <- factor(
    deviation_data_long$measure,
    levels = c("X Error", "Y Error", "Z Error", "Euclidean Error")
  )

  # calculate quartile and IQR for each measure
  quartiles <- deviation_data_long %>%
    group_by(measure) %>%
    summarise(
      Q1 = quantile(value, 0.25, na.rm = TRUE),
      Q3 = quantile(value, 0.75, na.rm = TRUE),
      IQR = IQR(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      lower_bound = Q1 - 1.5 * IQR,
      upper_bound = Q3 + 1.5 * IQR
    )

  # id and remove outliers
  deviation_data_long <- deviation_data_long %>%
    left_join(quartiles, by = "measure") %>%
    mutate(
      is_outlier = (value < lower_bound) | (value > upper_bound)
    )

  outliers <- deviation_data_long %>%
    filter(is_outlier)

  p_deviation03 <- deviation_data_long %>%
    plot_ly(
      x = ~measure,
      y = ~value,
      type = "box",
      color = ~measure,
      colors = custom_colors,

      text = ~paste("ID:", ID, "<br>Component:", measure, "<br>Value:", round(value, 3), " m"),
      hoverinfo = "text",
      boxpoints = "outliers",
      marker = list(opacity = 0.65, line = list(color = "black", width = 1)),
      line = list(color = "black", width = 1)
    ) %>%

    layout(
      margin = list(t = 100, b = 100, l = 100, r = 100),
      title = list(
        text = "Distribution of Radar–Drone Position Errors and Euclidean Error",
        font = list(size = 24)
      ),

      xaxis = list(
        title = "Error Component",
        titlefont = list(size = 18),
        tickfont = list(size = 16)
      ),
      yaxis = list(
        title = "Error Magnitude (m)",
        titlefont = list(size = 18),
        tickfont = list(size = 16)
      ),

      font = list(size = 18),
      showlegend = FALSE,

      #RMSE annotations
      annotations = list(
        list(
          x = "X Error",
          y = max(deviation_data_long$value[deviation_data_long$measure == "X Error"], na.rm = TRUE) + 2,
          text = paste0("RMSE: ", round(rmse_x, 2), " m"),
          showarrow = FALSE,
          font = list(size = 14)
        ),
        list(
          x = "Y Error",
          y = max(deviation_data_long$value[deviation_data_long$measure == "Y Error"], na.rm = TRUE) + 2,
          text = paste0("RMSE: ", round(rmse_y, 2), " m"),
          showarrow = FALSE,
          font = list(size = 14)
        ),
        list(
          x = "Z Error",
          y = max(deviation_data_long$value[deviation_data_long$measure == "Z Error"], na.rm = TRUE) + 3,
          text = paste0("RMSE: ", round(rmse_z, 2), " m"),
          showarrow = FALSE,
          font = list(size = 14)
        ),
        list(
          x = "Euclidean Error",
          y = max(deviation_data_long$value[deviation_data_long$measure == "Euclidean Error"], na.rm = TRUE) + 3,
          text = paste0("RMSE: ", round(rmse_euclidean, 2), " m"),
          showarrow = FALSE,
          font = list(size = 14)
        )
      )
    )

  p_deviation03

  #################################
  # plot to show Deviations as a Function of Distance From Radar
  #################################

  # physical radar coordinates
  radar_coords <- data.frame(
    X = -120.45366181,
    Y = 34.4567375903
  ) %>%
    st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
    st_transform(crs = 32611)

  # calculate dist from radar to drone points
  paired_final_sf <- paired_final_sf %>%
    st_as_sf(coords = c("X_m_drone", "Y_m_drone"), crs = 32611, remove = FALSE) %>%
    mutate(
      distance_to_radar = st_distance(geometry, radar_coords$geometry) %>% as.numeric()
    ) %>%
    st_drop_geometry()

  # reshape data for plotting
  deviation_distance_long <- paired_final_sf %>%
    select(distance_to_radar, dev_x, dev_y, dev_z, euclidean_distance) %>%
    pivot_longer(cols = starts_with("dev_") | starts_with("euclidean"),
                 names_to = "Deviation_Type", values_to = "Deviation_Value")

  deviation_distance_long <- deviation_distance_long %>%
    mutate(
      Deviation_Type = recode(
        Deviation_Type,
        "dev_x" = "X Error",
        "dev_y" = "Y Error",
        "dev_z" = "Z Error",
        "euclidean_distance" = "Euclidean Error"
      )
    )
  # set order of labels
  deviation_distance_long$Deviation_Type <- factor(
    deviation_distance_long$Deviation_Type,
    levels = c("X Error", "Y Error", "Z Error", "Euclidean Error")
  )

  # create plot of dev vs. distance from radar
  p_deviationbydistance <- ggplot(deviation_distance_long, aes(x = distance_to_radar, y = Deviation_Value, fill = Deviation_Type)) +
    geom_point(alpha = 0.7, shape = 21, color = "black", stroke = 0.5) +
    geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +

    facet_wrap(~Deviation_Type, scales = "free_y") +  # lets each error plot have its own yaxis limits
    #facet_wrap(~Deviation_Type, scales = "fixed") +    # all error plots set to same yaxis limits

    # # for fixed axis across all scatterplots/flights
    # facet_wrap(~Deviation_Type, scales = "free_y") +
    # facetted_pos_scales(
    #   y = list(
    #     #scale_y_continuous(limits = c(-40, 80)),   # X Error
    #     scale_y_continuous(
    #       limits = c(-40, 80),
    #       breaks = seq(-25, 75, by = 25),
    #     ),
    #     #scale_y_continuous(limits = c(-40, 80)),   # Y Error
    #     scale_y_continuous(
    #       limits = c(-40, 80),
    #       breaks = seq(-25, 75, by = 25),
    #     ),
    #     #scale_y_continuous(limits = c(-40, 80)),   # Z Error
    #     scale_y_continuous(
    #       limits = c(-40, 80),
    #       breaks = seq(-25, 75, by = 25),
    #     ),
    #     scale_y_continuous(limits = c(0, 85))     # Euclidean Error
    #   )
    # ) +

    theme_minimal() +
    labs(
      title = list(
        text = "Radar–Drone Position Error vs. Distance from Radar",
        font = list(size = 24)
      ),
      x = list(
        title = "Distance from Radar (m)",
        titlefont = list(size = 18),
        tickfont = list(size = 16)
      ),
      y = list(
        title = "Error (m)",
        titlefont = list(size = 18),
        tickfont = list(size = 16)
      )
    ) +
    scale_fill_manual(values = custom_colors) +
    theme(
      legend.position = "none",
      plot.margin = margin(t = 10, r = 15, b = 10, l = 10)
    )

  p_deviationbydistance

  ########################################
  # Return results
  ########################################
  return(
    list(
      paired_data = paired_final_sf,

      rmse_data = rmse_data,
      bias_data = bias_data,
      corrected_rmse_data = corrected_rmse_data,   # rmse with systematic bias removed

      boxplot = p_deviation03,
      scatterplot = p_deviationbydistance,
      offsetplot = p_off,
      yoffsetplot = p_yoff,

      flight_info = data.frame(
        flight_id = foraging_id,
        date = foraging_date,
        time = foraging_time
      )
      )
  )

}
