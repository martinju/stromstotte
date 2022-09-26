# 0 [format_daily_database.R] Format the manual  csv files of daily prices from the nordpool website (just once)
# 1 [estimate_model.R] estimate model on static dataset (updated monthly) (save this model)
# 2 [get_hourly_data.R] read daily data and append to existing HOURLY data + addend to existing daily data (every day at 13.15)
# 3 [forecast.R] load model and daily data and use these to forecast the rest of the month
# 4 save current estimated compensation to file
# 5 load updated daily data and current estimated compensation, apply it to tomorrows daily data and plot that

