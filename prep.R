library(fs)

dir_create("raw-data")

dir_create("graphics")

dir_copy("raw-data", new_path = "State-of-womens-soccer/raw-data")

dir_copy("graphics", new_path = "State-of-womens-soccer/www")

