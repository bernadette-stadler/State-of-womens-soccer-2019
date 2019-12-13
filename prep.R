library(fs)

dir_create("raw-data")

dir_create("graphics")

dir_copy("raw-data", new_path = "State-of-womens-soccer/raw-data")

dir_copy("graphics", new_path = "State-of-womens-soccer/www")

file_copy("State-of-womens-soccer-2019.pdf", new_path = "State-of-womens-soccer/www/overview.pdf")