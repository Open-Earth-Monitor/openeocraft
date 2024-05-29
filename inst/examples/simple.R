library(openeo)

con <- connect("http://127.0.0.1:4659")
p <- processes()
login("rolf", "123456")
list_collections()

x <- p$save_result(
  data = 1,
  format = "rds"
)

x

compute_result(x, output_file = "~/ndvi.rds")

result <- readRDS("~/ndvi.rds")
result
