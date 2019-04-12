# devtools::install_github("GuangchuangYu/hexSticker")
# install.packages("rsvg")
# install.packages("pdftools")


library(hexSticker)
sticker(
  subplot = "./man/figures/carot.png",
  s_x = 1,
  s_y = 1.19,
  s_width = 0.65,
  package = "CARoT",
  p_color = "white",
  p_size = 24,
  p_y = 0.46,
  spotlight = TRUE,
  l_alpha = 0.35,
  l_x = 1,
  l_y = 1,
  l_width = 5,
  l_height = 5,
  h_fill = "springgreen3",
  h_color = "springgreen4",
  dpi = 300,
  filename = "./man/figures/carot_hex.png"
)
