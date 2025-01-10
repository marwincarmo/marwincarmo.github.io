# Code from https://www.jhelvy.com/posts/2021-03-25-customizing-distill-with-htmltools-and-css/#link-buttons-with-icons-text

# Generates <i class="icon"></i>
make_icon <- function(icon) {
  return(htmltools::tag("i", list(class = icon)))
}

make_icon_text <- function(icon, text) {
  return(htmltools::HTML(paste0(make_icon(icon), " ", text)))
}

icon_link <- function(icon = NULL, text = NULL, url = NULL) {
  if (!is.null(icon)) {
    text <- make_icon_text(icon, text)
  }
  return(htmltools::a(href = url, text, class = "icon-link"))
}