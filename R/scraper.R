
searchIkea = function(query){

  #### first retrieve N pages on the first page
  baselink = paste0(
    "http://www.ikea.com/nl/nl/search/?query=",
    query ,
    "&pageNumber="
  )
  firstpagelink = paste0(baselink , "1")

  flog.info("SEARCH %s", firstpagelink)
  out = read_html(firstpagelink)
  tmp =  html_nodes(out, xpath = '//a/@href') %>% html_text()
  pages = tmp[stringr::str_detect(tmp, "pageNumber")]

  NPAGES = pages %>% str_extract("\\d+") %>% as.numeric() %>% max()

  flog.info("npages %s", NPAGES)

  flog.info("loop over pages")
  #### loop over the search pages
  if(NPAGES > 0){
    iter = 1:NPAGES
    purrr::map_df(iter, searchIkeaOnePage, baselink, query)
  }
}


searchIkeaOnePage = function(iter, baselink, query)
{
  link = paste0(baselink,iter)
  out = read_html(link)

  lijst =  html_nodes(out, xpath = '//a/@href') %>% html_text()
  producten = str_detect(lijst , "catalog/products")
  bvtab = str_detect(lijst , "bvtab")
  geldigeproductlinks = lijst[producten & !bvtab]

  outframe = data.frame()
  for( j in 1:length(geldigeproductlinks))
  {
    link = paste0("http://www.ikea.com/", geldigeproductlinks[j])
    out = read_html(link)

    ## retrieve image
    img = html_nodes(
      out,
      xpath = '//img[@id="productImg"]/@src'
    ) %>%
      html_text()

    imagefile = str_sub(img,24,str_length(img))

    tryCatch(
      download.file(
        paste0("http://www.ikea.com", img),
        destfile = paste0("images/", imagefile),
        quiet = TRUE
      ),
      error=function(e)  flog.error("No such file")
    )

    ## retrive breadCrumbs
    breadCrumbs = html_nodes(
      out,
      xpath = '//ul[@id="breadCrumbs"]'
    ) %>%
      html_text() %>%
      cleanstring() %>%
      str_split("/")
    ## Neem 4e breadcrumb als ie niet leeg is anders 3e
    type4 = ifelse(is.na(breadCrumbs[[1]][4]), breadCrumbs[[1]][3], breadCrumbs[[1]][4])
    type3 = ifelse(is.na(breadCrumbs[[1]][3]), breadCrumbs[[1]][2], breadCrumbs[[1]][3])
    ## add results to output data frame
    outframe = bind_rows(
      outframe,
      data.frame(link,type3, type4,imagefile, stringsAsFactors = FALSE)
    )
  }
  flog.info("search page %s processed", iter)
  outframe
}

cleanstring <- function(x){
  stringr::str_replace_all(x,"\r","") %>%
    stringr::str_replace_all("\t","") %>%
    stringr::str_replace_all("\n","")
}

