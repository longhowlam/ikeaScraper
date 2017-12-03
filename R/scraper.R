
searchIkea = function(query, imagedir = "images"){

  #### first retrieve N pages on the first page
  baselink = paste0(
    "http://www.ikea.com/nl/nl/search/?query=",
    query ,
    "&pageNumber="
  )
  firstpagelink = paste0(baselink , "1")

  futile.logger::flog.info("SEARCH %s", firstpagelink)
  out = xml2::read_html(firstpagelink)
  tmp =  rvest::html_nodes(out, xpath = '//a/@href') %>% rvest::html_text()
  pages = tmp[stringr::str_detect(tmp, "pageNumber")]

  NPAGES = pages %>% stringr::str_extract("\\d+") %>% as.numeric() %>% max()

  ## create directory for images if it does not already exists
  if(!file.exists(imagedir))
  {
    dir.create(imagedir)
  }

  futile.logger::flog.info("npages %s", NPAGES)

  futile.logger::flog.info("loop over pages")
  #### loop over the search pages
  if(NPAGES > 0){
    iter = 1:NPAGES
    purrr::map_df(iter, searchIkeaOnePage, baselink, query, imagedir)
  }
}


searchIkeaOnePage = function(iter, baselink, query, imagedir)
{
  link = paste0(baselink,iter)
  out = xml2::read_html(link)

  lijst =  rvest::html_nodes(out, xpath = '//a/@href') %>% rvest::html_text()
  producten = stringr::str_detect(lijst , "catalog/products")
  bvtab = stringr::str_detect(lijst , "bvtab")
  geldigeproductlinks = lijst[producten & !bvtab]

  outframe = data.frame()
  for( j in 1:length(geldigeproductlinks))
  {
    link = paste0("http://www.ikea.com/", geldigeproductlinks[j])
    out = xml2::read_html(link)

    ## retrieve image
    img = rvest::html_nodes(
      out,
      xpath = '//img[@id="productImg"]/@src'
    ) %>%
      rvest::html_text()

    imagefile = stringr::str_sub(img,24, stringr::str_length(img))

    tryCatch(
      download.file(
        paste0("http://www.ikea.com", img),
        destfile = paste0(imagedir, "/", imagefile),
        quiet = TRUE
      ),
      error=function(e)  futile.logger::flog.error("No such file")
    )

    ## retrive breadCrumbs
    breadCrumbs = rvest::html_nodes(
      out,
      xpath = '//ul[@id="breadCrumbs"]'
    ) %>%
      rvest::html_text() %>%
      cleanstring() %>%
      stringr::str_split("/")
    ## Neem 3e breadcrumb als ie niet leeg is anders 2e
    type3 = ifelse(is.na(breadCrumbs[[1]][3]), breadCrumbs[[1]][2], breadCrumbs[[1]][3])
    type2 = ifelse(is.na(breadCrumbs[[1]][2]), breadCrumbs[[1]][1], breadCrumbs[[1]][2])

    ## extract the price
    price = rvest::html_nodes(
      out,
      "#price1"
    ) %>% rvest::html_text() %>%
      cleanstring()

    ## name
    name = rvest::html_nodes(
      out,
      "#name"
    ) %>%
      rvest::html_text() %>%
        cleanstring()

    ## add all results to output data frame
    outframe = bind_rows(
      outframe,
      data.frame(link,name, type2, type3, price, imagefile, stringsAsFactors = FALSE)
    )
  }
  futile.logger::flog.info("search page %s processed", iter)
  outframe
}

cleanstring <- function(x){
  stringr::str_replace_all(x,"\r","") %>%
    stringr::str_replace_all("\t","") %>%
    stringr::str_replace_all("\n","")
}

