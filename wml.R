library(uuid)
library(httr)
library(jsonlite)

VERSION <- "2021-06-24"

getHeaders <- function(api_key, base_url) {
  headers <- c("cache-control"="no-cache", "Content-Type"="application/json")
  url <- stringr::str_interp("${base_url}/icp4d-api/v1/authorize")
  body <- stringr::str_interp('{"username":"admin","api_key":"${api_key}"}')
  r <- POST(
    url,
    add_headers(
      .headers = headers
    ),
    body=fromJSON(body), 
    encode="json"
  )
  token <- content(r)$token
  headers <- c("Authorization"=stringr::str_interp("Bearer ${token}"), "Content-Type"="application/json", "Accept"="application/json")
  return(headers)
}


createAsset <- function(file_name, file_path, space_id, api_key, base_url) {
  headers <- getHeaders(api_key)
  url <- stringr::str_interp("${base_url}/v2/assets")
  query <- list(version=VERSION, space_id=space_id)
  asset_meta <- stringr::str_interp('{"metadata": {"name": "${file_name}", "asset_type": "data_asset", "origin_country": "us", "asset_category": "USER"}, "entity": {"data_asset": {"mime_type": "application/octet-stream"}}}')
  r <- POST(
    url,
    add_headers(
      .headers = headers
    ),
    query=query,
    body=fromJSON(asset_meta), 
    encode="json"
  )
  
  
  asset_id <- content(r)$metadata$asset_id
  url <- stringr::str_interp("${base_url}/v2/assets/${asset_id}/attachments")
  attachment_meta <- stringr::str_interp('{"asset_type": "data_asset", "name": "${file_name}", "mime": "application/octet-stream"}')
  r <- POST(
    url,
    add_headers(
      .headers = headers
    ),
    query=query,
    body=fromJSON(attachment_meta), 
    encode="json"
  )
  
  attachment_id <- content(r)$attachment_id
  attachment_url <- content(r)$url1
  url <- stringr::str_interp("${base_url}${attachment_url}")
  r <- PUT(
    url,
    body=list(file = upload_file(file_path))
  )
  
  url <- stringr::str_interp("${base_url}/v2/assets/${asset_id}/attachments/${attachment_id}/complete")
  r <- POST(
    url,
    add_headers(
      .headers = headers
    ),
    query=query
  )
}


downloadAsset <- function(asset_uid, file_name, space_id, api_key, base_url) {
  headers <- getHeaders(api_key)
  url <- stringr::str_interp("${base_url}/v2/assets/${asset_uid}")
  query <- list(version=VERSION, space_id=space_id)
  r <- GET(
    url,
    add_headers(
      .headers = headers
    ),
    query=query
  )
  
  attachment_id <- content(r)$attachments[[1]]$id
  url <- stringr::str_interp("${base_url}/v2/assets/${asset_uid}/attachments/${attachment_id}")
  r <- GET(
    url,
    add_headers(
      .headers = headers
    ),
    query=query
  )
  attachment_signed_url <- content(r)$url
  
  url <- stringr::str_interp("${base_url}${attachment_signed_url}")
  r <- GET(url, write_disk(file_name, overwrite=TRUE))
}


listAssets <- function(space_id, api_key, base_url) {
  headers <- getHeaders(api_key)
  url <- stringr::str_interp("${base_url}/v2/asset_types/data_asset/search")
  query <- list(version=VERSION, space_id=space_id)
  body <- stringr::str_interp('{"query": "*:*"}')
  
  r <- POST(
    url,
    add_headers(
      .headers = headers
    ),
    query=query,
    body=fromJSON(body), 
    encode="json"
  )
  
  l <- list()
  for (x in content(r)$results) {
    l[[ x$metadata$name ]] = x$metadata$asset_id
  }
  # names(l)
  
  return (l)
}


listDeployments <- function(space_id, api_key, base_url) {
  headers <- getHeaders(api_key)
  url <- stringr::str_interp("${base_url}/ml/v4/deployments")
  query <- list(version=VERSION, space_id=space_id)
  r <- GET(
    url,
    add_headers(
      .headers = headers
    ),
    query=query
  )
  
  l <- list()
  for (x in content(r)$resources) {
    l[[ x$metadata$name ]] = x$metadata$id
  }
  
  return (l)
}


createDeployment <- function(config, space_id, api_key, base_url) {
  deployments <- listDeployments(space_id=space_id, api_key=api_key)
  deployment_id <- deployments[['r helper deployment']]
  uuid <- UUIDgenerate()
  headers <- getHeaders(api_key)
  url <- stringr::str_interp("${base_url}/ml/v4/deployment_jobs")
  query <- list(version=VERSION, space_id=space_id)
  body <- stringr::str_interp('{
    "scoring": {"input_data": [{"values": [${config}]}]},
    "deployment": {"id": "${deployment_id}"},
    "space_id": "${space_id}",
    "name": "name_${uuid}"
  }')
  r <- POST(
    url,
    add_headers(
      .headers = headers
    ),
    query=query,
    body=body,
    encode="raw"
  )
}


score <- function(payload, deployment_name, space_id, api_key, base_url) {
  deployments <- listDeployments(space_id=space_id, api_key=api_key)
  deployment_id <- deployments[[deployment_name]]
  headers <- getHeaders(api_key)
  url <- stringr::str_interp("${base_url}/ml/v4/deployments/${deployment_id}/predictions")
  query <- list(version=VERSION)
  body <- stringr::str_interp('{"input_data": [{"values": [{"inputs": [{"Sepal.Length": 5.1, "Sepal.Width": 3.5, "Petal.Length": 1.4, "Petal.Width": 0.2}]}]}]}')
  r <- POST(
    url,
    add_headers(
      .headers = headers
    ),
    query=query,
    body=body,
    encode="raw"
  )
  
  return (toJSON(content(r)$predictions[[1]]$out))
}

