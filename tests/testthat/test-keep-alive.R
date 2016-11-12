context("keep-alive")

h <- new_handle()
handle_setopt(h, verbose=TRUE,
  debugfunction=function(type, msg) cat(readBin(msg, character())))
log1 <- capture.output(curl_fetch_memory(httpbin("get"), handle=h))
log2 <- capture.output(curl_fetch_memory(httpbin("get"), handle=h))

test_that("The connection was kept alive", {
  expect_false(any(grepl("Re-using existing connection!", log1)))
  expect_true(any(grepl("Re-using existing connection!", log2)))
})

if (suppressMessages(require(RCurl))) {
  d <- debugGatherer()
  h2 <- getCurlHandle(verbose=TRUE, debugfunction=d$update)
  req3 <- getURL("https://httpbin.org/get", curl=h2)
  log3 <- d$value()["text"]
  d$reset()
  req4 <- getURL("https://httpbin.org/get", curl=h2)
  log4 <- d$value()["text"]

  test_that("RCurl keeps alive", {
    expect_false(grepl("Re-using existing connection!", log3))
    expect_true(grepl("Re-using existing connection!", log4))
  })
}
