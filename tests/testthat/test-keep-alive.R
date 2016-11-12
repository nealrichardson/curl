context("keep-alive")

h <- new_handle(verbose=TRUE,
  debugfunction=function(type, msg) cat(readBin(msg, character())))
log1 <- capture.output(curl_fetch_memory(httpbin("get"), handle=h))
log2 <- capture.output(curl_fetch_memory(httpbin("get"), handle=h))

test_that("The connection was kept alive", {
  expect_false(any(grepl("Re-using existing connection!", log1)))
  expect_true(any(grepl("Re-using existing connection!", log2)))
})
