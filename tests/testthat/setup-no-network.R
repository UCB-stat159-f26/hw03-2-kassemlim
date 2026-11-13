# Block live requests for the whole test run: `fetch_url()` is the package's
# only network entry point, so any test that forgets to mock it fails loudly
# instead of contacting Tennis Abstract.
local_mocked_bindings(
  fetch_url = function(url) {
    stop("Live network requests are disabled in tests: ", url, call. = FALSE)
  },
  pause_before_retry = function(attempt) invisible(NULL),
  .env = teardown_env()
)
