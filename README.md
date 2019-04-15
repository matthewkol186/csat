# Coverage Survey Analysis Tool
Repository for R code + documentation associated with the Coverage Survey Analysis Tool, produced by The Task Force for Global Health for use by the World Health Organization. Check out the web tool where this is deployed at https://coverage.securedatakit.com/.

## Testing
We use `usethis::testthat` for our unit testing. To create a new function, please run `usethis::use_r("foo")` from the home directory to generate a new function file in the `R/` directory. Then, to create an accompanying testing file, run `usethis::use_test("foo")` to generate an accomapnying test file. Once finished writing the test, you can simply run `devtools::test()` from the command line to visualize the output of the unit tests that have been run.

<img src="http://www.taskforce.org/wp-content/uploads/2016/07/TFGH_logo.jpg" alt="Task Force for Global Health Logo" width="150">
