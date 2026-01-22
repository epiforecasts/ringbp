# Contributing to ringbp

This outlines how to propose a change to ringbp.

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. 
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file. 
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. 
If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).
See our guide on [how to create a great issue](https://code-review.tidyverse.org/issues/) for more advice.

### Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("epiforecasts/ringbp", fork = TRUE)`.

*   Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.

*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). The style we follow for entries to `NEWS.md` is:

> [Information on the change]. Addresses #[issue number] by @[GitHub username] in #[PR number] and reviewed by @[GitHub username]. 

See the [`NEWS.md`](../NEWS.md) file for examples to follow.

#### What happens after submitting a PR?

* PRs are reviewed by the team before they are merged. The review process only begins after the continuous integration checks, which have to be manually triggered by a maintainer for first-time contributors, have passed.

* Usually, all the review conversations occur under the PR. The reviewer merges the PR when every issue has been resolved. Please use the "Resolve conversation" functionality in the GitHub web interface to indicate when a specific issue has been addressed, responding with a commit pointing to the change made where applicable.

* When a PR is ready to be merged, you may be asked to [rebase](https://www.atlassian.com/git/tutorials/merging-vs-rebasing) on the `main` branch. You can do this by checking out your branch and running `git rebase main`. If it is successful, your commits will be placed on top of the commit history of `main` in preparation for a merge. A rebase might result in some merge conflicts. Make sure that they are resolved, then push your changes to your branch again (using the `--force` option, that is, `git push -f`, if required).

*   A number of issues can cause the Github checks to fail. It can be helpful to safeguard against them by doing the following:
  *   Check that there are no linting issues by running `lintr::lint_package()`.
  *   Run `devtoools::check()` to check for wider package issues like mismatching documentation, etc. (this currently requires a fair bit of time/computation).
  *   (Optional) Turn on continuous integration with Github Actions on your forked repository.

* On a case-by-case basis, you may be asked to increment the package version both in the `NEWS.md` and
`DESCRIPTION` files. Please do not do this unless you're asked to. We follow the [Tidyverse package versioning guide](https://r-pkgs.org/lifecycle.html). You can run `usethis::use_version()` to automatically
make the changes for you interactively.

### Code style

*   New code should follow the tidyverse [style guide](https://style.tidyverse.org). 
    You can use [Air](https://posit-dev.github.io/air/) to apply this style, but please don't restyle code that has nothing to do with your PR.  

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.  

## Code of Conduct

Please note that the ringbp project is released with a
[Contributor Code of Conduct](https://github.com/epiforecasts/.github/blob/main/CODE_OF_CONDUCT.md). By contributing to this project you agree to abide by its terms.
