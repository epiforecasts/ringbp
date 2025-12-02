# Create a list of offspring distributions to run the ringbp model

Create a list of offspring distributions to run the ringbp model

## Usage

``` r
offspring_opts(community, isolated, asymptomatic = community)
```

## Arguments

- community:

  a `function`: a random number generating `function` that samples from
  the community (non-isolated) offspring distribution, the `function`
  accepts a single `integer` argument specifying the number of times to
  sample the offspring distribution (i.e. the length of the `function`
  output)

- isolated:

  a `function`: a random number generating `function` that samples from
  the isolated cases offspring distribution, the `function` accepts a
  single `integer` argument specifying the number of times to sample the
  offspring distribution (i.e. the length of the `function` output)

- asymptomatic:

  a `function`: a random number generating `function` that samples from
  the sub-clinical non-isolated cases offspring distribution, the
  `function` accepts a single `integer` argument specifying the number
  of times to sample the offspring distribution (i.e. the length of the
  `function` output). Will be specified as the same as the `community`
  offspring distribution if left unspecified

## Value

A `list` with class `<ringbp_offspring_opts>`.

## Details

If `asymptomatic` is not provided it will be specified as the same as
`community` meaning transmission of subclinical cases to be equal to
clinical cases unless specified otherwise.

## Examples

``` r
# Negative binomial offspring distributions with:
# Community R0 of 2.5 and dispersion of 0.16
# Isolated R0 of 0.5 and dispersion of 1
# Asymptomatic R0 of 2.5 and dispersion of 0.16
offspring_opts(
  community = \(n) rnbinom(n = n, mu = 2.5, size = 0.16),
  isolated = \(n) rnbinom(n = n, mu = 0.5, size = 1),
  asymptomatic = \(n) rnbinom(n = n, mu = 2.5, size = 0.16)
)
#> $community
#> function (n) 
#> rnbinom(n = n, mu = 2.5, size = 0.16)
#> <environment: 0x555c44862628>
#> 
#> $isolated
#> function (n) 
#> rnbinom(n = n, mu = 0.5, size = 1)
#> <environment: 0x555c44862628>
#> 
#> $asymptomatic
#> function (n) 
#> rnbinom(n = n, mu = 2.5, size = 0.16)
#> <environment: 0x555c44862628>
#> 
#> attr(,"class")
#> [1] "ringbp_offspring_opts"
```
