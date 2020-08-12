
# spectrometer-tests

This is a repo of integration tests against popular (or problematic) open-source repositories.

## Adding tests

- Add a project repo as a submodule

``` sh
git submodule add git@github.com:foo/bar.git repos/strategygroup/bar/
```

**Only** use ssh remotes for submodules

- Add a test to `test/IntegrationSpec.hs`, using the `repo` helper

See existing examples in `test/IntegrationSpec.hs`, and see docs for `Repo` and `Analysis` in `test/Repo.hs`

## Failing tests

Comment out failing tests (or use `skip` from `test/IntegrationSpec.hs`). For each failing test, add a `FIXME` comment detailing why the test fails, and/or the relevant error message.
