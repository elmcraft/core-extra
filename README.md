# core-extra

This is a large collection of utility-style functions pertaining to the Elm standard library (core).

## Upgrading

(This will work once we release this library)

This library is a successor to all the elm-community/\*-extra packages, as well as several other community packages, such as GlobalWebIndex/cmd-extra, hayleigh-dot-dev/tuple-extra, stoeffel/set-extra, matthewsj/elm-ordering (albeit somewhat lossely that one).

Since this single library subsumes all of these, we recommend using some of the following to update.

### Step 1: Dependencies

```bash
npm i -g elm-json # if you don't already have it

git checkout -b core-extra-upgrade # make sure you have a clean working tree, in case you want to revert

elm-json install gampleman/core-extra@1 # v1 is a backwards compatible version
elm-json uninstall elm-community/array-extra elm-community/basics-extra elm-community/dict-extra elm-community/list-extra elm-community/maybe-extra elm-community/result-extra elm-community/string-extra GlobalWebIndex/cmd-extra hayleigh-dot-dev/tuple-extra stoeffel/set-extra

elm make src/Main.elm # or whatever else you use to build your application
# There shouldn't be any errors, but if there somehow are any, now would be a good time to fix them.
elm-test # Also, should work exactly the same. Please report any issues.

git commit -am "Completed step 1 in upgrade"
```

At this point you have core-extra installed. The second step is optional, and can be completed later, at your convenience.

### Step 2: Upgrading deprecated functions

Since we combined a large number of source libraries, we have done some work to create a more cohesive API. As such, some functions are deprecated in favour of other names and removed in 2.0.0. We have an automated script that will help you fix these automatically.

```bash
git status # make sure you have a clean working tree, in case you want to revert

npx elm-review --template gampleman/core-extra/elm-review-core-extra/preview --fix
# You can run this command multiple times, if you want to abort and fix stuff automatically

elm-json install gampleman/core-extra@2 # upgrade to version 2, that no longer contains deprecated functions
```

## Contributing

Pull requests are welcome. You can expect some kind of response within 14 days.

If you are proposing a new function be added, please adhere to the following..

1. Include [documentation](http://package.elm-lang.org/help/documentation-format) and make sure your documentation has a code snippet demonstrating what the function does. We use [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples) in our CI set up which verifies our examples that our example code is correct, so please take advantage of that.
2. Provide a detailed use case where your new function would be useful. Also, compare your new function to the best possible implementation that doesn't use your function.
3. Add tests.

If you are improving existing functions please demonstrate the performance gains in something like [Ellie](https://ellie-app.com/) and by using a benchmark library like [this one](https://github.com/elm-explorations/benchmark).

## Contributors

This package is based on an amalgamation of a number of open source packages in the Elm community.
This is a list of contributors to those original packages:

@8n8, @abadi199, @ahstro, @akovari, @alex-tan, @AlienKevin, @andersk, @andys8, @annaghi, @Apanatshka, @armatures, @AyaMorisawa, @Bernardoow, @BrianHicks, @brianvanburken, @carolineartz, @cbenz, @ccapndave, @Chadtech, @cjoach, @cmditch, @codedmart, @CristianIorga2000-ops, @CSchank, @cynic, @DanielCardonaRojas, @dasch, @dependabot[bot], @eeue56, @EverybodyKurts, @frankschmitt, @fredcy, @gampleman, @garetht, @giladwo, @hsribei, @ianmackenzie, @iazel, @indique, @jaapz, @jackfranklin, @jacob-tock, @Janiczek, @janjelinek, @jfmengels, @jhrcek, @jmpavlick, @jonboiser, @JordyMoos, @JoshuaHall, @JustusAdam, @jvoigtlaender, @jwoudenberg, @knuton, @kqr, @kraklin, @kuon, @kutyel, @loganmac, @lorenzo, @lue-bird, @m-shaka, @MartinSStewart, @martinsvalin, @matthewsj, @mdgriffith, @Menschenkindlein, @mgold, @miguel-vila, @Munksgaard, @myrho, @nibrivia, @nikolakasev, @Orasund, @OvermindDL1, @paralax, @patrickdet, @prikhi, @prozacchiwawa, @pzp1997, @r41d, @rehno-lindeque, @rlefevre, @robinheghan, @rogeriochaves, @seanhess, @shmookey, @sindikat, @skyqrose, @smucode, @sporto, @srid, @stil4m, @stoeffel, @tac-tics, @ThomasWeiser, @timjs, @tmcw, @tmsolber, @toastal, @TSFoster, @turboMaCk, @twopoint718, @txgruppi, @ursi, @wilcooo, @xtian, @ybakos, @z5h, @zwilias

## License

The overall project is licensed under the MIT license, but individual source files may have other (generally compatible) licenses. Please check the LICENSE file for details.
