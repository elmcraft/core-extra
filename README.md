# core-extra

This is a large collection of utility-style functions pertaining to the Elm standard library (core).

## Upgrading

This library is a successor to all the elm-community/\*-extra packages related to elm/core, as well as several other community packages, such as GlobalWebIndex/cmd-extra, hayleigh-dot-dev/tuple-extra, stoeffel/set-extra, matthewsj/elm-ordering (albeit somewhat loosely that one).

Since this single library subsumes all of these, we recommend using some of the following to update.

### Step 1: Dependencies

```bash
npm i -g elm-json # if you don't already have it

git checkout -b core-extra-upgrade # Make sure you have a clean working tree, in case you want to revert

elm-json install elmcraft/core-extra@1 # v1 is a backwards compatible version
elm-json uninstall elm-community/array-extra elm-community/basics-extra elm-community/dict-extra elm-community/list-extra elm-community/maybe-extra elm-community/result-extra elm-community/string-extra GlobalWebIndex/cmd-extra hayleigh-dot-dev/tuple-extra stoeffel/set-extra

elm make src/Main.elm # or whatever else you use to build your application
# There shouldn't be any errors, but if there somehow are any, now would be a good time to fix them.
elm-test # Verify that everything went OK.

git commit -am "Completed step 1 in upgrade"
```

At this point you have core-extra installed. The second step is optional, and can be completed later, at your convenience.

### Step 2: Upgrading deprecated functions

Since we combined a large number of source libraries, we have done some work to create a more cohesive API. As such, some functions are deprecated in favour of other names and removed in 2.0.0. We have an automated script that will help you fix these automatically.

```bash
git status # make sure you have a clean working tree, in case you want to revert

npx elm-review --template elmcraft/core-extra/upgrade/2.0.0 --fix
# You can run this command multiple times if you'd like to abort and make changes manually

elm-json install elmcraft/core-extra@2 # upgrade to version 2 which no longer contains deprecated functions
```

## Contributing

Pull requests are welcome. You can expect some kind of response within 14 days.

If you are proposing a new function be added, please adhere to the following:

1. Include [documentation](http://package.elm-lang.org/help/documentation-format) and make sure your documentation has a code snippet demonstrating what the function does. We use [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples) in our CI setup which verifies that our example code is correct, so please take advantage of that.
2. Provide a detailed use case where your new function would be useful. Also, compare your new function to the best possible implementation that doesn't use your function.
3. Add tests.
4. To measure performance of different implementations, add benchmarks in the benchmarks directory.

## Contributors

This package is based on an amalgamation of a number of open source packages in the Elm community.
This is a list of contributors to those original packages:

@8n8, @abadi199, @acerempel, @ahstro, @akovari, @alex-tan, @AlienKevin, @andersk, @andys8, @annaghi, @Apanatshka, @armatures, @AyaMorisawa, @Bernardoow, @BrianHicks, @brianvanburken, @carolineartz, @cbenz, @ccapndave, @Chadtech, @cjoach, @cmditch, @cobalamin, @codedmart, @CristianIorga2000-ops, @CSchank, @cynic, @DanielCardonaRojas, @DanielJenkins, @dasch, @dependabot[bot], @eeue56, @EverybodyKurts, @frankschmitt, @fredcy, @gampleman, @garetht, @giladwo, @hsribei, @ianmackenzie, @iazel, @indique, @ingara, @jaapz, @jackfranklin, @jacob-tock, @Janiczek, @janjelinek, @jfmengels, @jhrcek, @jmpavlick, @jonboiser, @JordyMoos, @JoshuaHall, @JustusAdam, @jvoigtlaender, @jwoudenberg, @knuton, @kqr, @kraklin, @kuon, @kutyel, @loganmac, @lorenzo, @lue-bird, @m-shaka, @MartinSStewart, @martinsvalin, @matthewsj, @mdgriffith, @Menschenkindlein, @mgold, @miguel-vila, @Munksgaard, @myrho, @nibrivia, @nikolakasev, @opsb, @Orasund, @OvermindDL1, @paralax, @patrickdet, @Pilatch, @prikhi, @prozacchiwawa, @pzp1997, @r41d, @rehno-lindeque, @rlefevre, @robinheghan, @rogeriochaves, @seanhess, @shmookey, @sindikat, @skyqrose, @smucode, @sporto, @srid, @stil4m, @stoeffel, @tac-tics, @ThomasWeiser, @timjs, @tmcw, @tmsolber, @toastal, @TSFoster, @turboMaCk, @twopoint718, @txgruppi, @ursi, @wilcooo, @xtian, @ybakos, @z5h, @zwilias

For contributors to this repository specifically, see [contributors](https://github.com/elmcraft/core-extra/graphs/contributors).

## License

The overall project is licensed under the MIT license, but individual source files may have other (generally compatible) licenses. Please check the LICENSE file for details.
