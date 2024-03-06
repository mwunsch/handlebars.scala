How to deploy:

- Make changes
- Run `sbt all/test` and check to see that the tests pass.
- Update version in `build.sbt`
- `sbt all/updateVersion` (task will update the versions in `./README.md` and `addons/play-json/README.md`)
- `sbt all/publish`
  - Or, `sbt core/publishSigned play-json/publishSigned`
  - Or, `sbt all/publishLocal`
- Commit, tag, push, etc.


