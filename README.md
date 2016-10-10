# nyaa-nyaa-nyaa

<img align='right' src='https://cloud.githubusercontent.com/assets/1013641/19213315/00d26940-8da4-11e6-9f18-8b696d720d3e.gif' alt='凛ちゃん'>

> I, the anime notifier, notify and you shall watch. :cat2:

*nyaa-nyaa-nyaa* is an anime notifier to let users be able to focus on
watching anime without getting hassled by recording or looking for
anime episodes. *nyaa-nyaa-nyaa* will poll *somewhere*, and notify
when there comes a new episode.

## Instruction

Use [Stack](https://haskellstack.org) to build:

```
$ stack build
```

Install:

```
$ stack install
```

Run `nyaa` with a project directory. The directory should contain
`config.yaml` and `anime.yaml`. For the Yaml samples, please refer to
the [`config`](config) directory.

```
$ nyaa config/
$ stack exec nyaa -- config/
```

For instruction, run `nyaa` without any argument:

```
$ nyaa
$ stack exec nyaa
```

## License

[BSD3](LICENSE)
