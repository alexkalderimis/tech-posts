---
title: Continuous deployment to Netlify from GitLab CI
description: My experience using GitLab CI/CD pipelines to deploy a static site to Netlify
tags: haskell, netlify, ci, gitlab
---

A smooth CI experience is as great a joy, as a buggy CI/CD pipeline is
psychic torture. When things work smoothly, you can forget about the
details and focus on the code, or in this case the writing; when things
don't work, it is a mess of poking around and speculative commits to re-run
broken builds.

Thankfully, after a bit of messing about, I'm happy to discover that GitLab
CI/CD and Netlify hosting is very definitively in the former camp - this post
is a little aide memoire for me, and hopefully a guide for any readers on how
to get things set up, without any of the intervening psychic torment.

## The basics

We are going to deploy a static site to Netlify, specifically
[this site](https://gitlab.com/alexkalderimis/tech-posts). To do so requires
building it first, and generating a directory that contains the static
site files.

This site is built using Hakyll, using the stack build tool, so I can build that
locally with:

```shell
stack exec site rebuild
```

You may be using a different tool - but the approach will be similar in spirit.

Once there is a directory (in my case `_site`), this needs to be uploaded to
Netlify. We can use the `netlify-cli` to do this, which we can install from
`npm`:

```shell
npm install netlify-cli -g
```

And then we can use it to initiate a manual deploy:

```shell
netlify deploy --prod --dir=_site
```

## Automating the process

To automate this process using GitLab CI/CD, we define the steps in a
`.gitlab-ci.yml` file. This is a very powerful format, and we can define
arbitrary jobs that we can arrange in any topology that makes sense for us.

The only tricky bit about this set-up is that we have two different languages,
with different build chains, caching needs, the lot. The only thing that
connects them is the `_site` directory, produced by the `build` stage. We will
model that as an artifact.

### Setup

We need two stages:

```yaml
stages:
  - build
  - deploy
```

### Building

We don't _need_ GHC installed - stack can do that, but having it around (and
stack of course) would speed things up. So we can define the build job to use a
specific Haskell image:

```yaml
stack-build:
  image: haskell:8.6.3
  stage: build
```

Haskell builds can take a while (the first time at least), so we will definitely
need caching to speed builds up. We are going to cache the stack build results,
as well as the built site itself:

```
  variables:
    STACK_ROOT: "${CI_PROJECT_DIR}/.stack-root"
  cache:
    key: ${CI_COMMIT_REF_SLUG}
    paths:
      - .stack-work/
      - .stack-root/
```

We use the variable to ensure the stack directories are in a predictable (and
cachable) location.

With that done, we can define the build script, which compiles the Hakyll
engine, and then builds the site:

```yaml
  script:
    - stack install --only-dependencies
    - stack build
    - stack exec site rebuild
```

The script produces a directory called `_site` that we want to use in the deploy
job, so lets name it as an artifact:


```yaml
  artifacts:
    paths:
      - _site
```

When this completes, we will know that our site builds, and the new build result
will be cached in `_site` and be available to subsequent jobs.

### Deployment

To deploy we will define a `"netlify"` job in the `"deploy"` stage:

```yaml
netlify: 
  stage: deploy
```

Since the `netlify-cli` is a node-js application, we will use a `node` image:

```yaml
  image: node
```

We don't need to actually build the site here, since the build job generated it,
and it is stored as an artifact. We can assume that this artifact will be
available to us by default, so we don't need to specify anything.

We will want caching though - specific to node, so that we can avoid installing
the CLI each time:

```yaml
  cache:
    key: ${CI_COMMIT_REF_SLUG}
    paths:
      - .npm/
```

We only want to deploy from the master branch:

```yaml
  only:
    - master
```

And then we can define our deployment steps:

```yaml
  script:
    - npm install netlify-cli -g
    - netlify deploy --site=$NETLIFY_SITE_ID --auth=$NETLIFY_AUTH_TOKEN --prod --dir=_site
```

Notice here that we need two values to pass to the `netlify-cli` tool in the CI
pipeline:

- `NETLIFY_SITE_ID`: A UUID that identifies the site. This can be found in the
  site settings on Netlify
- `NETLIFY_AUTH_TOKEN`: A token you obtain from the personal settings section on
  Netlify - it is best to use a new one for each use-case.

Once you get these two values, add them to the CI pipeline as custom variables.
You should mask the `NETLIFY_AUTH_TOKEN` to prevent it appearing in build logs,
but the `NETLIFY_SITE_ID` is not sensitive.

Finally I choose to associate my deployment with an environment in GitLab:

```yaml
  environment:
    name: prod
```

I added a new `prod` environment in the `Operations > Environments` settings
section, to enable me to track all deployments to this site. Since this job
is associated with an environment, you can choose to limit the `NETLIFY_*`
variables to just this `prod` environment - there is no need for them to be
available in the build job.

## Wrapping up

And done! You can see the complete pipeline definition [here](https://gitlab.com/alexkalderimis/tech-posts/blob/master/.gitlab-ci.yml). With this,
every push to master builds the site and pushes the generated files to Netlify.
