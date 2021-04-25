---
title: Typed Components
published: false
description: 
tags: haskell, dependency-injection, clojure, 
---

Dependencies come with a huge cost - they can lead systems to become brittle,
tightly coupled and hard to maintain. This leads developers to repeatedly
re-discover the power of dependency injection.

Dependency injection takes very different forms depending on the software
ecosystem it is used within. The technical capabilities of a toolchain and the
social factors of the developer community both play an important role. But
whether we find a system defined in XML configuration files or specified in code
via convention or annotation, the common core of dependency injection is getting
the graph of components to be built for you, without having to pass all the
arguments around yourself.

Functional programmers often suggest that there are not really any design
patterns, there are just functions. This can often lead us to ignore the useful
work down in other ecosystems. Here I will introduce the excellent dependency
injection tool [component][component_] from the Clojure community, and suggest
a way to bring its design to Haskell, while preserving type-safety.

## An introduction to Components

The component library has two core concepts: _components_ and _systems_, which
are composed of components. Each component implements the `component/LifeCycle`
protocol, which basically says that they can be _started_ and _stopped_, in
order to acquire and release resources respectively. Systems, constructed with
`component/system-map` in the typical case, exist to group components together,
and they take the responsibility of wiring the various dependencies together, in
the correct order.

As an example, a web-application might be composed of a router, which needs a DB
to handle various requests, as well as a logger:

``` clojure
(def app
  [config]
  (-> (component/system-map
        :config config
        :db     (new-db)
        :logger (new-logger)
        :router (new-router))
      (component/system-using
        {:router {:database :db
                  :logger :logger}
        {:db {:

```

In component, the system is wired together from the flat map of parts.

``` haskell

data System xs where
  Empty :: System '[]
  System :: t -> System ts -> System (t ': ts)

```

[component_]: https://github.com/stuartsierra/component
