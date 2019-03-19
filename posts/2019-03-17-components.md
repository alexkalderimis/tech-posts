---
title: Typed Components
published: false
description: 
tags: haskell, dependency-injection, clojure, 
---

Dependencies come with a huge cost - they can lead systems to become brittle,
tightly coupled and hard to maintain. This leads developers to repeatedly
re-discover the power of dependency injection.

``` haskell

data System xs where
  Empty :: System '[]
  System :: t -> System ts -> System (t ': ts)

```
