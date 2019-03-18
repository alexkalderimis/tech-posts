---
title: Typed Components
published: false
description: 
tags: haskell, dependency-injection, clojure, 
---

``` haskell

data System xs where
  Empty :: System '[]
  System :: t -> System ts -> System (t ': ts)

```
