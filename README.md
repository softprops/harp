# harp

a well tuned instrument for parsing [haproxy](http://haproxy.1wt.eu/) config files


# usage

Currently supports basic config file parsing into a typed structure called `harp.Config` which gives you query access to frontends, backends, defaults, listeners and options of each as Int, scala.concurrent.duration.Duration, and String values

```scala
import harp.Config
val parsed = Config.fromStr(configFileContents) 
val frontends = for {
  cfg <- parsed
  end  <- cfg.frontends  
} yield end.name

val backends = for {
  cfg <- parsed
  end  <- cfg.backends
} yield end.name

println("frontends")
frontends.foreach(println)
println("backends")
backends.foreach(println)
```

Doug Tangren (softprops) 2014
