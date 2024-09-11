## Delaunay Triangulation / Voronoi Diagram

Todo :D


also TODO : API

## Usage

add to ``sbt build`` :

```scala
libraryDependencies += "blob" % "quadedgetriangulation_2.13" % "1.+"
```

You might need to add a resolver to this repo:

see this repo : https://github.com/djspiewak/sbt-github-packages

in project/plugins.sbt:
```scala
addSbtPlugin("com.codecommit" % "sbt-github-packages" % "0.5.2")
```

in build.sbt:
```scala
githubTokenSource := // TokenSource.GitConfig("github.token") or TokenSource.Environment("GITHUB_TOKEN")
resolvers += Resolver.githubPackages("TortueSandwich", "BlobTrigDelauVoro")
libraryDependencies += "blob" % "quadedgetriangulation_2.13" % "1.+"
```

### Keywords

Delaunay Triangulation
Voronoi diagram
Divide and conquer
Triangulator
Functionnal programming
Guibas and Stolfi
