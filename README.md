## Delaunay Triangulation / Voronoi Diagram

"Object oriented" fuonctionnal implementation of QuadEdge and guibas stolfi divide and conquer algorithm for delaunay triangulation

I mean it is more readable/understanable than pure C code (at least, i hope)

the paper : http://mesh.brown.edu/DGP/pdfs/Guibas-tog85.pdf

Todo rest of the readme :D


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
