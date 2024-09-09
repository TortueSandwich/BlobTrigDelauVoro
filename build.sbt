scalaVersion := "2.13.12"

name := "QuadEdgeTriangulation"
organization := "blob"
version := "1.2"

githubOwner := "TortueSandwich"
githubRepository := "BlobTrigDelauVoro"
githubTokenSource := TokenSource.GitConfig("github.token")
resolvers += Resolver.githubPackages("TortueSandwich", "BlobTrigDelauVoro")
