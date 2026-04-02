enablePlugins(ScalaJSPlugin)

name         := "scorry"
version      := "0.1.0"
scalaVersion := "3.8.3"

scalaJSUseMainModuleInitializer := true

scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.NoModule) }

libraryDependencies += "com.raquo" %%% "laminar" % "17.2.1"
