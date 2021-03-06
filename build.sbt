name := "rpgcharbot"
version := "0.1"
scalaVersion := "2.12.4"
resolvers += "jitpack" at "https://jitpack.io"
resolvers += Resolver.jcenterRepo
scalacOptions += "-Ypartial-unification"

wartremoverErrors in (Compile, compile) ++= Warts.unsafe

libraryDependencies ++= Seq(
  "ch.qos.logback"                 %  "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging"     %% "scala-logging"   % "3.7.2",
  "com.github.sys1yagi.mastodon4j" %  "mastodon4j"      % "1.4.0",
  "org.jsoup"                      %  "jsoup"           % "1.11.1",
  "co.fs2"                         %% "fs2-core"        % "0.10.0-M8",
  "com.github.pureconfig"          %% "pureconfig"      % "0.8.0",
  "org.tpolecat"                   %% "atto-core"       % "0.6.1-M7",
  "org.specs2"                     %% "specs2-core"     % "4.0.1" % "test",
  "net.dv8tion"                    %  "JDA"             % "3.3.1_306",
  "com.vdurmont"                   %  "emoji-java"      % "4.0.0"
)