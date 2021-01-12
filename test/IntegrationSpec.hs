{-# LANGUAGE QuasiQuotes #-}

module IntegrationSpec
  ( spec,
  )
where

import qualified Data.Text as T
import Repo
import Path
import qualified Strategy.Bundler as Bundler
import qualified Strategy.Cargo as Cargo
import qualified Strategy.Carthage as Carthage
import qualified Strategy.Cocoapods as Cocoapods
import qualified Strategy.Leiningen as Leiningen
import qualified Strategy.Gomodules as Gomod
import qualified Strategy.Maven as Maven
import qualified Strategy.Maven.Pom.Closure as PomClosure
import qualified Strategy.NuGet.Nuspec as Nuspec
import qualified Strategy.NuGet.PackageReference as PackageReference
import qualified Strategy.NuGet.PackagesConfig as PackagesConfig
import qualified Strategy.Python.Pipenv as Pipenv
import qualified Strategy.Python.Setuptools as Setuptools
import qualified Strategy.Scala as Scala
import qualified Strategy.Rebar3 as Rebar3
import Test.Hspec hiding (pending)

pending :: Applicative f => f a -> f ()
pending _ = pure ()

spec :: Spec
spec = do
  let keycloak jdkPkg =
        repo
          Repo
            { repoRoot = [reldir|repos/maven/keycloak|],
              repoPrebuildScript = Just [relfile|repos/maven/keycloakbuild.sh|],
              repoDynamicNixDeps = ["maven", jdkPkg],
              repoAnalyses =
                [ Analysis
                    { analysisName = "maven",
                      analysisFinder = PomClosure.findProjects,
                      analysisFunc = Maven.getDeps,
                      analysisMkProject = Maven.mkProject [absdir|/|],
                      analysisProjects = map simpleTestProject [[reldir|.|], [reldir|boms|]]
                    }
                ]
            }

  keycloak "jdk8"
  keycloak "jdk11"

  repo
    Repo
      { repoRoot = [reldir|repos/ruby/vmfloaty|],
        repoPrebuildScript = Just [relfile|repos/ruby/vmfloatybuild.sh|],
        repoDynamicNixDeps = ["ruby", "bundler"],
        repoAnalyses =
          [ Analysis
              { analysisName = "bundler",
                analysisFinder = Bundler.findProjects,
                analysisFunc = Bundler.getDeps,
                analysisMkProject = Bundler.mkProject,
                analysisProjects = [simpleTestProject [reldir|.|]]
              }
          ]
      }

  repo
    Repo
      { repoRoot = [reldir|repos/ruby/rails|],
        repoPrebuildScript = Just [relfile|repos/ruby/railsbuild.sh|],
        repoDynamicNixDeps = ["ruby", "bundler", "libiconv", "zlib", "lzma", "rubyPackages.libxml-ruby", "rubyPackages.mysql2", "ncurses", "postgresql", "sqlite"],
        repoAnalyses =
          [ Analysis
              { analysisName = "bundler",
                analysisFinder = Bundler.findProjects,
                analysisFunc = Bundler.getDeps,
                analysisMkProject = Bundler.mkProject,
                analysisProjects = [simpleTestProject [reldir|.|]]
              }
          ]
      }

  let vault goPkg =
        repo
          Repo
            { repoRoot = [reldir|repos/gomod/vault|],
              repoPrebuildScript = Nothing,
              repoDynamicNixDeps = [goPkg],
              repoAnalyses =
                [ Analysis
                    { analysisName = "gomod",
                      analysisFinder = Gomod.findProjects,
                      analysisFunc = Gomod.getDeps,
                      analysisMkProject = Gomod.mkProject,
                      analysisProjects = [simpleTestProject [reldir|.|]]
                    }
                    -- FIXME: we don't support filepath replaces:
                    -- > replace also can be used to inform the go tooling of the relative or absolute on-disk location of modules in a multi-module project, such as:
                    -- >     replace example.com/project/foo => ../foo
                    {-
                    Analysis
                      { analysisFunc = Gomod.discover,
                        analysisProjects = [[reldir|.|]]
                      }
                    -}
                ]
            }

  vault "go"
  vault "go_1_15"

  let lein = filteredLein (const True)
      filteredLein :: (Leiningen.LeiningenProject -> Bool) -> Path Rel Dir -> [Path Rel Dir] -> Spec
      filteredLein sieve root paths = repo
        Repo
          { repoRoot = root,
            repoPrebuildScript = Nothing,
            repoDynamicNixDeps = ["leiningen"],
            repoAnalyses =
              [ Analysis
                  { analysisName = "leiningen",
                    analysisFinder = fmap (filter sieve) . Leiningen.findProjects,
                    analysisFunc = Leiningen.getDeps,
                    analysisMkProject = Leiningen.mkProject,
                    analysisProjects = map simpleTestProject paths
                  }
              ]
          }

  focus $ lein [reldir|repos/clojure/puppetserver|] [[reldir|.|]]

  focus $ lein [reldir|repos/clojure/ring|]
    [ [reldir|./|],
      [reldir|ring-bench/|],
      [reldir|checkouts/ring-core/|],
      [reldir|checkouts/ring-devel/|],
      [reldir|checkouts/ring-devel/checkouts/ring-core/|],
      [reldir|checkouts/ring-jetty-adapter/|],
      [reldir|checkouts/ring-jetty-adapter/checkouts/ring-core/|],
      [reldir|checkouts/ring-jetty-adapter/checkouts/ring-servlet/|],
      [reldir|checkouts/ring-jetty-adapter/checkouts/ring-servlet/checkouts/ring-core/|],
      [reldir|checkouts/ring-servlet/|],
      [reldir|checkouts/ring-servlet/checkouts/ring-core/|],
      [reldir|ring-core/|],
      [reldir|ring-devel/|],
      [reldir|ring-devel/checkouts/ring-core/|],
      [reldir|ring-jetty-adapter/|],
      [reldir|ring-jetty-adapter/checkouts/ring-core/|],
      [reldir|ring-jetty-adapter/checkouts/ring-servlet/|],
      [reldir|ring-jetty-adapter/checkouts/ring-servlet/checkouts/ring-core/|],
      [reldir|ring-servlet/|],
      [reldir|ring-servlet/checkouts/ring-core/|]
    ]

  let noCrucible Leiningen.LeiningenProject {..} = not . T.isInfixOf "crucible" . T.pack $ toFilePath leinDir

  focus $ filteredLein noCrucible [reldir|repos/clojure/eastwood|]
    [ [reldir|./|],
      [reldir|crucible/project-clj-files/specter-2017-10-07/|],
      [reldir|crucible/project-clj-files/clj-time-2018-09-11/|],
      [reldir|crucible/project-clj-files/quartzite-2014-12-27/|],
      [reldir|crucible/project-clj-files/byte-streams-2018-10-01/|],
      [reldir|crucible/project-clj-files/java.jmx-2018-06-28/|],
      [reldir|crucible/project-clj-files/data.finger-tree-2018-10-08/|],
      [reldir|crucible/project-clj-files/tools.trace-2018-09-28/|],
      [reldir|crucible/project-clj-files/data.generators-2013-09-07/|],
      [reldir|crucible/project-clj-files/compojure-2017-05-03/|],
      [reldir|crucible/project-clj-files/data.xml-2017-11-13/|],
      [reldir|crucible/project-clj-files/data.codec-2017-11-19/|],
      [reldir|crucible/project-clj-files/algo.generic-2014-02-23/|],
      [reldir|crucible/project-clj-files/tools.emitter.jvm-2014-05-29/|],
      [reldir|crucible/project-clj-files/core.match-2017-07-25/|],
      [reldir|crucible/project-clj-files/seesaw-2015-01-04/|],
      [reldir|crucible/project-clj-files/criterium-2016-02-16/|],
      [reldir|crucible/project-clj-files/clojure-2014-03-20/|],
      [reldir|crucible/project-clj-files/potemkin-2015-07-15/|],
      [reldir|crucible/project-clj-files/core.contracts-2017-03-21/|],
      [reldir|crucible/project-clj-files/reply-2018-10-03/|],
      [reldir|crucible/project-clj-files/tools.cli-2018-10-08/|],
      [reldir|crucible/project-clj-files/urly-2014-12-16/|],
      [reldir|crucible/project-clj-files/algo.graph-2013-12-11/|],
      [reldir|crucible/project-clj-files/honeysql-2017-05-20/|],
      [reldir|crucible/project-clj-files/carmine-2018-09-22/|],
      [reldir|crucible/project-clj-files/tools.reader-2017-09-22/|],
      [reldir|crucible/project-clj-files/math.numeric-tower-2014-01-16/|],
      [reldir|crucible/project-clj-files/chash-2014-12-16/|],
      [reldir|crucible/project-clj-files/schema-2017-10-02/|],
      [reldir|crucible/project-clj-files/archimedes-2014-12-15/|],
      [reldir|crucible/project-clj-files/plumbing-2014-12-22/|],
      [reldir|crucible/project-clj-files/data.zip-2017-04-07/|],
      [reldir|crucible/project-clj-files/scrypt-2014-12-16/|],
      [reldir|crucible/project-clj-files/core.cache-2018-03-28/|],
      [reldir|crucible/project-clj-files/data.json-2018-01-05/|],
      [reldir|crucible/project-clj-files/tools.logging-2014-09-21/|],
      [reldir|crucible/project-clj-files/hiccup-2017-01-15/|],
      [reldir|crucible/project-clj-files/core.incubator-2013-06-14/|],
      [reldir|crucible/project-clj-files/stencil-2014-10-23/|],
      [reldir|crucible/project-clj-files/timbre-2017-04-15/|],
      [reldir|crucible/project-clj-files/core.rrb-vector-2018-09-25/|],
      [reldir|crucible/project-clj-files/medley-2017-04-23/|],
      [reldir|crucible/project-clj-files/lib-noir-2015-04-06/|],
      [reldir|crucible/project-clj-files/buffy-2017-09-02/|],
      [reldir|crucible/project-clj-files/collection-check-2017-02-24/|],
      [reldir|crucible/project-clj-files/automat-2017-09-15/|],
      [reldir|crucible/project-clj-files/cheshire-2018-09-21/|],
      [reldir|crucible/project-clj-files/vclock-2014-12-16/|],
      [reldir|crucible/project-clj-files/tools.analyzer-2018-01-19/|],
      [reldir|crucible/project-clj-files/core.memoize-2018-04-22/|],
      [reldir|crucible/project-clj-files/tools.macro-2013-09-12/|],
      [reldir|crucible/project-clj-files/enlive-2014-11-13/|],
      [reldir|crucible/project-clj-files/core.matrix-2018-02-06/|],
      [reldir|crucible/project-clj-files/java.data-2018-06-05/|],
      [reldir|crucible/project-clj-files/data.csv-2018-01-05/|],
      [reldir|crucible/project-clj-files/pantomime-2015-07-12/|],
      [reldir|crucible/project-clj-files/cassaforte-2014-03-11/|],
      [reldir|crucible/project-clj-files/http-kit-2018-04-22/|],
      [reldir|crucible/project-clj-files/serialism-2014-12-16/|],
      [reldir|crucible/project-clj-files/Midje-2014-03-09/|],
      [reldir|crucible/project-clj-files/core.unify-2017-03-21/|],
      [reldir|crucible/project-clj-files/utf8-2014-08-26/|],
      [reldir|crucible/project-clj-files/test.generative-2013-11-10/|],
      [reldir|crucible/project-clj-files/Selmer-2018-09-13/|],
      [reldir|crucible/project-clj-files/cider-nrepl-2018-09-25/|],
      [reldir|crucible/project-clj-files/core.typed-2014-03-21/|],
      [reldir|crucible/project-clj-files/tools.namespace-2017-04-25/|],
      [reldir|crucible/project-clj-files/fs-2017-03-12/|],
      [reldir|crucible/project-clj-files/data.avl-2018-10-08/|],
      [reldir|crucible/project-clj-files/java.jdbc-2018-08-13/|],
      [reldir|crucible/project-clj-files/math.combinatorics-2018-01-05/|],
      [reldir|crucible/project-clj-files/algo.monads-2014-02-23/|],
      [reldir|crucible/project-clj-files/meltdown-2014-11-14/|],
      [reldir|crucible/project-clj-files/useful-2016-12-13/|],
      [reldir|crucible/project-clj-files/support-2014-12-16/|],
      [reldir|crucible/project-clj-files/kria-2014-10-14/|],
      [reldir|crucible/project-clj-files/neocons-2017-03-02/|],
      [reldir|crucible/project-clj-files/java.classpath-2018-05-06/|],
      [reldir|crucible/project-clj-files/core.logic-2017-06-15/|],
      [reldir|crucible/project-clj-files/mailer-2014-12-16/|],
      [reldir|crucible/project-clj-files/data.fressian-2013-11-22/|],
      [reldir|crucible/project-clj-files/ubergraph-2018-07-10/|],
      [reldir|crucible/project-clj-files/elastisch-2017-09-20/|],
      [reldir|crucible/project-clj-files/core.async-2018-08-16/|],
      [reldir|crucible/project-clj-files/money-2014-12-16/|],
      [reldir|crucible/project-clj-files/instaparse-2018-08-19/|],
      [reldir|crucible/project-clj-files/tools.analyzer.jvm-2018-01-19/|],
      [reldir|crucible/project-clj-files/data.priority-map-2018-07-10/|],
      [reldir|crucible/check-var-info/|],
      [reldir|copy-deps-scripts/deps/|] 
    ]

  let cargoProject root =
        repo
          Repo
            { repoRoot = root,
              repoPrebuildScript = Nothing,
              repoDynamicNixDeps = ["rustc", "cargo"],
              repoAnalyses =
                [ Analysis
                    { analysisName = "Cargo",
                      analysisFinder = Cargo.findProjects,
                      analysisFunc = Cargo.getDeps,
                      analysisMkProject = Cargo.mkProject,
                      analysisProjects = [simpleTestProject [reldir|.|]]
                    }
                ]
            }

  cargoProject [reldir|repos/rust/bat|]
  cargoProject [reldir|repos/rust/fd|]
  pending $ cargoProject [reldir|repos/rust/servo|]

  repo
    Repo
      { repoRoot = [reldir|repos/scala/scala|],
        repoPrebuildScript = Nothing,
        repoDynamicNixDeps = ["sbt", "scala"],
        repoAnalyses =
          [ Analysis
              { analysisName = "maven",
                analysisFinder = Scala.findProjects,
                analysisFunc = Maven.getDeps,
                analysisMkProject = Maven.mkProject [absdir|/|],
                analysisProjects = [simpleTestProject [reldir|target/library|]] -- FIXME: this should actually be '.', but instead it's the path of the generated poms
              }
          ]
      }

  repo
    Repo
      { repoRoot = [reldir|repos/scala/sbt|],
        repoPrebuildScript = Nothing,
        repoDynamicNixDeps = ["sbt", "scala"],
        repoAnalyses =
          [ Analysis
              { analysisName = "maven",
                analysisFinder = Scala.findProjects,
                analysisFunc = Maven.getDeps,
                analysisMkProject = Maven.mkProject [absdir|/|],
                analysisProjects = map simpleTestProject
                  [ [reldir|zinc-lm-integration/target/scala-2.12/|],
                    [reldir|util-cache/target/scala-2.12/|],
                    [reldir|testing/agent/target/|],
                    [reldir|target/scala-2.12/|],
                    [reldir|scripted-plugin/target/scala-2.12/|],
                    [reldir|launch/target/|],
                    [reldir|internal/util-relation/target/scala-2.12/|],
                    [reldir|internal/util-position/target/scala-2.12/|],
                    [reldir|internal/util-interface/target/|],
                    [reldir|internal/util-control/target/scala-2.12/|]
                  ] -- FIXME: these should not be in the ~target~s
              }
          ]
      }

  let erlang root =
        repo
          Repo
            { repoRoot = root,
              repoPrebuildScript = Nothing,
              repoDynamicNixDeps = ["rebar3", "erlang"],
              repoAnalyses =
                [ Analysis
                    { analysisName = "rebar3",
                      analysisFinder = Rebar3.findProjects,
                      analysisFunc = Rebar3.getDeps,
                      analysisMkProject = Rebar3.mkProject,
                      analysisProjects = [simpleTestProject [reldir|.|]]
                    }
                ]
            }

  erlang [reldir|repos/erlang/cowboy|]
  erlang [reldir|repos/erlang/emqx|]

  -- FIXME: Package not found in any repo: base64url v1.0
  -- ????
  pending $ erlang [reldir|repos/erlang/ejabberd|]

  -- TODO: split repos
  repo
    Repo
      { repoRoot = [reldir|repos/nuget/orchestrator-powershell|],
        repoPrebuildScript = Nothing,
        repoDynamicNixDeps = [],
        repoAnalyses =
          [ Analysis
              { analysisName = "nuspec",
                analysisFinder = Nuspec.findProjects,
                analysisFunc = Nuspec.getDeps,
                analysisMkProject = Nuspec.mkProject,
                analysisProjects = [simpleTestProject [reldir|.|]]
              }
          ]
      }
  repo
    Repo
      { repoRoot = [reldir|repos/nuget/orchestrator-powershell|],
        repoPrebuildScript = Nothing,
        repoDynamicNixDeps = [],
        repoAnalyses =
          [ Analysis
              { analysisName = "packagereference",
                analysisFinder = PackageReference.findProjects,
                analysisFunc = PackageReference.getDeps,
                analysisMkProject = PackageReference.mkProject,
                analysisProjects = map simpleTestProject [[reldir|UiPath.PowerShell.Tests|], [reldir|UiPath.Web.Client|], [reldir|UiPath.PowerShell|]]
              }
          ]
      }

  repo
    Repo
      { repoRoot = [reldir|repos/nuget/ServiceStack|],
        repoPrebuildScript = Nothing,
        repoDynamicNixDeps = [],
        repoAnalyses =
          [ Analysis
              { analysisName = "packagereference",
                analysisFinder = PackageReference.findProjects,
                analysisFunc = PackageReference.getDeps,
                analysisMkProject = PackageReference.mkProject,
                analysisProjects = map simpleTestProject 
                  [ [reldir|tests/RazorRockstars.Console|],
                    [reldir|tests/ServiceStack.ServiceModel.Tests|],
                    [reldir|tests/CheckWeb|],
                    [reldir|tests/ServiceStack.Razor.Tests|],
                    [reldir|tests/CheckTemplatesCore|],
                    [reldir|tests/ServiceStack.Razor.BuildTask.Tests|],
                    [reldir|tests/CheckWebApi|],
                    [reldir|tests/ServiceStack.Common.Tests|],
                    [reldir|tests/ServiceStack.WebHostApp|],
                    [reldir|tests/ServiceStack.Server.Tests|],
                    [reldir|tests/RazorRockstars.BuildTask|],
                    [reldir|tests/ChatSelfHost|],
                    [reldir|tests/ServiceStack.Razor.BuildTask.IntegrationTests|],
                    [reldir|tests/ServiceStack.RazorNancyTests|],
                    [reldir|tests/ServiceStack.WebHost.IntegrationTests|],
                    [reldir|tests/NetCoreWeb.Tests|],
                    [reldir|tests/CheckGrpc|],
                    [reldir|tests/Check.ServiceInterface|],
                    [reldir|tests/CheckMvc|],
                    [reldir|tests/CheckHttpListener|],
                    [reldir|tests/ServiceStack.RazorHostTests|],
                    [reldir|tests/ServiceStack.Extensions.Tests|],
                    [reldir|tests/ServiceStack.Tests|],
                    [reldir|tests/ServiceStack.IntegrationTests/ServiceStack.IntegrationTests.Host.Web|],
                    [reldir|tests/ServiceStack.IntegrationTests/ServiceStack.IntegrationTests.ConsoleClient|],
                    [reldir|tests/ServiceStack.IntegrationTests/RedisPerfTest|],
                    [reldir|tests/ServiceStack.IntegrationTests/ServiceStack.IntegrationTests.ServiceInterface|],
                    [reldir|tests/ServiceStack.IntegrationTests/ServiceStack.IntegrationTests.test|],
                    [reldir|tests/ServiceStack.IntegrationTests/ServiceStack.IntegrationTests.ServiceModel|],
                    [reldir|tests/ServiceStack.ServiceHost.Tests|],
                    [reldir|tests/RazorRockstars.Web.Tests|],
                    [reldir|tests/Check.ServiceModel|],
                    [reldir|tests/RazorRockstars.Web|],
                    [reldir|tests/CheckCoreApi|],
                    [reldir|tests/CheckIIS|],
                    [reldir|tests/ServiceStack.OpenApi.Tests|],
                    [reldir|tests/ServiceStack.Auth.Tests|],
                    [reldir|tests/ServiceStack.Logging.Tests|],
                    [reldir|tests/CheckRazorCore|],
                    [reldir|tests/CheckWebCore|],
                    [reldir|tests/ServiceStack.Core.SelfHostTests|],
                    [reldir|tests/RazorRockstars.Console.Files|],
                    [reldir|tests/ServiceStack.AuthWeb.Tests|],
                    [reldir|tests/ServiceStack.WebHost.Endpoints.Tests|],
                    [reldir|tests/NetCoreTests|],
                    [reldir|tests/Mvc.Core.Tests|],
                    [reldir|src/ServiceStack.Common|],
                    [reldir|src/ServiceStack.Mvc|],
                    [reldir|src/ServiceStack.Empty|],
                    [reldir|src/ServiceStack.Kestrel|],
                    [reldir|src/ServiceStack.Razor.BuildTask|],
                    [reldir|src/ServiceStack.Api.OpenApi|],
                    [reldir|src/ServiceStack.Razor|],
                    [reldir|src/ServiceStack.Authentication.NHibernate|],
                    [reldir|src/ServiceStack.NetFramework|],
                    [reldir|src/ServiceStack.Logging.Slack|],
                    [reldir|src/ServiceStack.Authentication.OpenId|],
                    [reldir|src/ServiceStack.ProtoBuf|],
                    [reldir|src/ServiceStack|],
                    [reldir|src/ServiceStack.Logging.Serilog|],
                    [reldir|src/ServiceStack.Caching.Memcached|],
                    [reldir|src/ServiceStack.Client|],
                    [reldir|src/ServiceStack.Api.Swagger|],
                    [reldir|src/ServiceStack.Core.SelfHost|],
                    [reldir|src/ServiceStack.HttpClient|],
                    [reldir|src/ServiceStack.Core.WebApp|],
                    [reldir|src/ServiceStack.Interfaces|],
                    [reldir|src/ServiceStack.Logging.EntLib5|],
                    [reldir|src/ServiceStack.Logging.EventLog|],
                    [reldir|src/ServiceStack.MsgPack|],
                    [reldir|src/ServiceStack.GrpcClient|],
                    [reldir|src/ServiceStack.Logging.Elmah|],
                    [reldir|src/ServiceStack.Authentication.OAuth2|],
                    [reldir|src/ServiceStack.Desktop|],
                    [reldir|src/ServiceStack.RabbitMq|],
                    [reldir|src/ServiceStack.Logging.Log4Net|],
                    [reldir|src/ServiceStack.Logging.NLog|],
                    [reldir|src/ServiceStack.Authentication.MongoDb|],
                    [reldir|src/ServiceStack.Server|],
                    [reldir|src/ServiceStack.Wire|],
                    [reldir|src/ServiceStack.Extensions|],
                    [reldir|src/ServiceStack.Authentication.RavenDb|]
                  ]
              }
          ]
      }

  repo
    Repo
      { repoRoot = [reldir|repos/nuget/ServiceStack|],
        repoPrebuildScript = Nothing,
        repoDynamicNixDeps = [],
        repoAnalyses =
          [ Analysis 
              { analysisName = "packagesconfig",
                analysisFinder = PackagesConfig.findProjects,
                analysisFunc = PackagesConfig.getDeps,
                analysisMkProject = PackagesConfig.mkProject,
                analysisProjects = map simpleTestProject 
                  [ [reldir|tests/RazorRockstars.Console|],
                    [reldir|tests/CheckWeb|],
                    [reldir|tests/ServiceStack.Razor.Tests|],
                    [reldir|tests/ServiceStack.Razor.BuildTask.Tests|],
                    [reldir|tests/CheckWebApi|],
                    [reldir|tests/ServiceStack.Razor.BuildTask.IntegrationTests|],
                    [reldir|tests/ServiceStack.RazorNancyTests|],
                    [reldir|tests/ServiceStack.WebHost.IntegrationTests|],
                    [reldir|tests/Check.ServiceInterface|],
                    [reldir|tests/CheckMvc|],
                    [reldir|tests/CheckHttpListener|],
                    [reldir|tests/ServiceStack.RazorHostTests|],
                    [reldir|tests/ServiceStack.ServiceHost.Tests|],
                    [reldir|tests/RazorRockstars.Web.Tests|],
                    [reldir|tests/RazorRockstars.Web|],
                    [reldir|tests/CheckIIS|],
                    [reldir|tests/ServiceStack.OpenApi.Tests|],
                    [reldir|tests/ServiceStack.Auth.Tests|],
                    [reldir|tests/ServiceStack.Logging.Tests|],
                    [reldir|tests/RazorRockstars.Console.Files|],
                    [reldir|tests/ServiceStack.AuthWeb.Tests|],
                    [reldir|src/ServiceStack.Razor|],
                    [reldir|src/ServiceStack.Caching.Memcached|],
                    [reldir|src/ServiceStack.Logging.EventLog|],
                    [reldir|src/ServiceStack.Wire|]
                  ]
              }
              -- FIXME: src/ServiceStack.Core.WebApp/project.json: Error in $: Failed reading: not a valid json value
              {-
              Analysis
                { analysisName = "ProjectJson",
                  analysisFunc = ProjectJson.discover,
                  analysisProjects = [[reldir|src/ServiceStack.Authentication.OAuth2|]]
                }
              -}
          ]
      }

  repo
    Repo
      { repoRoot = [reldir|repos/nuget/Avalonia|],
        repoPrebuildScript = Nothing,
        repoDynamicNixDeps = [],
        repoAnalyses =
          [ Analysis
              { analysisName = "packagereference",
                analysisFinder = PackageReference.findProjects,
                analysisFunc = PackageReference.getDeps,
                analysisMkProject = PackageReference.mkProject,
                analysisProjects = map simpleTestProject 
                  [ [reldir|nukebuild|],
                    [reldir|tests/Avalonia.Layout.UnitTests|],
                    [reldir|tests/Avalonia.Benchmarks|],
                    [reldir|tests/Avalonia.Visuals.UnitTests|],
                    [reldir|tests/Avalonia.Input.UnitTests|],
                    [reldir|tests/Avalonia.Markup.UnitTests|],
                    [reldir|tests/Avalonia.Controls.UnitTests|],
                    [reldir|tests/Avalonia.Interactivity.UnitTests|],
                    [reldir|tests/Avalonia.LeakTests|],
                    [reldir|tests/Avalonia.Base.UnitTests|],
                    [reldir|tests/Avalonia.Direct2D1.UnitTests|],
                    [reldir|tests/Avalonia.Skia.UnitTests|],
                    [reldir|tests/Avalonia.Direct2D1.RenderTests|],
                    [reldir|tests/Avalonia.DesignerSupport.TestApp|],
                    [reldir|tests/Avalonia.Animation.UnitTests|],
                    [reldir|tests/Avalonia.Markup.Xaml.UnitTests|],
                    [reldir|tests/Avalonia.DesignerSupport.Tests|],
                    [reldir|tests/Avalonia.UnitTests|],
                    [reldir|tests/Avalonia.Controls.DataGrid.UnitTests|],
                    [reldir|tests/Avalonia.ReactiveUI.UnitTests|],
                    [reldir|tests/Avalonia.Styling.UnitTests|],
                    [reldir|tests/Avalonia.Skia.RenderTests|],
                    [reldir|samples/ControlCatalog.iOS|],
                    [reldir|samples/RemoteDemo|],
                    [reldir|samples/Previewer|],
                    [reldir|samples/interop/Direct3DInteropSample|],
                    [reldir|samples/interop/NativeEmbedSample|],
                    [reldir|samples/interop/WindowsInteropTest|],
                    [reldir|samples/PlatformSanityChecks|],
                    [reldir|samples/ControlCatalog.Desktop|],
                    [reldir|samples/RenderDemo|],
                    [reldir|samples/BindingDemo|],
                    [reldir|samples/ControlCatalog.Android|],
                    [reldir|samples/ControlCatalog.NetCore|],
                    [reldir|samples/VirtualizationDemo|],
                    [reldir|samples/ControlCatalog|],
                    [reldir|packages/Avalonia|],
                    [reldir|src/Avalonia.Diagnostics|],
                    [reldir|src/Skia/Avalonia.Skia|],
                    [reldir|src/Avalonia.Controls|],
                    [reldir|src/Avalonia.Input|],
                    [reldir|src/Avalonia.FreeDesktop|],
                    [reldir|src/tools/Avalonia.Designer.HostApp|],
                    [reldir|src/Avalonia.Headless.Vnc|],
                    [reldir|src/Avalonia.Layout|],
                    [reldir|src/Avalonia.Visuals|],
                    [reldir|src/Avalonia.X11|],
                    [reldir|src/Avalonia.Styling|],
                    [reldir|src/Avalonia.Headless|],
                    [reldir|src/Avalonia.Native|],
                    [reldir|src/Avalonia.Themes.Fluent|],
                    [reldir|src/Avalonia.Dialogs|],
                    [reldir|src/Avalonia.Controls.DataGrid|],
                    [reldir|src/Avalonia.Themes.Default|],
                    [reldir|src/iOS/Avalonia.iOSTestApplication|],
                    [reldir|src/iOS/Avalonia.iOS|],
                    [reldir|src/Avalonia.OpenGL|],
                    [reldir|src/Avalonia.Interactivity|],
                    [reldir|src/Markup/Avalonia.Markup.Xaml|],
                    [reldir|src/Markup/Avalonia.Markup|],
                    [reldir|src/Markup/Avalonia.Markup.Xaml.Loader|],
                    [reldir|src/Avalonia.Desktop|],
                    [reldir|src/Linux/Avalonia.LinuxFramebuffer|],
                    [reldir|src/Android/Avalonia.Android|],
                    [reldir|src/Android/Avalonia.AndroidTestApplication|],
                    [reldir|src/Avalonia.DesktopRuntime|],
                    [reldir|src/Avalonia.Build.Tasks|],
                    [reldir|src/Avalonia.Base|],
                    [reldir|src/Avalonia.Animation|],
                    [reldir|src/Avalonia.DesignerSupport|],
                    [reldir|src/Avalonia.Remote.Protocol|],
                    [reldir|src/Windows/Avalonia.Direct2D1|],
                    [reldir|src/Windows/Avalonia.Win32|],
                    [reldir|src/Windows/Avalonia.Win32.Interop|],
                    [reldir|src/Avalonia.ReactiveUI|]
                  ]
              }
          ]
      }

  let cocoa root = repo
        Repo
          { repoRoot = root,
            repoPrebuildScript = Nothing,
            repoDynamicNixDeps = [],
            repoAnalyses =
              [ Analysis
                  { analysisName = "cocoapods",
                    analysisFinder = Cocoapods.findProjects,
                    analysisFunc = Cocoapods.getDeps,
                    analysisMkProject = Cocoapods.mkProject,
                    analysisProjects = [simpleTestProject [reldir|.|]]
                  }
              ]
          }

  cocoa [reldir|repos/cocoapods/ShadowsocksX-NG|]
  cocoa [reldir|repos/cocoapods/SDWebImage|]

  let carthage root = repo
        Repo
          { repoRoot = root,
            repoPrebuildScript = Nothing,
            repoDynamicNixDeps = [],
            repoAnalyses =
              [ Analysis
                  { analysisName = "carthage",
                    analysisFinder = Carthage.findProjects,
                    analysisFunc = Carthage.getDeps,
                    analysisMkProject = Carthage.mkProject,
                    analysisProjects = [simpleTestProject [reldir|.|]]
                  }
              ]
          }

  carthage [reldir|repos/carthage/SwiftQueue|]
  carthage [reldir|repos/carthage/Carthage|]

  let setuptools root = repo
        Repo
          { repoRoot = root,
            repoPrebuildScript = Nothing,
            repoDynamicNixDeps = [],
            repoAnalyses =
              [ Analysis
                  { analysisName = "setuptools",
                    analysisFinder = Setuptools.findProjects,
                    analysisFunc = Setuptools.getDeps,
                    analysisMkProject = Setuptools.mkProject,
                    analysisProjects = [simpleTestProject [reldir|.|]]
                  }
              ]
          }

  setuptools [reldir|repos/python/thefuck|]
  -- FIXME: setup.py parser doesn't allow a trailing comma in the requires list:
  --   ['foo','bar',]
  pending $ setuptools [reldir|repos/python/flask|]

  -- FIXME: Error parsing file pipenv/Pipfile.lock : Error in $.develop.pipenv: key "version" not found
  pending $
    repo
      Repo
        { repoRoot = [reldir|repos/python/pipenv|],
          repoPrebuildScript = Nothing,
          repoDynamicNixDeps = [],
          repoAnalyses =
            [ Analysis
                { analysisName = "pipenv",
                  analysisFinder = Pipenv.findProjects,
                  analysisFunc = Pipenv.getDeps,
                  analysisMkProject = Pipenv.mkProject,
                  analysisProjects = map simpleTestProject [[reldir|.|], [reldir|examples|]]
                }
            ]
        }
