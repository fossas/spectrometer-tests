{-# LANGUAGE QuasiQuotes #-}

module IntegrationSpec
  ( spec,
  )
where

import Repo
import Path
import qualified Strategy.Cargo as Cargo
import qualified Strategy.Carthage as Carthage
-- import qualified Strategy.Clojure as Clojure
import qualified Strategy.Cocoapods.Podfile as Podfile
import qualified Strategy.Cocoapods.PodfileLock as PodfileLock
import qualified Strategy.Erlang.Rebar3Tree as Rebar3Tree
import qualified Strategy.Go.GoList as GoList
import qualified Strategy.Maven.Pom as MavenPom
import qualified Strategy.NuGet.Nuspec as Nuspec
import qualified Strategy.NuGet.PackageReference as PackageReference
import qualified Strategy.NuGet.PackagesConfig as PackagesConfig
import qualified Strategy.Python.Pipenv as Pipenv
import qualified Strategy.Python.ReqTxt as ReqTxt
import qualified Strategy.Python.SetupPy as SetupPy
import qualified Strategy.Ruby.BundleShow as BundleShow
import qualified Strategy.Ruby.GemfileLock as GemfileLock
import qualified Strategy.Scala as Scala
import Test.Hspec

spec :: Spec
spec = do
  -- let keycloak jdkPkg =
  --       repo
  --         Repo
  --           { repoRoot = [reldir|repos/maven/keycloak|],
  --             repoPrebuildScript = Just [relfile|repos/maven/keycloakbuild.sh|],
  --             repoDynamicNixDeps = ["maven", jdkPkg],
  --             repoAnalyses =
  --               [ Analysis
  --                   { analysisName = "MavenPom",
  --                     analysisFunc = MavenPom.discover,
  --                     analysisProjects = [[reldir|.|], [reldir|boms|]]
  --                   }
  --                   -- FIXME: guava doesn't come bundled with the maven depgraph plugin
  --                   {-
  --                   Analysis
  --                     { analysisName = "MavenPlugin"
  --                     , analysisFunc = MavenPlugin.discover
  --                     , analysisProjects = [[reldir|.|]]
  --                     }
  --                    -}
  --               ]
  --           }

  -- keycloak "jdk8"
  -- keycloak "jdk11"

  -- repo
  --   Repo
  --     { repoRoot = [reldir|repos/ruby/vmfloaty|],
  --       repoPrebuildScript = Just [relfile|repos/ruby/vmfloatybuild.sh|],
  --       repoDynamicNixDeps = ["ruby", "bundler"],
  --       repoAnalyses =
  --         [ Analysis
  --             { analysisName = "GemfileLock",
  --               analysisFunc = GemfileLock.discover,
  --               analysisProjects = [[reldir|.|]]
  --             },
  --           Analysis
  --             { analysisName = "BundleShow",
  --               analysisFunc = BundleShow.discover,
  --               analysisProjects = [[reldir|.|]]
  --             }
  --         ]
  --     }

  -- repo
  --   Repo
  --     { repoRoot = [reldir|repos/ruby/rails|],
  --       repoPrebuildScript = Just [relfile|repos/ruby/railsbuild.sh|],
  --       repoDynamicNixDeps = ["ruby", "bundler", "libiconv", "zlib", "lzma", "rubyPackages.libxml-ruby", "rubyPackages.mysql2", "ncurses", "postgresql", "sqlite"],
  --       repoAnalyses =
  --         [ Analysis
  --             { analysisName = "GemfileLock",
  --               analysisFunc = GemfileLock.discover,
  --               analysisProjects = [[reldir|.|]]
  --             },
  --           Analysis
  --             { analysisName = "BundleShow",
  --               analysisFunc = BundleShow.discover,
  --               analysisProjects = [[reldir|.|]]
  --             }
  --         ]
  --     }

  -- let vault goPkg =
  --       repo
  --         Repo
  --           { repoRoot = [reldir|repos/gomod/vault|],
  --             repoPrebuildScript = Nothing,
  --             repoDynamicNixDeps = [goPkg],
  --             repoAnalyses =
  --               [ Analysis
  --                   { analysisName = "GoList",
  --                     analysisFunc = GoList.discover,
  --                     analysisProjects = [[reldir|.|]]
  --                   }
  --                   -- FIXME: we don't support filepath replaces:
  --                   -- > replace also can be used to inform the go tooling of the relative or absolute on-disk location of modules in a multi-module project, such as:
  --                   -- >     replace example.com/project/foo => ../foo
  --                   {-
  --                   Analysis
  --                     { analysisFunc = Gomod.discover,
  --                       analysisProjects = [[reldir|.|]]
  --                     }
  --                   -}
  --               ]
  --           }

  -- vault "go"
  -- vault "go_1_15"

  -- repo
  --   Repo
  --     { repoRoot = [reldir|repos/clojure/puppetserver|],
  --       repoPrebuildScript = Nothing,
  --       repoDynamicNixDeps = ["leiningen"],
  --       repoAnalyses =
  --         [ Analysis
  --             { analysisName = "Clojure",
  --               analysisFunc = Clojure.discover,
  --               analysisProjects = [[reldir|.|]]
  --             }
  --         ]
  --     }

  -- repo
  --   Repo
  --     { repoRoot = [reldir|repos/clojure/ring|],
  --       repoPrebuildScript = Nothing,
  --       repoDynamicNixDeps = ["leiningen"],
  --       repoAnalyses =
  --         [ Analysis
  --             { analysisName = "Clojure",
  --               analysisFunc = Clojure.discover,
  --               analysisProjects = [[reldir|.|]]
  --             }
  --         ]
  --     }

  -- repo
  --   Repo
  --     { repoRoot = [reldir|repos/clojure/eastwood|],
  --       repoPrebuildScript = Nothing,
  --       repoDynamicNixDeps = ["leiningen"],
  --       repoAnalyses =
  --         [ Analysis
  --             { analysisName = "Clojure",
  --               analysisFunc = Clojure.discover,
  --               analysisProjects = [[reldir|.|]]
  --             }
  --         ]
  --     }

  let cargoProject root =
        repo
          Repo
            { repoRoot = root,
              repoPrebuildScript = Nothing,
              repoDynamicNixDeps = ["rustc", "cargo"],
              repoAnalyses =
                [ Analysis
                    { analysisName = "Cargo",
                      analysisDiscover = Cargo.findProjects,
                      analysisFunc = Cargo.getDeps,
                      analysisMkProject = Cargo.mkProject,
                      analysisProjects = [simpleTestProject [reldir|.|]]
                    }
                ]
            }

  cargoProject [reldir|repos/rust/bat|]
  cargoProject [reldir|repos/rust/fd|]
  pending $ cargoProject [reldir|repos/rust/servo|]

  -- repo
  --   Repo
  --     { repoRoot = [reldir|repos/scala/scala|],
  --       repoPrebuildScript = Nothing,
  --       repoDynamicNixDeps = ["sbt", "scala"],
  --       repoAnalyses =
  --         [ Analysis
  --             { analysisName = "Scala",
  --               analysisFunc = Scala.discover,
  --               analysisProjects = [[reldir|target/library|]] -- FIXME: this should actually be '.', but instead it's the path of the generated poms
  --             }
  --         ]
  --     }

  -- repo
  --   Repo
  --     { repoRoot = [reldir|repos/scala/sbt|],
  --       repoPrebuildScript = Nothing,
  --       repoDynamicNixDeps = ["sbt", "scala"],
  --       repoAnalyses =
  --         [ Analysis
  --             { analysisName = "Scala",
  --               analysisFunc = Scala.discover,
  --               analysisProjects =
  --                 [ [reldir|zinc-lm-integration/target/scala-2.12/|],
  --                   [reldir|util-cache/target/scala-2.12/|],
  --                   [reldir|testing/agent/target/|],
  --                   [reldir|target/scala-2.12/|],
  --                   [reldir|scripted-plugin/target/scala-2.12/|],
  --                   [reldir|launch/target/|],
  --                   [reldir|internal/util-relation/target/scala-2.12/|],
  --                   [reldir|internal/util-position/target/scala-2.12/|],
  --                   [reldir|internal/util-interface/target/|],
  --                   [reldir|internal/util-control/target/scala-2.12/|]
  --                 ] -- FIXME: these should not be in the ~target~s
  --             }
  --         ]
  --     }

  -- let erlang root =
  --       repo
  --         Repo
  --           { repoRoot = root,
  --             repoPrebuildScript = Nothing,
  --             repoDynamicNixDeps = ["rebar3", "erlang"],
  --             repoAnalyses =
  --               [ Analysis
  --                   { analysisName = "Rebar3Tree",
  --                     analysisFunc = Rebar3Tree.discover,
  --                     analysisProjects = [[reldir|.|]]
  --                   }
  --               ]
  --           }

  -- erlang [reldir|repos/erlang/cowboy|]
  -- erlang [reldir|repos/erlang/emqx|]

  -- -- FIXME: Package not found in any repo: base64url v1.0
  -- -- ????
  -- pending $ erlang [reldir|repos/erlang/ejabberd|]

  -- repo
  --   Repo
  --     { repoRoot = [reldir|repos/nuget/orchestrator-powershell|],
  --       repoPrebuildScript = Nothing,
  --       repoDynamicNixDeps = [],
  --       repoAnalyses =
  --         [ Analysis
  --             { analysisName = "Nuspec",
  --               analysisFunc = Nuspec.discover,
  --               analysisProjects = [[reldir|.|]]
  --             },
  --           Analysis
  --             { analysisName = "PackageReference",
  --               analysisFunc = PackageReference.discover,
  --               analysisProjects = [[reldir|UiPath.PowerShell.Tests|], [reldir|UiPath.Web.Client|], [reldir|UiPath.PowerShell|]]
  --             }
  --         ]
  --     }

  -- repo
  --   Repo
  --     { repoRoot = [reldir|repos/nuget/ServiceStack|],
  --       repoPrebuildScript = Nothing,
  --       repoDynamicNixDeps = [],
  --       repoAnalyses =
  --         [ Analysis
  --             { analysisName = "PackageReference",
  --               analysisFunc = PackageReference.discover,
  --               analysisProjects =
  --                 [ [reldir|tests/RazorRockstars.Console|],
  --                   [reldir|tests/ServiceStack.ServiceModel.Tests|],
  --                   [reldir|tests/CheckWeb|],
  --                   [reldir|tests/ServiceStack.Razor.Tests|],
  --                   [reldir|tests/CheckTemplatesCore|],
  --                   [reldir|tests/ServiceStack.Razor.BuildTask.Tests|],
  --                   [reldir|tests/CheckWebApi|],
  --                   [reldir|tests/ServiceStack.Common.Tests|],
  --                   [reldir|tests/ServiceStack.WebHostApp|],
  --                   [reldir|tests/ServiceStack.Server.Tests|],
  --                   [reldir|tests/RazorRockstars.BuildTask|],
  --                   [reldir|tests/ChatSelfHost|],
  --                   [reldir|tests/ServiceStack.Razor.BuildTask.IntegrationTests|],
  --                   [reldir|tests/ServiceStack.RazorNancyTests|],
  --                   [reldir|tests/ServiceStack.WebHost.IntegrationTests|],
  --                   [reldir|tests/NetCoreWeb.Tests|],
  --                   [reldir|tests/CheckGrpc|],
  --                   [reldir|tests/Check.ServiceInterface|],
  --                   [reldir|tests/CheckMvc|],
  --                   [reldir|tests/CheckHttpListener|],
  --                   [reldir|tests/ServiceStack.RazorHostTests|],
  --                   [reldir|tests/ServiceStack.Extensions.Tests|],
  --                   [reldir|tests/ServiceStack.Tests|],
  --                   [reldir|tests/ServiceStack.IntegrationTests/ServiceStack.IntegrationTests.Host.Web|],
  --                   [reldir|tests/ServiceStack.IntegrationTests/ServiceStack.IntegrationTests.ConsoleClient|],
  --                   [reldir|tests/ServiceStack.IntegrationTests/RedisPerfTest|],
  --                   [reldir|tests/ServiceStack.IntegrationTests/ServiceStack.IntegrationTests.ServiceInterface|],
  --                   [reldir|tests/ServiceStack.IntegrationTests/ServiceStack.IntegrationTests.test|],
  --                   [reldir|tests/ServiceStack.IntegrationTests/ServiceStack.IntegrationTests.ServiceModel|],
  --                   [reldir|tests/ServiceStack.ServiceHost.Tests|],
  --                   [reldir|tests/RazorRockstars.Web.Tests|],
  --                   [reldir|tests/Check.ServiceModel|],
  --                   [reldir|tests/RazorRockstars.Web|],
  --                   [reldir|tests/CheckCoreApi|],
  --                   [reldir|tests/CheckIIS|],
  --                   [reldir|tests/ServiceStack.OpenApi.Tests|],
  --                   [reldir|tests/ServiceStack.Auth.Tests|],
  --                   [reldir|tests/ServiceStack.Logging.Tests|],
  --                   [reldir|tests/CheckRazorCore|],
  --                   [reldir|tests/CheckWebCore|],
  --                   [reldir|tests/ServiceStack.Core.SelfHostTests|],
  --                   [reldir|tests/RazorRockstars.Console.Files|],
  --                   [reldir|tests/ServiceStack.AuthWeb.Tests|],
  --                   [reldir|tests/ServiceStack.WebHost.Endpoints.Tests|],
  --                   [reldir|tests/NetCoreTests|],
  --                   [reldir|tests/Mvc.Core.Tests|],
  --                   [reldir|src/ServiceStack.Common|],
  --                   [reldir|src/ServiceStack.Mvc|],
  --                   [reldir|src/ServiceStack.Empty|],
  --                   [reldir|src/ServiceStack.Kestrel|],
  --                   [reldir|src/ServiceStack.Razor.BuildTask|],
  --                   [reldir|src/ServiceStack.Api.OpenApi|],
  --                   [reldir|src/ServiceStack.Razor|],
  --                   [reldir|src/ServiceStack.Authentication.NHibernate|],
  --                   [reldir|src/ServiceStack.NetFramework|],
  --                   [reldir|src/ServiceStack.Logging.Slack|],
  --                   [reldir|src/ServiceStack.Authentication.OpenId|],
  --                   [reldir|src/ServiceStack.ProtoBuf|],
  --                   [reldir|src/ServiceStack|],
  --                   [reldir|src/ServiceStack.Logging.Serilog|],
  --                   [reldir|src/ServiceStack.Caching.Memcached|],
  --                   [reldir|src/ServiceStack.Client|],
  --                   [reldir|src/ServiceStack.Api.Swagger|],
  --                   [reldir|src/ServiceStack.Core.SelfHost|],
  --                   [reldir|src/ServiceStack.HttpClient|],
  --                   [reldir|src/ServiceStack.Core.WebApp|],
  --                   [reldir|src/ServiceStack.Interfaces|],
  --                   [reldir|src/ServiceStack.Logging.EntLib5|],
  --                   [reldir|src/ServiceStack.Logging.EventLog|],
  --                   [reldir|src/ServiceStack.MsgPack|],
  --                   [reldir|src/ServiceStack.GrpcClient|],
  --                   [reldir|src/ServiceStack.Logging.Elmah|],
  --                   [reldir|src/ServiceStack.Authentication.OAuth2|],
  --                   [reldir|src/ServiceStack.Desktop|],
  --                   [reldir|src/ServiceStack.RabbitMq|],
  --                   [reldir|src/ServiceStack.Logging.Log4Net|],
  --                   [reldir|src/ServiceStack.Logging.NLog|],
  --                   [reldir|src/ServiceStack.Authentication.MongoDb|],
  --                   [reldir|src/ServiceStack.Server|],
  --                   [reldir|src/ServiceStack.Wire|],
  --                   [reldir|src/ServiceStack.Extensions|],
  --                   [reldir|src/ServiceStack.Authentication.RavenDb|]
  --                 ]
  --             },
  --           Analysis
  --             { analysisName = "PackagesConfig",
  --               analysisFunc = PackagesConfig.discover,
  --               analysisProjects =
  --                 [ [reldir|tests/RazorRockstars.Console|],
  --                   [reldir|tests/CheckWeb|],
  --                   [reldir|tests/ServiceStack.Razor.Tests|],
  --                   [reldir|tests/ServiceStack.Razor.BuildTask.Tests|],
  --                   [reldir|tests/CheckWebApi|],
  --                   [reldir|tests/ServiceStack.Razor.BuildTask.IntegrationTests|],
  --                   [reldir|tests/ServiceStack.RazorNancyTests|],
  --                   [reldir|tests/ServiceStack.WebHost.IntegrationTests|],
  --                   [reldir|tests/Check.ServiceInterface|],
  --                   [reldir|tests/CheckMvc|],
  --                   [reldir|tests/CheckHttpListener|],
  --                   [reldir|tests/ServiceStack.RazorHostTests|],
  --                   [reldir|tests/ServiceStack.ServiceHost.Tests|],
  --                   [reldir|tests/RazorRockstars.Web.Tests|],
  --                   [reldir|tests/RazorRockstars.Web|],
  --                   [reldir|tests/CheckIIS|],
  --                   [reldir|tests/ServiceStack.OpenApi.Tests|],
  --                   [reldir|tests/ServiceStack.Auth.Tests|],
  --                   [reldir|tests/ServiceStack.Logging.Tests|],
  --                   [reldir|tests/RazorRockstars.Console.Files|],
  --                   [reldir|tests/ServiceStack.AuthWeb.Tests|],
  --                   [reldir|src/ServiceStack.Razor|],
  --                   [reldir|src/ServiceStack.Caching.Memcached|],
  --                   [reldir|src/ServiceStack.Logging.EventLog|],
  --                   [reldir|src/ServiceStack.Wire|]
  --                 ]
  --             }
  --             -- FIXME: src/ServiceStack.Core.WebApp/project.json: Error in $: Failed reading: not a valid json value
  --             {-
  --             Analysis
  --               { analysisName = "ProjectJson",
  --                 analysisFunc = ProjectJson.discover,
  --                 analysisProjects = [[reldir|src/ServiceStack.Authentication.OAuth2|]]
  --               }
  --             -}
  --         ]
  --     }

  -- repo
  --   Repo
  --     { repoRoot = [reldir|repos/nuget/Avalonia|],
  --       repoPrebuildScript = Nothing,
  --       repoDynamicNixDeps = [],
  --       repoAnalyses =
  --         [ Analysis
  --             { analysisName = "PackageReference",
  --               analysisFunc = PackageReference.discover,
  --               analysisProjects =
  --                 [ [reldir|nukebuild|],
  --                   [reldir|tests/Avalonia.Layout.UnitTests|],
  --                   [reldir|tests/Avalonia.Benchmarks|],
  --                   [reldir|tests/Avalonia.Visuals.UnitTests|],
  --                   [reldir|tests/Avalonia.Input.UnitTests|],
  --                   [reldir|tests/Avalonia.Markup.UnitTests|],
  --                   [reldir|tests/Avalonia.Controls.UnitTests|],
  --                   [reldir|tests/Avalonia.Interactivity.UnitTests|],
  --                   [reldir|tests/Avalonia.LeakTests|],
  --                   [reldir|tests/Avalonia.Base.UnitTests|],
  --                   [reldir|tests/Avalonia.Direct2D1.UnitTests|],
  --                   [reldir|tests/Avalonia.Skia.UnitTests|],
  --                   [reldir|tests/Avalonia.Direct2D1.RenderTests|],
  --                   [reldir|tests/Avalonia.DesignerSupport.TestApp|],
  --                   [reldir|tests/Avalonia.Animation.UnitTests|],
  --                   [reldir|tests/Avalonia.Markup.Xaml.UnitTests|],
  --                   [reldir|tests/Avalonia.DesignerSupport.Tests|],
  --                   [reldir|tests/Avalonia.UnitTests|],
  --                   [reldir|tests/Avalonia.Controls.DataGrid.UnitTests|],
  --                   [reldir|tests/Avalonia.ReactiveUI.UnitTests|],
  --                   [reldir|tests/Avalonia.Styling.UnitTests|],
  --                   [reldir|tests/Avalonia.Skia.RenderTests|],
  --                   [reldir|samples/ControlCatalog.iOS|],
  --                   [reldir|samples/RemoteDemo|],
  --                   [reldir|samples/Previewer|],
  --                   [reldir|samples/interop/Direct3DInteropSample|],
  --                   [reldir|samples/interop/NativeEmbedSample|],
  --                   [reldir|samples/interop/WindowsInteropTest|],
  --                   [reldir|samples/PlatformSanityChecks|],
  --                   [reldir|samples/ControlCatalog.Desktop|],
  --                   [reldir|samples/RenderDemo|],
  --                   [reldir|samples/BindingDemo|],
  --                   [reldir|samples/ControlCatalog.Android|],
  --                   [reldir|samples/ControlCatalog.NetCore|],
  --                   [reldir|samples/VirtualizationDemo|],
  --                   [reldir|samples/ControlCatalog|],
  --                   [reldir|packages/Avalonia|],
  --                   [reldir|src/Avalonia.Diagnostics|],
  --                   [reldir|src/Skia/Avalonia.Skia|],
  --                   [reldir|src/Avalonia.Controls|],
  --                   [reldir|src/Avalonia.Input|],
  --                   [reldir|src/Avalonia.FreeDesktop|],
  --                   [reldir|src/tools/Avalonia.Designer.HostApp|],
  --                   [reldir|src/Avalonia.Headless.Vnc|],
  --                   [reldir|src/Avalonia.Layout|],
  --                   [reldir|src/Avalonia.Visuals|],
  --                   [reldir|src/Avalonia.X11|],
  --                   [reldir|src/Avalonia.Styling|],
  --                   [reldir|src/Avalonia.Headless|],
  --                   [reldir|src/Avalonia.Native|],
  --                   [reldir|src/Avalonia.Themes.Fluent|],
  --                   [reldir|src/Avalonia.Dialogs|],
  --                   [reldir|src/Avalonia.Controls.DataGrid|],
  --                   [reldir|src/Avalonia.Themes.Default|],
  --                   [reldir|src/iOS/Avalonia.iOSTestApplication|],
  --                   [reldir|src/iOS/Avalonia.iOS|],
  --                   [reldir|src/Avalonia.OpenGL|],
  --                   [reldir|src/Avalonia.Interactivity|],
  --                   [reldir|src/Markup/Avalonia.Markup.Xaml|],
  --                   [reldir|src/Markup/Avalonia.Markup|],
  --                   [reldir|src/Markup/Avalonia.Markup.Xaml.Loader|],
  --                   [reldir|src/Avalonia.Desktop|],
  --                   [reldir|src/Linux/Avalonia.LinuxFramebuffer|],
  --                   [reldir|src/Android/Avalonia.Android|],
  --                   [reldir|src/Android/Avalonia.AndroidTestApplication|],
  --                   [reldir|src/Avalonia.DesktopRuntime|],
  --                   [reldir|src/Avalonia.Build.Tasks|],
  --                   [reldir|src/Avalonia.Base|],
  --                   [reldir|src/Avalonia.Animation|],
  --                   [reldir|src/Avalonia.DesignerSupport|],
  --                   [reldir|src/Avalonia.Remote.Protocol|],
  --                   [reldir|src/Windows/Avalonia.Direct2D1|],
  --                   [reldir|src/Windows/Avalonia.Win32|],
  --                   [reldir|src/Windows/Avalonia.Win32.Interop|],
  --                   [reldir|src/Avalonia.ReactiveUI|]
  --                 ]
  --             }
  --         ]
  --     }

  -- repo
  --   Repo
  --     { repoRoot = [reldir|repos/cocoapods/ShadowsocksX-NG|],
  --       repoPrebuildScript = Nothing,
  --       repoDynamicNixDeps = [],
  --       repoAnalyses =
  --         [ Analysis
  --             { analysisName = "Podfile",
  --               analysisFunc = Podfile.discover,
  --               analysisProjects = [[reldir|.|]]
  --             },
  --           Analysis
  --             { analysisName = "PodfileLock",
  --               analysisFunc = PodfileLock.discover,
  --               analysisProjects = [[reldir|.|]]
  --             }
  --         ]
  --     }

  -- repo
  --   Repo
  --     { repoRoot = [reldir|repos/cocoapods/SDWebImage|],
  --       repoPrebuildScript = Nothing,
  --       repoDynamicNixDeps = [],
  --       repoAnalyses =
  --         [ Analysis
  --             { analysisName = "Podfile",
  --               analysisFunc = Podfile.discover,
  --               analysisProjects = [[reldir|.|]]
  --             }
  --         ]
  --     }

  -- repo
  --   Repo
  --     { repoRoot = [reldir|repos/carthage/SwiftQueue|],
  --       repoPrebuildScript = Nothing,
  --       repoDynamicNixDeps = [],
  --       repoAnalyses =
  --         [ Analysis
  --             { analysisName = "Carthage",
  --               analysisFunc = Carthage.discover,
  --               analysisProjects = [[reldir|.|]]
  --             }
  --         ]
  --     }

  -- repo
  --   Repo
  --     { repoRoot = [reldir|repos/carthage/Carthage|],
  --       repoPrebuildScript = Nothing,
  --       repoDynamicNixDeps = [],
  --       repoAnalyses =
  --         [ Analysis
  --             { analysisName = "Carthage",
  --               analysisFunc = Carthage.discover,
  --               analysisProjects = [[reldir|.|]]
  --             }
  --         ]
  --     }

  -- repo
  --   Repo
  --     { repoRoot = [reldir|repos/python/thefuck|],
  --       repoPrebuildScript = Nothing,
  --       repoDynamicNixDeps = [],
  --       repoAnalyses =
  --         [ Analysis
  --             { analysisName = "ReqTxt",
  --               analysisFunc = ReqTxt.discover,
  --               analysisProjects = [[reldir|.|]]
  --             },
  --           Analysis
  --             { analysisName = "SetupPy",
  --               analysisFunc = SetupPy.discover,
  --               analysisProjects = [[reldir|.|]]
  --             }
  --         ]
  --     }

  -- -- FIXME: setup.py parser doesn't allow a trailing comma in the requires list:
  -- --   ['foo','bar',]
  -- pending $
  --   repo
  --     Repo
  --       { repoRoot = [reldir|repos/python/flask|],
  --         repoPrebuildScript = Nothing,
  --         repoDynamicNixDeps = [],
  --         repoAnalyses =
  --           [ Analysis
  --               { analysisName = "SetupPy",
  --                 analysisFunc = SetupPy.discover,
  --                 analysisProjects = [[reldir|.|]]
  --               }
  --           ]
  --       }

  -- -- FIXME: Error parsing file pipenv/Pipfile.lock : Error in $.develop.pipenv: key "version" not found
  -- pending $
  --   repo
  --     Repo
  --       { repoRoot = [reldir|repos/python/pipenv|],
  --         repoPrebuildScript = Nothing,
  --         repoDynamicNixDeps = [],
  --         repoAnalyses =
  --           [ Analysis
  --               { analysisName = "Pipenv",
  --                 analysisFunc = Pipenv.discover,
  --                 analysisProjects = [[reldir|.|], [reldir|examples|]]
  --               }
  --           ]
  --       }
