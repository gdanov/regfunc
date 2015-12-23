(defproject
	regfunc "0.1.0-SNAPSHOT"

	:dependencies [[org.clojure/clojure "1.7.0"]
								 [http-kit "2.1.16"]
								 [org.clojure/data.json "0.2.6"]
								 [clj-time "0.9.0"]

                 [prismatic/schema "0.4.0"]
                 [com.roomkey/annotate "1.0.1"]

								 [org.clojure/core.async "0.1.346.0-17112a-alpha"]

                 [mvxcvi/puget "0.8.0"]

                 [midje "1.8.3"]

                 [robert/hooke "1.3.0"]
                 [com.taoensso/timbre "4.1.0"]

                 [criterium "0.4.3"]

                 [org.clojure/tools.nrepl "0.2.12"]]

  ;; :global-vars {*print-length* 30}

  :jvm-opts ["-Dannotate.typecheck=on"]
  
	:plugins [[mvxcvi/whidbey "0.6.0"]
						[lein-midje "3.1.3"]]

	;; :main ^:skip-aot jira.core

	:target-path "target/%s"

	:profiles {:uberjar {:aot :all}
             :repl {:plugins [[cider/cider-nrepl "0.10.0"]
                              [refactor-nrepl "2.0.0-SNAPSHOT"]
                              ]}}

	:whidbey {:width           180
						:map-delimiter   ""
						:extend-notation true
						:print-meta      true
						:color-scheme    {:delimiter [:blue]
															:tag       [:bold :red]
															}
						}
	)
