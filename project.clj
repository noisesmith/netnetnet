(defproject netnetnet "0.0.1"
  :description "Data modeling in the browser"
  :url "http://github.com/prismofeverything"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [http-kit "2.1.12"]
                 [polaris "0.0.4"]
                 [ring "1.2.0"]
                 [org.clojure/clojurescript "0.0-2156"]
                 [org.clojure/core.async "0.1.267.0-0d7780-alpha"]
                 [om "0.5.0-rc1"]
                 [domina "1.0.2"]
                 [com.keminglabs/singult "0.1.6"]
                 [org.clojure/google-closure-library-third-party "0.0-2029"]]
  :plugins [[lein-cljsbuild "1.0.2"]
            [com.keminglabs/cljx "0.3.2"]]
  :source-paths ["src/clj" "target/generated/clj"]
  :resource-paths ["resources/"]
  :min-lein-version "2.0.0"
  :repl-options {:host "localhost"
                 :port 12921}
  :main netnetnet.server
  :cljx 
  {:builds
   [{:source-paths ["src/cljx"]
     :output-path "target/generated/clj"
     :rules :clj}
    {:source-paths ["src/cljx"]
     :output-path "target/generated/cljs"
     :rules :cljs}]}
  :cljsbuild 
  {:builds 
   {:dev 
    {:libs ["singult"]
     :source-paths ["src/cljs" "target/generated/cljs"]
     :compiler 
     {:externs []
      :optimizations :whitespace
      :output-to "resources/public/js/app/netnetnet.js"
      :output-dir "resources/public/js/app/out"}}}})
