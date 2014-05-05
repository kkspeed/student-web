import Hakyll
import WebPage.Generate.Rules

config :: Configuration
config = defaultConfiguration
        {deployCommand = "scp -r _site/* muyuanli@timberlake.cse.buffalo.edu:~/public_html"}

main :: IO ()
main = hakyllWith config compileRules
