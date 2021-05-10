import Neovim (neovim, plugins, defaultConfig)
import Proteome.Plugin (plugin)

main :: IO ()
main = neovim defaultConfig {plugins = [plugin]}
