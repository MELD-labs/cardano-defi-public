import Lending.Faucet.Handler (apiServer)
import Service.Runner (runApp)

main :: IO ()
main = runApp apiServer
