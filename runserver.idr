module RunServer
import Network.API
import Network.Wai
import Network.DefaultHttpEngine
import Control.ST
import Network.Wai
import Network.ST.TcpSockets
import Network.Socket.Data

specificEngine : HttpEngine IO
specificEngine = defaultHttpEngine IO ioTcpSockets

localAddress : SocketAddress
localAddress = IPv4Addr 127 0 0 1

mainST : ST IO String []
mainST = runApplication specificEngine (Just localAddress) 8080 $
           mkApplication specificEngine $ \engineState, req => do
            sendResponse specificEngine engineState (MkHttpResponse http200 [] "It works!")

-- runServer : (apiDef : ApiDef) -> (serverAPI : ServerType apiDef) -> IO ()
-- runServer apiDef serverAPI = ?runServer_rhs

main : IO ()
main = do
  result <- run mainST
  putStrLn "finished"
