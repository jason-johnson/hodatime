import Parser
import Olson
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Exit (ExitCode(..), exitWith)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Control.Monad (when, forM_)

main :: IO ()
main = run =<< parse

run :: Settings -> IO ()
run settings@(Settings _ _ file) = do
  bs <- BS.readFile file
  (bs', header) <- runGet getHeader bs
  printFile settings bs' header
  return ()

skipTransactions :: Int -> BS.ByteString -> Header -> IO BS.ByteString
skipTransactions transSize bs (Header _ gmtCount stdCount leapsCnt transitionsCnt typeCnt abbrLen) = do
  let size = transitionsCnt * transSize + transitionsCnt + typeCnt * transSize + typeCnt + typeCnt + abbrLen + leapsCnt * transSize * 2 + stdCount + gmtCount
  (bs', _) <- runGet (getSkipped size) bs
  return bs'

printFile :: Settings -> BS.ByteString -> Header -> IO ()
printFile (Settings version HeaderOnly file) bStr header@(Header hVersion _ _ _ _ _ _)
  | version == 1 = printAll header bStr
  | version == 2 && hVersion == 50 = skipAndPrint
  | version == 3 && hVersion == 51 = skipAndPrint
  | otherwise = failVersion
  where
    failVersion = do
      putStrLn $ "Requested version: " ++ show version ++ ", file version: " ++ show hVersion
      printAll header bStr
      exitWith (ExitFailure 1)
    printAll hdr bs = do
      putStrLn $ "File: " ++ file
      putStrLn $ "Data left in file: " ++ show (BS.length bs)
      putStrLn $ show hdr
    skipAndPrint = do
      bs <- skipTransactions 4 bStr header
      (bs', header') <- runGet getHeader bs
      printAll header' bs'
printFile (Settings version TransitionsOnly file) bStr header@(Header hVersion _ _ _ _ _ _)
  | version == 1 = printAll header bStr get32bitInteger False
  | version == 2 && hVersion == 50 = skipAndPrint
  | version == 3 && hVersion == 50 = skipAndPrint
  | otherwise = do
      putStrLn $ "Requested version: " ++ show version ++ ", file version: " ++ show hVersion
      printAll header bStr get32bitInteger False
      exitWith (ExitFailure 1)
  where
    printAll hdr bs getTrans needPosixStr = do
      putStrLn $ "File: " ++ file
      (bs', transitions) <- runGet (getPayload hdr getTrans) bs
      forM_ transitions (putStrLn . show)
      if needPosixStr 
        then do
          (bs'', posixTZ) <- runGet getPosixTZ bs'
          putStrLn $ "Posix TZ string: " ++ posixTZ
          putStrLn $ "Data left in file: " ++ show (BS.length bs'')
        else do
          putStrLn $ "Data left in file: " ++ show (BS.length bs')
    skipAndPrint = do
      bs <- skipTransactions 4 bStr header
      (bs', header') <- runGet getHeader bs
      printAll header' bs' get64bitInteger True
printFile settings@(Settings _ AllData _) bStr header = do
  printFile (settings { setDisplay = HeaderOnly }) bStr header
  printFile (settings { setDisplay = TransitionsOnly }) bStr header