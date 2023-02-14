import           Development.Shake
import           Development.Shake.FilePath
import           System.Exit

pdflatex, rubber :: String
pdflatex = "pdflatex"
rubber   = "rubber"

main :: IO ()
main = shake shakeOptions $ do

  want ["CPiH.pdf"]

  "*.pdf" %> \output -> do
      let input = output -<.> "tex"
      need [input, "references.bib"]
      cmd rubber ["-d", "--unsafe", input]
