import           Development.Shake
import           Development.Shake.FilePath
import           System.Exit

lhs2TeX, pdflatex, rubber, agda, bibtex :: String
lhs2TeX  = "lhs2TeX"
pdflatex = "pdflatex"
rubber   = "rubber"
agda     = "agda"
bibtex   = "bibtex"

main :: IO ()
main = shake shakeOptions $ do

  want ["CPiH.pdf"]

  "*.tex" %> \output -> do
      let input = output -<.> "lhs"
      need [input]
      cmd lhs2TeX ["--tt", "-o", output, input]

  "*.pdf" %> \output -> do
      let input = output -<.> "tex"
      need [input, "references.bib"]
      cmd rubber ["-d", "--unsafe", input]
