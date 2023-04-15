$MaelstormJAR = Join-Path $PSScriptRoot "maelstrom\lib\maelstrom.jar"

# & java -Djava.awt.headless=true -jar $MaelstormJAR @Args
& java -jar $MaelstormJAR @Args