Config { font = "xft:inconsolata:size=14:antialias=true"
       , bgColor = "#002b36"
       , fgColor = "#657b83"
       , position = Bottom 
       , lowerOnStart = True
       , hideOnStart = False
       , persistent = True
       , commands = [ 
            Run Com "tail" ["--lines=1","~/.info"] "info" 1
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "INFO: }{ %info%"
       }
