Config { font = "xft:inconsolata:size=14:antialias=true"
       , bgColor = "#000000"
       , fgColor = "#FFFFFF"
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
