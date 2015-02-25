Config { font = "xft:inconsolata:size=14:antialias=true"
       , bgColor = "#000000"
       , fgColor = "#FFFFFF"
       , position = Top 
       , lowerOnStart = True
       , commands = [ Run Weather "KAUS" ["-t"," <tempF>F","-L","64","-H","77","--normal","green","--high","red","--low","lightblue"] 36000
                    , Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Date "%a %b %_d %l:%M" "date" 10
                    , Run BatteryP ["BAT0"] ["-t", "<left>% (<timeleft>)"] 20
                    , Run StdinReader
                    , Run Volume "default" "Master" [] 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %cpu% | %memory% * %swap% | %battery% | %default:Master%   <fc=#ee9a00>%date%</fc> | %KAUS%"
       }
