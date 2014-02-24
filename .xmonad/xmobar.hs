Config { font = "xft:inconsolata:size=16:antialias=true"
       , bgColor = "#002b36"
       , fgColor = "#657b83"
       , position = Top 
       , lowerOnStart = True
       , commands = [ Run Weather "KAUS" ["-t"," <tempF>F","-L","64","-H","77","--normal","green","--high","red","--low","lightblue"] 36000
                    , Run Cpu ["-L","3","-H","50","--normal","green","--high","red"] 10
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Swap [] 10
                    , Run Date "%a %b %_d %l:%M" "date" 10
                    , Run BatteryP ["BAT0"] ["-t", "<left>% (<timeleft>)"] 20
                    , Run StdinReader
                    , Run Com "amixer get Master | grep Mono: | sed s/^[^[]*//" [] "vol" 1
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %cpu% | %memory% * %swap% | %battery% | %vol%   <fc=#ee9a00>%date%</fc> | %KAUS%"
       }