#!wolframscript

path="Folder/Containing/The/configFile.json";
configFile=FileNameJoin[{path,"configFile.json"}];
logSizeLimit=5*10^4;pollTime=.1;

Import["PATH/TO/WolfBot.wl"];
Needs["WolfBot`"];
BotProcessMessage[configFile,logSizeLimit,pollTime,""]
