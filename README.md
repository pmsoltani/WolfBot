# WolfBot
Telegram bot engine with Wolfram Language

*Note:* this documention is a work in progress, but there are many lines of comment in the package file ("WolfBot.wl") that can give you an understanding.

*Note:* this engine has been developed using a MacBook for MacOSX & the cloud (maybe, possibly, for Unix too!). It is very probable that it will not work in a Windows machine :-(

# Introduction
This is a Wolfram Language package (engine) to interface with the [Telegram bot API]. If you're not familiar with Telegram bots, visit [Bots: An introduction for developers]. This package is based on the work by [Guillermo Hernández].
To have a working bot, some basic functions are needed:

* A function to send messages (`BotCall`)
* A function to send files (`BotFileCall`)
* A fucntion to receive user inputs & process them (`BotCommand`)

This package, however is more extended and has several other functions as well:

* `BotLog` -> logs everthings sent or received to a file
* `BotGetFile` -> receives the files sent by user
* `BotLogImport` -> imports or creates a log file to be processed
* `BotSelectUpdate` -> selects the first unanswered user input from the imported log to be processed
* `BotSelectUpdateWebhook` -> selects the first unanswered user input, if the bot uses [webhook]
* `BotUserPrivilege` -> determines the clearance of the user
* `BotKeyboardArray` -> creates a [custom keyboard] for the user
* `BotAnswer` -> uses `BotCall` & `BotFileCall` to send the results back to the user
* `BotProcessMessage` -> The main "front-end" of the package, which uses all other fucntions

This engine relies heavily on creating and using logs. Basically, every communication is first logged and then processed. The `BotProcessMessage` function incorporates methods to insure that the logs are small enough so as not to reduce the importing speed.
The engine supports bot "getUpdates" & "webhook" methods for receiving messages. For using webhooks, a Wolfram account with some [Cloud Credits] is required. *Beware that using this engine on webhooks can reduce your cloud credits and so cost you money!*
Switching between "getUpdates" & "webhook" is done automatically like this:
`BotProcessMessage[configFile,logSizeLimit,pollTime,""]` -> for using "getUpdates" &
`BotProcessMessage[configFile,logSizeLimit,pollTime,ToString[HTTPRequestData["Body"]]]&],Permissions->"Public"]` -> for using "webhook".

# Using the engine

## getUpdates
1. create a Telegram bot using [@botfather]
2. fill out the "configFile.json" with your bot's information
3. download the files "WolfBot.wl", "WolfBot.nb" & "WolfBot-Script.m"
4. open "WolfBot.nb" and evaluate the "getupdates" section
5. alternatively, open "WolfBot-Script.m" with a text editor and change the paths and then run the following command in the terminal:
   `wolframscript -f Path/To/WolfBot-Script.m"`

## Webhook
1. create a Telegram bot using [@botfather]
2. fill out the "configFile.json" with your bot's information
3. create a folder named "configFile.json", "WolfBot" in your Wolfram cloud and upload the files "WolfBot.wl" & "WolfBot.nb" in it
4. evaluate the cells in the "webhook" section

# Appendix
A schematic of the engine when using "getUpdates" is shown below:
```
Chart size:                       ┌──────────────────┐                        
65x77                             │                  │                       
    ┌────────────────────────────>│ Telegram Bot API │                       
    │                             │                  │                       
    │                             └──────────────────┘                       
    │                                       ▲                                
┌───────┐       BotProcessMessage ──────────┼───────────────────────────────┐
│ _   _ │      │   _                        │                               │
│| | | |│      │  / \                       │                               │
│| | | |│      │ ( 1 )                      │                               │
│| |_| |│      │  \_/                       │                               │
│ \___/ │      │ ┌────────────────────┐     │                               │
│       │      │ │████████████████████│     │                               │
│       │      │ │██Read or create a██│     │                               │
│  ___  │      │ │██file containing███│     │                               │
│ / __| │      │ │████the paths of████│     │                               │
│ \__ \ │      │ │██bot's log files███│     │                               │
│ |___/ │      │ │████████████████████│     │                               │
│       │      │ └────────────────────┘     │                               │
│       │      │   _        │               │                               │
│   ___ │      │  / \       │               │                               │
│  / _ \│      │ ( 2 )      │               │                               │
│ |  __/│      │  \_/       ▼               │                           _   │
│  \___|│      │ ┌───────── BotSelectUpdate ┼────────┐  ┌──────────┐   / \  │
│       │      │ │                          ▼        │  │          │  ( 3 ) │
│       │      │ │ ┌ BotLogImport ┐ ┌─── BotCall ──┐ │  │          ▼   \_/  │
│  _ __ │      │ │ │              │ │              │ │  │  ┌─ BotCommand ─┐ │
│ | '__|│      │ │ │              │ │ ┌─ BotLog ─┐ │ │  │  │              │ │
│ | |   │      │ │ │              │ │ │          │ │ │  │  │ ┌─ BotUser ─┐│ │
│ |_|   │      │ │ │              │ │ └──────────┘ │ │  │  │ │ Privilege ││ │
│       │      │ │ └──────────────┘ └──────────────┘ │  │  │ └───────────┘│ │
│       │      │ │  ▲      │                ▲        │  │  │       │      │ │
│  ___  │      │ │  │      │  If there's no │        │  │  │       │      │ │
│ / __| │      │ │  │      └── new updates ─┘        │  │  │       ▼      │ │
│ \__ \ │      │ │  │          in the log            │  │  │┌────────────┐│ │
│ |___/ │      │ └──┼────────────────────────────────┘  │  ││████████████││ │
│       │      │    │              │     Process the    │  ││█Processing█││ │
└───────┘      │    │              │ selected update so │  ││██Commands██││ │
    ▲          │    │   _          └── that BotCommand ─┘  ││████████████││ │
    │          │    │  / \               can use it        │└────────────┘│ │
    │          │    │ ( 4 )                                │       │      │ │
    │          │    │  \_/                                 │  If keyboard │ │
    │          │    │ ┌─────── BotAnswer ──────┐           │   is needed  │ │
    │          │    │ │                        │           │       │      │ │
    │          │    │ │ ┌────── BotCall ─────┐ │           │       ▼      │ │
┌───────────┐  │    │ │ │                    │ │           │ ┌─── Bot ───┐│ │
│ Telegram  │  │    │ │ └────────────────────┘ │           │ │  Keyboard ││ │
│  Bot API  │<─┼────┼─│                        │           │ └── Array ──┘│ │
└───────────┘  │    │ │ ┌──── BotFileCall ───┐ │           └──────────────┘ │
               │    │ │ │                    │ │                   │        │
               │    │ │ └────────────────────┘ │                   │        │
               │    │ │                        │<──────────────────┘        │
               │    │ └────────────────────────┘                            │
               │    │   _          │                                        │
               │    │  / \         │                                        │
               │    │ ( 5 )        │                                        │
               │    │  \_/         ▼                                        │
               │    │ ┌────────────────────────┐                            │
┌───────────┐<─┼────┘ │████████████████████████│                            │
│   Bot's   │  │      │██Update the bot's log██│                            │
│  Server   │<─┼──────│███file with the call███│                            │
└───────────┘  │      │████████results█████████│                            │
               │      │████████████████████████│                            │
               │      └────────────────────────┘                            │
               └────────────────────────────────────────────────────────────┘
```

[Telegram bot API]: https://core.telegram.org/bots/api
[Bots: An introduction for developers]: https://core.telegram.org/bots/
[Guillermo Hernández]: https://github.com/Dih5/TgBot
[webhook]: https://core.telegram.org/bots/webhooks
[custom keyboard]: https://core.telegram.org/bots#keyboards
[CloudCredits]: http://www.wolfram.com/cloud-credits
[@botfather]: https://t.me/botfather
