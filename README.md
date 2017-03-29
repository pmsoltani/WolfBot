# WolfBot
Telegram bot engine with Wolfram Language

**Note:** After reading this file, if you still have no clue, or have some technical question, contact me.

**Note:** this engine has been developed using a MacBook for macOS & the cloud (maybe, possibly, for Unix too!). It is very probable that it will not work on a Windows machine :-(

# Introduction
This is a Wolfram Language package (engine) to interface with the [Telegram bot API]. If you're not familiar with Telegram bots, visit [Bots: An introduction for developers]. This package is based on the work by [Guillermo Hernández].
To have a working bot, some basic functions are needed:

* A function to send messages (`BotCall`)
* A function to send files (`BotFileCall`)
* A function to receive user inputs & process them (`BotCommand`)

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

This engine relies heavily on creating and using logs. Basically, every communication is first logged and then processed. The `BotProcessMessage` function incorporates methods to ensure that the logs are small enough so as not to reduce the importing speed.

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

## The "confingFile.json"
The engine repeatedly uses this file to get important stuff from it:
* The bot's token, needed for any kind of communication,
* The bot's username, necessary for creating the folder on the disk, in which the logs and other files are stored,
* ID's of the users
* Info about other services, currently about the [Dark Sky API]

The engine supports 3 kinds of users:
1. Admins, who have the most clearance (2),
2. Members, who don't have the privilege to do everything with the bot (1),
3. Blocked members, who can't do anything with the bot (0)

When a new user starts the bot, he/she receives a message telling him/her that the admin(s) have been notified and to wait for admission. The member will be added to the "TempList". At the same time, the admin(s) receive a message about the new user and they can decide whether to let him/her in, right from telegram. If approved, the member will be removed from the "TempList" and added to the "MemberList", otherwise, he/she will be added to the "BlackList".

Adding new admins can only be done by modifying the "configFile.json" manually.
This admin/member feature of the engine can be bypassed by changing the `/start` command section of the `BotCommand` function so that new users are automatically added to the "MemberList", but it is not advised.

### The Dark Sky API
The engine can send some info to the users about their location, such as the nearest city to them (if they aren't in one), elevation from the sea level and some weather information. The last part usually comes from the Dark Sky API.

In the free mode of this API, 1000 calls per day are allowed, so the total count and the last call's date is saved in "configFile.json" and if the threshold is reached, the weather info will be sent using Wolfram language's `WeatherData`.

## Supported commands
Telegram bot commands are in the form `/command [optional arguments]`. Each command requires a minimum clearance which is defined at the beginning of the `BotCommand` function in an association called `minPrivilege`. This is because experienced Wolfram language users can execute potentially harmful commands (like shell commands). Note that when the engine is running in the cloud, the privilege of some commands is lowered, because the harming the system will be no longer the case.

Supported commands:
1. `/start`: common among every Telegram bot, it essentially starts the conversation with the bot. New users will be informed to wait until they are approved by the admin(s).
2. `/accept` & `/decline`: used only by admins to approve/reject new users.
3. `/temp`: used by the admins to see who is in the "TempList", so they can approve/reject them.
4. `/whoami`: sends the user his/her status (Admin, Member, Blocked)
5. `/ping`: calculates how many seconds it takes to receive and send back a typical message.
6. `/wol` (`/wolfram`): is used to take advantage of the Wolfram language. Examples:

    /wol 2+2,
    
    /wol N[Pi,200],
    
    /wol Plot[Sin[x],{x,0,2Pi}]
    
    The engine will send back a text/image/audio, based on the `Head` of the answer.
    
7. `/shell`: used to execute shell commands directly from Telegram. Some useful functions are displayed by custom keyboard, namely "Server screenshot" & "Turn off server's display", if the "admin" uses no arguments with the command; otherwise, the argument is executed (like `/shell pwd`).
8. `/open`: with no arguments, sends back the list of installed applications on the server using a custom keyboard, otherwise opens the specified file/app (like `/open Terminal`, `/open ~/Downloads/123.txt`).
9. `/music`: this one is really fun: on a macOS, one can control iTunes using a custom keyboard, otherwise this command generates a random music and sends it to the user.
10. `/location`: after sending this command, the engine asks for the location of the user and will provide him/her some weather information (Powered by Dark Sky). This command isn't necessary and users can send their locations any time to receive the same results.
11. `/today`: Displays the date in Shamsi & Gregorian calendars (needs the [Shamsi] package).
12. `/qr`: is used to generate/scan qr codes. Users are asked to send a text or a picture.
13. `/files`: used by the admins to browse and download files on their `$HomeDirectory`. Uses custom keyboard usually, unless the number of files in particular folder is too many (more that 240 files), in which case sends several messages to the user.
14. `/dump`: used by the admins so they can make the server download their files/links. It will then provides them an option to open those files using the default application (if the server isn't running in the cloud, of course).
15. `/keyboardoff`: removes the custom keyboards.
16. `/help`: lists the available commands.
17. `/botstop`: gives the admins a way to stop the engine remotely (without getting out of the bed!). To restart the engine, the admin needs to be behind his/her computer, of course. The effect of this command is only obvious when the engine uses "getUpdates" method.

# How does it work?
The way the engine works depends on the method by which it's working, namely "getUpdates" and "webhook".

## getUpdates
The engine starts with the following command:
```
BotProcessMessage[
    configFile,
    logSizeLimit,
    pollTime,
    ""
]
```

When the engine starts, it looks for a file named "log.txt" within a directory named `<BotUsername>_temp` in the path of the "WolfBot.nb" notebook. This file contains the paths of "result logs" created when the bot communicates with its users. If the engine finds the "log.txt" file, it reads the last "result log" file (which is a JSON file, by the way), otherwise, it creates one and then creates a "result log" JSON file (using `BotLogImport` & `BotSelectUpdate` functions).

The "result log" is then searched to find an unanswered user input (if not found any, the engine calls the Telegram API with a "getUpdates" method, until it receives an update). The unanswered update is then processed before it is sent to the `BotCommand` function. For example, when the user sends a location, this is not a command, hence it can't be used right away; so the `BotProcessMessage` does the following to the user's latitude and longitude:
```
content=<|"command"->"/location",data-><|"latitude"->LAT,"longitude"->LON|>|>;
BotCommand[content]
```
The `BotCommand` function then decides what to do with the modified user input, based on the many factors such as the system on which the engine is running, the privilege of the user, etc.

Sometimes further interaction with the user is needed; for example, when he/she sends a `/qr` command, the engine should be ready to receive another input, namely a text or a picture. For this reason, a very important association is declared: `followup`. This association contains the type of input to be expected from each user for the next time; so its structure is like this:
```
followup=<|userID_1->{type1,type2,...},userID_2->{type1,type2,...},...|>
```
The user can, of course, deviate from this and send an irrelevant input and because of the structure of the engine, it won't cause any problems.

So the `BotCommand` function creates a `response` and a `followup` based on the input. These two are the output of the `BotCommand` function. The `response` is then analyzed by the `BotAnswer` function and is sent to the user. The results of the communication are logged to the result log and that particular update is marked as "answered". This concludes the first iteration of the `BotProcessMessage` function. The `followup` association is used for the next iteration. When all updates are answered, the engine calls the Telegram API for new ones periodically (specified by the `pollTime` variable).

## webhook
In the webhook method, the Telegram API automatically sends new updates to a server specified by the owner of the bot. To use this functionality, one has to have some "Cloud Credits" in his/her Wolfram Cloud account.

**Caution: Using this method will consume your cloud credits eventually. Use at your own risk!**

The server is created using `APIFunction` & `CloudDeploy` functions so that the APIFunction sends the content of the messages sent by Telegram API to the engine. This can be done using the `HTTPRequestData` function:
```
CloudDeploy[
    APIFunction[
        {},
        BotProcessMessage[
            configFile,
            logSizeLimit,
            pollTime,
            ToString[HTTPRequestData["Body"]]
        ]&
    ],
    Permissions->"Public"
]
```

Note that the 4th argument of the `BotProcessMessage` isn't `""` like before. this indicates to the engine that it should use the webhook method. The The `APIFunction` brings up the `BotProcessMessage` whenever there are new updates from the Telegram API. The updates are logged and then answered in the same way as "getUpades" method, but the engine stops after answering all updates and won't call the Telegram API for new updates. This way, the bot's performance is much better and there's a lot less pressure on the system.


# Appendix A
Some scrrenshots of the bot.

## `/start` & `/help` commands
![alt text][sc1]

## `/whoami`, `/ping` & `/today` commands
![alt text][sc2]

## `/wol` commnad
![alt text][sc3]

## `/shell` command
![alt text][sc4]

## `/music` command
![alt text][sc5]

## `/files` command
![alt text][sc6]

## `/qr` command
![alt text][sc7]

# Appendix B
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
[Dark Sky API]: https://darksky.net/dev/
[Shamsi]: https://github.com/pmsoltani/Shamsi-calendar


[sc1]: https://github.com/pmsoltani/WolfBot/blob/master/screenshots/1.png
[sc2]: https://github.com/pmsoltani/WolfBot/blob/master/screenshots/2.png
[sc3]: https://github.com/pmsoltani/WolfBot/blob/master/screenshots/3.png
[sc4]: https://github.com/pmsoltani/WolfBot/blob/master/screenshots/4.png
[sc5]: https://github.com/pmsoltani/WolfBot/blob/master/screenshots/5.png
[sc6]: https://github.com/pmsoltani/WolfBot/blob/master/screenshots/6.png
[sc7]: https://github.com/pmsoltani/WolfBot/blob/master/screenshots/7.png
