(* ::Package:: *)

(* ::Input:: *)
(*(**)
(*Telegram Bot engine using Wolfram Language*)
(**)
(*by Pooria Soltani*)
(**)
(*based on the work by Guillermo Hernandez*)
(**)*)


BeginPackage["WolfBot`"]


Unprotect["WolfBot`*"];
ClearAll["WolfBot`*"];
ClearAll["WolfBot`Private`*"];


BotLog::usage="BotLog[res,path]"<>"\n"<>
	"Logs the results "<>
	"\!\(\*StyleBox[\"res\",FontSlant->Italic]\)"<>
	" of communication with Telegram API in the location "<>
	"\!\(\*StyleBox[\"path\",FontSlant->Italic]\)"<>".";

BotCall::usage="BotCall[bot,method,args,logPath,timeOut]"<>"\n"<>
	"Makes calls to Telegram API with the specified "<>
	"\!\(\*StyleBox[\"method\",FontSlant->Italic]\)"<>" and "<>
	"\!\(\*StyleBox[\"args\",FontSlant->Italic]\)"<>" and "<>
	"logs the results in "<>
	"\!\(\*StyleBox[\"logPath\",FontSlant->Italic]\)"<>".";

BotFileCall::usage="BotFileCall[bot,path,method,fileField,args,"<>
	"logPath]"<>"\n"<>
	"Sends the file in "<>
	"\!\(\*StyleBox[\"path\",FontSlant->Italic]\)"<>" using the specified "<>
	"\!\(\*StyleBox[\"method\",FontSlant->Italic]\)"<>", "<>
	"\!\(\*StyleBox[\"fileField\",FontSlant->Italic]\)"<>" & "<>
	"\!\(\*StyleBox[\"args\",FontSlant->Italic]\)"<>" to the user "<>
	"and logs the results in "<>
	"\!\(\*StyleBox[\"logPath\",FontSlant->Italic]\)"<>".";

BotGetFile::usage="BotGetFile[bot,fileID,saveDir,logPath]"<>"\n"<>
	"Downloads the file with the specified "<>
	"\!\(\*StyleBox[\"fileID\",FontSlant->Italic]\)"<>" to "<>
	"\!\(\*StyleBox[\"saveDir\",FontSlant->Italic]\)"<>
	" and logs the results in "<>
	"\!\(\*StyleBox[\"logPath\",FontSlant->Italic]\)"<>".";

BotLogImport::usage="BotLogImport[bot,logPath,pollTime]"<>"\n"<>
	"Imports the bot's log in "<>
	"\!\(\*StyleBox[\"logPath\",FontSlant->Italic]\)"<>" if possible or "<>
	"calls the Telegram API with "<>
	"\!\(\*StyleBox[\"pollTime\",FontSlant->Italic]\)"<>" periods, if not.";

BotSelectUpdate::usage="BotSelectUpdate[bot,logPath,pollTime]"<>"\n"<>
	"Selects the first unanswered update from the imported log in "<>
	"\!\(\*StyleBox[\"logPath\",FontSlant->Italic]\)"<>" if possible or "<>
	"calls the Telegram API with "<>
	"\!\(\*StyleBox[\"pollTime\",FontSlant->Italic]\)"<>" periods to "<>
	"retrieve new unanswered updates, if not.";

BotSelectUpdateWebhook::usage="BotSelectUpdateWebhook[webhookUpdates,"<>
	"logPath]"<>"\n"<>
	"Selects the first unanswered update from the imported log in "<>
	"\!\(\*StyleBox[\"logPath\",FontSlant->Italic]\)"<>" or in "<>
	"\!\(\*StyleBox[\"webhookUpdates\",FontSlant->Italic]\)"<>".";

BotUserPrivilege::usage="BotUserPrivilege[bot,userID]"<>"\n"<>
	"Determines the clearance of the user with the ID "<>
	"\!\(\*StyleBox[\"userID\",FontSlant->Italic]\)"<>" or "<>
	"\!\(\*StyleBox[\"username\",FontSlant->Italic]\)"<>".";

BotKeyboardArray::usage="BotKeyboardArray[keys,oneTime,selective,remove]"<>
	"\n"<>
	"Creates a custom keyboard with "<>
	"\!\(\*StyleBox[\"keys\",FontSlant->Italic]\)"<>" according to the "<>
	"specified options:"<>"\n"<>
	"\!\(\*StyleBox[\"oneTime\",FontSlant->Italic]\)"<>
	": keyboard will hide "<>
	"after user presses a button."<>"\n"<>
	"\!\(\*StyleBox[\"selective\",FontSlant->Italic]\)"<>
	": keyboard will be "<>	
	"shown only for the user who initiated the conversation."<>"\n"<>
	"\!\(\*StyleBox[\"remove\",FontSlant->Italic]\)"<>": neglects the other"<>
	"two options and removes the keyboard completely.";

BotCommand::usage="BotCommand[botFile,command,targetUser,targetID,date]"<>
	"\n"<>
	"Deals with the given "<>
	"\!\(\*StyleBox[\"command\",FontSlant->Italic]\)"<>" from the "<>
	"\!\(\*StyleBox[\"targetUser\",FontSlant->Italic]\)"<>" with "<>
	"\!\(\*StyleBox[\"targetID\",FontSlant->Italic]\)"<>
	" and prepares a response";

BotAnswer::usage="BotAnswer[bot,message,messageID,targetID,saveDir]"<>"\n"<>
	"Sends the "<>
	"\!\(\*StyleBox[\"message\",FontSlant->Italic]\)"<>
	" to the user with ID "<>
	"\!\(\*StyleBox[\"targetID\",FontSlant->Italic]\)"<>
	" in reply to message with ID "<>
	"\!\(\*StyleBox[\"messageID\",FontSlant->Italic]\)";

BotProcessMessage::usage="BotProcessMessage[botFile,logSizeLimit,pollTime,"<>
	"webhookUpdates]"<>"\n"<>
	"Processes the inputs from the user using other functions in this package";


Begin["`Private`"]


Needs["Shamsi`"];


BotLog[res_Association,logPath_String]:=
	(* The purpose of this function is to log the results that are
	returned by the Telegram Bot API.
	
	The function first checks to see of the results are "ok",
	meaning that there aren't any errors. Only "ok" results are
	logged!
	
	After that, the function checks the type of results. The 
	returned results can be from response methods (such as
	"sendMessage" & "sendDocument") or from "updates".
	Since the result structure of these groups are somewhat
	different, the function treats them separately. The function
	"BotGetFile" also produces its results as an association
	which will NOT be loged using this function, but by itself.
	
	Each returned result, contains some general info (like the
	info about the sender) and some specific info (like the id
	of the sent file). The important stuff in the results are
	gathered and appended to a local variable named "assoc".
	
	The info in the "updates" (obtained by either "getUpdates"
	method or "webhook") can contain several updates, so there
	is a DO loop for that call.
	
	The "assoc" is then appended to the file specified by the
	input variable "logPath".
	
	This function isn't prepared for logging messages in groups *)
	Block[
		{botLog,updateID,answered,prefix,datePrefix,botID,botUser,
		targetID,targetFirst,targetLast,targetUser,unixDate,messageID,
		keys,text,fileName,fileID,latitude,longitude,contFirst,
		contLast,contPhone,caption,reply,assoc,
		temp,allLogged,startIndex,lastID,newIDs,
		err1,err2},
		
		err1="Message type not recognized!";
		err2="Bad input, or not \"ok\" results.";
		
		updateID=0;
		answered=False;
		
		If[
			(* Only OK results are logged! *)
			KeyExistsQ[res,"ok"]&&res["ok"]==True&&
				0<Length[res["result"]],
				
			If[
				(* This block checks to see weather a new log
				has to be created. This kind of error checking
				should be done in higher level functions like
				"BotLogImport" & "BotProcessMessage". *)
				FileExistsQ[logPath]==False,
				
				Quiet[CreateDirectory[DirectoryName[logPath]]];
				CreateFile[logPath];
				botLog={},
				
				Quiet[
					Check[
						botLog=Import[logPath,"RawJSON"],
						
						botLog={}
					]
				]
			];
			(* By now, the "botLog" variable is either {}, or contains
			a list of previously logged results *)
			
			keys=Keys[res["result"]];
			
			If[
				MemberQ[Flatten[keys],"update_id"]==False,
				(* Means that the result is obtained from a
				   response method (like "sendMessage"; else,
				   the result is for "updates". *)
				
				(* Logging the results of response methods *)
				(* General info *)
				botID=res["result","from","id"];
				botUser=res["result","from","username"];
				targetID=res["result","chat","id"];
				targetFirst=res["result","chat","first_name"];
				
				(* The user may or may not have "last_name" and "username" *)
				If[
					KeyExistsQ[res["result","chat"],"last_name"],
					targetLast=res["result","chat","last_name"],
					targetLast=""
				];
				If[
					KeyExistsQ[res["result","chat"],"username"],
					targetUser=res["result","chat","username"],
					targetUser=""
				];
				unixDate=res["result","date"];
				messageID=res["result","message_id"];
				
				assoc=<|"bot_id"->botID,"bot_username"->botUser,
					"to"-><|"id"->targetID,"first_name"->targetFirst,
					"last_name"->targetLast,"username"->targetUser|>,
					"date"->unixDate,"message_id"->messageID|>;
					
				(* Message specific info *)
				(* These will be appended to the "assoc" variable
				created above. *)
				Which[
					MemberQ[keys,"text"],
						text=res["result","text"];
						AppendTo[assoc,"text"->text],
						
					MemberQ[keys,"document"],
						fileName=res["result","document","file_name"];
						fileID=res["result","document","file_id"];
						AppendTo[
							assoc,
							"document"-><|"file_name"->fileName,
								"file_id"->fileID|>],
								
					MemberQ[keys,"photo"],
						fileID=res["result","photo"][[-1]]["file_id"];
						AppendTo[assoc,"photo"-><|"file_id"->fileID|>],
						
					MemberQ[keys,"audio"],
						fileID=res["result","audio","file_id"];
						AppendTo[assoc,"audio"-><|"file_id"->fileID|>],
						
					MemberQ[keys,"voice"],
						fileID=res["result","voice","file_id"];
						AppendTo[assoc,"voice"-><|"file_id"->fileID|>],
						
					MemberQ[keys,"video"],
						fileID=res["result","video","file_id"];
						AppendTo[assoc,"video"-><|"file_id"->fileID|>],
						
					MemberQ[keys,"location"],
						latitude=res["result","location","latitude"];
						longitude=res["result","location","longitude"];
						AppendTo[
							assoc,
							"location"-><|"latitude"->latitude,
								"longitude"->longitude|>],
								
					MemberQ[keys,"contact"],
						contFirst=res["result","contact","first_name"];
						contLast=res["result","contact","last_name"];
						contPhone=res["result","contact",
							"phone_number"];
						If[
							Head[contLast]===Missing,
							AppendTo[
								assoc,
								"contact"-><|"first_name"->contFirst,
								"phone_number"->contPhone|>],
							AppendTo[
								assoc,
								"contact"-><|"first_name"->contFirst,
									"last_name"->contLast,
									"phone_number"->contPhone|>]
						],
						
					True,
						err1
				];
				If[
					MemberQ[keys,"caption"],
					caption=res["result","caption"];
					AppendTo[assoc,"caption"->caption]
				];
				If[
					MemberQ[keys,"reply_to_message"],
					reply=res["result","reply_to_message","message_id"];
					AppendTo[assoc,"reply_to_message_id"->reply]
				];
				assoc=Association[assoc/.Association[x__]->
					"call_result"->Association[x]],

				(* Dealing with "updates" *)
				(* 
				New Variables:
				   "allLogged" : Checks if all of the updates are logged.
				   "startIndex": Marks the first update in the received batch
								 of updates that is new and hasn't been logged
								 before.
				   "temp"      : Contains a list of previously logged updates.
								 If there are any updates that haven't been
								 logged yet, this variable will hold the
								 necessary info for each new update in the
								 batch and will add them the the "assoc"
								 variable which is actually a list.
				   "lastID"    : The "update_id" of the last logged update.
				   "newIDs"    : A list of "update_id"s that haven't been
								 logged yet.
				*)
				allLogged=False;
				startIndex=1;
				temp=Select[botLog,KeyExistsQ[#,"update_result"]&];
				If[
					(* Means if there are any update results in the log *)
					0<Length[temp],
					
					lastID=temp[[-1]]["update_result","update_id"];
					newIDs=Map[Key["update_id"],res["result"]];
					If[
						(* Means if some of the results in the arrived batch
						of updates are already logged *)
						MemberQ[newIDs,lastID]&&lastID<Max[newIDs],
						
						startIndex=Position[newIDs,lastID][[1,1]]+1;
						If[
							Length[keys]<startIndex,
							allLogged=True
						],
						If[
							Max[newIDs]<=lastID,
							allLogged=True	
						]
					]
				];
				
				(* If the user edits his or her messages, Telegram creates
				a new update that has different key names, for example:
				"edited_message" instead of "message" or an additional
				"edit_date" key along with the "date" key. Because of this,
				the variables "prefix" and "datePrefix" are necessary. *)
				If[
					allLogged==False,
					Do[
						prefix="";
						datePrefix="";
						updateID=res["result"][[i]]["update_id"];
						If[
							KeyExistsQ[res["result"][[i]],prefix<>"message"],
							
							keys=Keys[res["result"][[i]][prefix<>"message"]],
							
							prefix="edited_";
							datePrefix="edit_";
							keys=Keys[res["result"][[i]][prefix<>"message"]];
						];
						
						(* Logging the update results *)
						(* General info *)
						targetID=res["result"][[i]][prefix<>"message","chat",
							"id"];
						targetFirst=res["result"][[i]][prefix<>"message",
							"chat","first_name"];
						
						(* The user may or may not have "last_name" and
						"username" *)
						If[
							KeyExistsQ[res["result"][[i]][prefix<>"message",
								"chat"],"last_name"],
							targetLast=res["result"][[i]][prefix<>"message",
								"chat","last_name"],
							targetLast=""
						];
						If[
							KeyExistsQ[res["result"][[i]][prefix<>"message",
								"chat"],"username"],
							targetUser=res["result"][[i]][prefix<>"message",
								"chat","username"],
							targetUser=""
						];
						
						(* If the message has been edited by the user, the
						"edit_date" is important, not the old one. *)
						unixDate=res["result"][[i]][prefix<>"message",
							datePrefix<>"date"];
						messageID=res["result"][[i]][prefix<>"message",
							"message_id"];
						
						(* The "answered" will be used in the "BotSelectUpdate"
						and "BotSelectUpdateWebhook" functions *)
						temp=<|"update_id"->updateID,
							"answered"->answered,
							"from"-><|"id"->targetID,
							"first_name"->targetFirst,
							"last_name"->targetLast,
							"username"->targetUser|>,
							"date"->unixDate,"message_id"->messageID|>;
						
						(* Message specific info *)
						Which[
							MemberQ[keys,"text"],
								text=res["result"][[i]][prefix<>"message",
									"text"];
								If[
									MemberQ[keys,"entities"],
									If[
										res["result"][[i]][prefix<>"message",
											"entities"][[1]]["type"]==
											"bot_command",
										AppendTo[temp,"command"->text],
										AppendTo[temp,"text"->text]
									],
									AppendTo[temp,"text"->text]
								],
								
							MemberQ[keys,"document"],
								fileName=res["result"][[i]][prefix<>"message",
									"document","file_name"];
								fileID=res["result"][[i]][prefix<>"message",
									"document","file_id"];
								AppendTo[
									temp,
									"document"-><|"file_name"->fileName,
										"file_id"->fileID|>],
										
							MemberQ[keys,"photo"],
								fileID=res["result"][[i]][prefix<>"message",
									"photo"][[-1]]["file_id"];
								(* When the user sends a photo, telegram
								creates several thumbs and "[[-1]]" means only
								the last of "file_id"s will be collected only
								(which represents the bigges picture. *)
								AppendTo[temp,"photo"-><|
									"file_id"->fileID|>],
									
							MemberQ[keys,"audio"],
								fileID=res["result"][[i]][prefix<>"message",
									"audio","file_id"];
								AppendTo[temp,"audio"-><|
									"file_id"->fileID|>],
									
							MemberQ[keys,"voice"],
								fileID=res["result"][[i]][prefix<>"message",
									"voice","file_id"];
								AppendTo[temp,"voice"-><|
									"file_id"->fileID|>],
									
							MemberQ[keys,"video"],
								fileID=res["result"][[i]][prefix<>"message",
									"video","file_id"];
								AppendTo[temp,"video"-><|
									"file_id"->fileID|>],
									
							MemberQ[keys,"location"],
								latitude=res["result"][[i]][prefix<>"message",
									"location","latitude"];
								longitude=res["result"][[i]][prefix<>"message",
									"location","longitude"];
								AppendTo[
									temp,
									"location"-><|"latitude"->latitude,
										"longitude"->longitude|>],
										
							MemberQ[keys,"contact"],
								contFirst=res["result"][[i]][prefix<>"message",
									"contact","first_name"];
								contLast=res["result"][[i]][prefix<>"message",
									"contact","last_name"];
								contPhone=res["result"][[i]][prefix<>"message",
									"contact","phone_number"];
								If[
									Head[contLast]===Missing,
									AppendTo[
										temp,
										"contact"-><|
											"first_name"->contFirst,
										"phone_number"->contPhone|>],
									AppendTo[
										temp,
										"contact"-><|
											"first_name"->contFirst,
											"last_name"->contLast,
											"phone_number"->contPhone|>]
								],
								
							True,
								err1
						];
						If[
							MemberQ[keys,"reply_to_message"],
							reply=res["result"][[i]][prefix<>"message",
								"reply_to_message","message_id"];
							AppendTo[temp,"reply_to_message_id"->reply]
						];
						temp=Association[temp/.Association[x__]->
							"update_result"->Association[x]];
						
						If[
							i==startIndex,
							assoc={temp},
							AppendTo[assoc,temp]
						],
						{i,startIndex,Length[keys]}
					],
					
					assoc={};
				];
				
			];
			
			(* Writing the results *)
			Export[logPath,Flatten[Append[{botLog},assoc]],"JSON"];
			assoc,
			err2
		]
	]


BotCall[bot_Association,method_String,args_List:{},logPath_String:"",
	timeOut:(_?NumericQ):15]:=
	(* This funtion is for sending messages to user, getting updates
	from the Telegram API and setting webhooks. *)
	Block[
		{token,url,res,err1,err2},
		
		err1="ERROR: Bot token not found!";
		
		token=bot["Token"];
		If[
			Head[token]===Missing,
			res=err1,
			url=URLBuild[
				{"https://api.telegram.org/bot"<>token,method},
				args];
			Quiet[
				Check[
					res=URLExecute[url,{},
						"RawJSON",TimeConstraint->timeOut],
					res=<||>
				]
			];
			If[
				KeyExistsQ[res,"ok"]&&res["ok"]==True,
				If[
					logPath=="",
					res,
					res=BotLog[res,logPath]
				],
				If[
					KeyExistsQ[res,"ok"],
					res=err2="ERROR: "<>res["description"],
					res=err2="ERROR: Unsuccessful. Try again!"
				]
			]
		]
	]


BotFileCall[bot_Association,filePath_String,method_String,fileField_String,
	args_List,logPath_String:""]:=
	(* This function if for sending files to the user. Some error checking
	is done beforehand, like checking the size of file which can't be bigger
	than 50mb *)
	Block[
		{token,url,bytes,fileName,res,err1,err2,err3,err4},
		
		err1="Bot token not found!";
		err2="File not found!";
		err3="Can't send a directory!";
		err4="File is too large!";
		
		token=bot["Token"];
		If[
			Head[token]===Missing,
			
			res=err1,
			
			If[
				FileExistsQ[filePath]==False,
				
				res=err2,
				
				If[
					FileFormat[filePath]=="Directory",
					
					res=err3,
					
					If[
						50000000<FileByteCount[filePath],
					
						res=err4,
					
						bytes=Import[filePath,"Byte"];
						fileName=FileBaseName[filePath]<>"."
							<>FileExtension[filePath];
						url=URLBuild[
							{"https://api.telegram.org/bot"<>token,method},
							args];
						res=URLExecute[
							url,
							{},
							"RawJSON",
							<|"Method"->"POST",
								"MultipartElements"->{{fileField<>
									"\";filename=\""<>fileName,
									"application/octet-stream",bytes}},
								"Headers"->{"Accept"->
									"application/json;charset=UTF-8",
									"Content-Type"->"multipart/form-data"}|>];
					
						If[
							KeyExistsQ[res,"ok"]&&res["ok"]==True,
							If[
								logPath=="",
								res,
								res=BotLog[res,logPath]
							],
							If[
								KeyExistsQ[res,"ok"],
								res=err4="ERROR: "<>res["description"],
								res=err4="ERROR: Unsuccessful. Try again!"
							]
						]
					]
				]
			]
		]
	]


BotGetFile[bot_Association,fileID_String,saveDir_String,logPath_String:""]:=
	(* If the users sends a file, this function can be used to get it. It
	first calls Telegram API with the "file_id" to receive the path to the
	file in the Telegram server and then downloads it. *)
	Block[
		{token,url,res,filePathTelegram,fileName,fileType,fileSize,err1,err2},
		
		err1="Bot token not found!";
		err2="Download unsuccessful :-( Check the parameters!";
		
		token=bot["Token"];
		If[
			Head[token]===Missing,
			
			err1,
			
			res=BotCall[bot,"getFile",{"file_id"->fileID}];
			If[
				Head[res]===Association,
			
				filePathTelegram=res["result","file_path"];
				(* The "file_path" returned by the Telegram 
				API is something like
				"documents/file_3.pdf"
				or
				"videos/file_1.mp4"
				
				So the file type can be extracted from it *)
				
				fileType=FileNameSplit[filePathTelegram][[1]];
				If[
					StringTake[fileType,-1]=="s",
					fileType=StringDrop[fileType,-1]
				];

				fileSize=res["result","file_size"];
				url=URLBuild[
					{"https://api.telegram.org/file/bot"<>token,
					filePathTelegram}];
				
				Quiet[CreateDirectory[saveDir]];
			
				fileName=FileNameJoin[{saveDir,
					DateString[{"Year","Month","Day",
						"Hour","Minute","Second"}]<>
					"_"<>FileNameSplit[filePathTelegram][[2]]}];
				
				res=URLDownload[url,fileName];
				
				If[
					(* If the extension of the file isn't shown in the
					"filePathTelegram" variable by Telegram, rename the
					file based on its format. *)
					StringContainsQ[
						FileNameSplit[fileName][[-1]],"."]==False,
					
					RenameFile[fileName,fileName<>"."<>
						ToLowerCase[FileFormat[fileName]]];
					fileName=fileName<>"."<>ToLowerCase[FileFormat[fileName]]
				];
				
				If[
					Head[res]===File,
					res=<|"getfile_result"-><|"file_type"->fileType,
						"file_id"->fileID,
						"file_size"->FileByteCount[fileName],
						"file_path"->fileName|>|>,
					err2
				];
				
				(* Logging for this function is done here and not by the
				"BotLog" function. *)
				If[
					logPath=="",
					
					res,
					
					If[
						FileExistsQ[logPath]==False,
						
						CreateFile[logPath];
						Export[logPath,res,"JSON"];
						res,
						
						Quiet[
							Check[
								Export[logPath,Append[Flatten[
									{Import[logPath,"RawJSON"]}],res],"JSON"];
								res,
								
								Export[logPath,res,"JSON"];
								res
							]
						]
					]
				]
			]
		]
	]


BotLogImport[bot_Association,logPath_String,pollTime_:0]:=
	(* This function imports the bot's log from a file. Basically
	all it does is checking for errors that could be faced when
	working with files.
 
	This function is only used when the bot gets new updates from
	Telegram by "getUpdates" method.
	
	If there's no log, or there's nothing useful in it, the
	function uses a "getupdate" method and puts the results in
	the log and then imports it!

	A special case is when the bot has answered all the updates
	and there are no new ones. To avoid sending too many API
	calls, a pollTime is introduced! *)
	Block[
		{botLog},
		
		botLog={};
		
		While[
			Length[botLog]==0,
			Quiet[
				Check[
					botLog=Import[logPath,"RawJSON"],
					
					BotCall[bot,"getUpdates",logPath];
					Pause[pollTime]
				]
			]
		];
		botLog
	]


BotSelectUpdate[bot_Association,logPath_String,pollTime_:0]:=
	(* This function is used only when the bot uses "getUpdates"
	method to obtain new updates. In this function, the imported
	bot's log is checked to see if there is any unanswered updates:

	1. If found any, the first one is selected to be processed.
	2. If none are found, it means that all updates are answered
	   or there are no "getupdate_result" in the log:
   
	   A. If all updates are answered, the Telegram API is 
	      notified by sending a "getUpdates" call with "offset"
	      greater than the latest "update_id" by 1. All updates
	      with less "update_id"s will be dismissed from future
	      "getUpdate" calls. Also, a pollTime is introduces to
	      avoid sending too many calls.
	   B. If there are no "getupdate_result" in the log file at
	      all, a "getUpdate" call is issued. If there are any
	      results, they will be logged into the log file, unless
		      there are no new updates, in which case after a
	      "pollTime", the whole process is repeated! *)
	Block[
		{index,botLog,temp,offset},
		
		index=0;
		
		While[
			index==0,
	
			botLog=BotLogImport[bot,logPath,pollTime];
			temp=Select[botLog,#["update_result","answered"]==False&];	
			If[
				0<Length[temp],
	
				temp=First[temp];
				index=Position[botLog,temp][[1,1]],
	
				temp=Select[botLog,#["update_result","answered"]==True&];
				If[
					0<Length[temp],
		
					temp=Last[temp];
					offset=temp["update_result","update_id"]+1;
					BotCall[bot,"getUpdates",{"offset"->offset},logPath];
					Pause[pollTime],
			
					BotCall[bot,"getUpdates",logPath];
					Pause[pollTime]
				]
			]
		];
		{botLog,index}
	]


BotSelectUpdateWebhook[webhookUpdates_Association,logPath_String]:=
	(* If the method of getting new updates is by webhook, this 
	function will be used to import un-answered updates, instead
	of "BotSelectUpdate". Obviously, there is no LOOP in this
	function.
	
	The function first loads old logs and then appends new updates
	to it using the "BotLog" function. A note here:
	The new updates delivered by Telegram API don't contain the
	status of the update (the "ok"\[Rule]True part) and so for the
	"BotLog" function to be able to process them, new updates are
	changed a bit, first.
	
	From the combined new and old updates, the first unanswered one
	is selected (using "index" variable). If there are none, "index"
	will be 0. Zero index in the "BotProcessMessage" function is
	considered as "botStop=True" which will terminate that function *)
	Block[
		{index,botLog,temp,newUpdates},
		
		index=0;
		
		Quiet[
			Check[
				botLog=Import[logPath,"RawJSON"],
				
				botLog={}
			]
		];
		
		If[
			KeyExistsQ[webhookUpdates,"update_id"],
			
			newUpdates=<|"ok"->True,"result"->{webhookUpdates}|>;
			botLog=Flatten[
				Append[{botLog},{BotLog[newUpdates,logPath]}]
			]
		];
		
		temp=Select[botLog,#["update_result","answered"]==False&];
		
		If[
			0<Length[temp],

			temp=First[temp];
			index=Position[botLog,temp][[1,1]],

			botLog={};
			index=0
		];
		
		{botLog,index}
	]


BotUserPrivilege[bot_Association,userID_Integer]:=
	(* This function checks the user's privilege or clearance based
	on the info on the bot's config file *)
	Block[
		{token,err1},
		
		err1="ERROR: Bot token not found!";
		
		token=bot["Token"];
		If[
			Head[token]===Missing,
			err1,
			Which[
				MemberQ[bot["AdminList"],userID,2],
					2,
					
				MemberQ[bot["MemberList"],userID,2],
					1,
					
				True,
					0
			]
		]
	]
BotUserPrivilege[bot_Association,username_String]:=
	(* Overloading the function to accept usernames as well as userIDs *)
	Block[
		{token,err1},
		
		err1="ERROR: Bot token not found!";
		
		token=bot["Token"];
		If[
			Head[token]===Missing,
			err1,
			Which[
				MemberQ[bot["AdminList"],username,2],
					2,
					
				MemberQ[bot["MemberLisfft"],username,2],
					1,
					
				True,
					0
			]
		]
	]


BotKeyboardArray[keys_List,oneTime:(0|1):0,selective:(0|1):0,remove:(0|1):0]:=
	(* Sometimes bot needs to create custom keyboards for the user. Keyboards
	are arrays of arrays of text like {{"a"},{"b"}} which creates to buttons
	for the keyboard below each other, or like {{"a","b"}}, in which case the
	buttons will be next to each other.
	Binary options can be passed into this fuction for various purposes:
	
	"oneTime"  : (Default=0), The keyboard will hide after the user clicks a
							  button (but still exists and can be accessed).
	"selective": (Default=0), The keyboard will be shown to a specific user.
	"remove"   : (Default=0), Ignores previous arguments of the function and
							  removes the keyboard completely *)
	If[
		remove==1,
		
		"{\"remove_keyboard\":true}",
		
		"{\"keyboard\":"<>
		StringReplace[
			FromCharacterCode[
				ToCharacterCode[
					ExportString[keys,"RawJSON"]],
					"UTF-8"
				],
			{"\n"->"","\t"->""}
		]<>
		If[
			oneTime==1,
			
			If[
				selective==1,
				
				",\"one_time_keyboard\":true,\"selective\":true}",
				
				",\"one_time_keyboard\":true}"
			],
			If[
				selective==1,
				
				",\"selective\":true}",
				
				"}"
			]
		]
	]


BotCommand[botFile_String,command_Association,followups_Association,
	targetUser_String,targetID_Integer,date_Integer,logPath_String]:=
	(* This function first checks to see if the user has enough clearance
	to send each specific command and if positive, does the job and
	prepares a response to be sent back to user. This function supports
	followups, which indicate that the user might send another input
	regarding the last command.
	
	Use the following lines for setting commands in the "BotFather". Note
	that these are commands which require minimum privilege of 1:
	
	whoami - Shows your clearance
	ping - Test how fast the server is
	music - Generates a random music
	location - How's the weather
	today - Date in Shamsi & Gregorian
	qr - Scan or generate QR codes
	wol - Calculate anything!
	keyboardoff - Remove the custom keyboard

	*)
	Block[
		{cmd,text,followup,minPrivilege,privilege,response,bot,newUser,newID,
		darkskyURL,darkskyResult,windBearing,err1,err2},
		
		err1="Command wasn't recognized or was incomplete. Use /help.";
		err2="You don't have enough clearance for this command :-(";
		
		(* Some commands like "/start", "/accept" & "/decline", need to
		change the bot's config file. Thats why the config file is imported
		here *)
		bot=Import[botFile,"RawJSON"];
		
		cmd=command["command"];
		cmd=ToLowerCase[cmd];
		If[
			KeyExistsQ[command,"text"],
			text=command["text"],
			text=""
		];
		
		(* The "followup" variable contains followups for all users *) 
		followup=followups;
		
		If[
			(* Checking to see if command is like:
			/accept@username
			or
			/decline@username
			and then split it up. *)
			0<Length[StringPosition[cmd,"@"]]&&
			StringPosition[cmd,"@"][[1,1]]<StringLength[cmd],
			
			{cmd,newUser}=StringSplit[cmd,"@"]
		];
		
		minPrivilege=<|"/start"->1,"/whoami"->0,"/ping"->1,"/wol"->1,
			"/wolfram"->1,"/shell"->2,"/open"->2,"/music"->1, "/location"->1,
			"/today"->1,"/qr"->1,"/files"->2,"/dump"->2,"/box"->2,
			"/keyboardoff"->1,"/accept"->2,"/decline"->2,"/temp"->2,
			"/help"->1,"/botstop"->2|>;
			
		If[
			(* For obvious security reasons, users should not be allowed
			to use "/wol" and "/wolfram" commands, when the bot engine isn't
			runnin in the cloud. When running in the cloud however, the
			security measures can be dialed down a bit! *)
			$CloudEvaluation==True,
	
			minPrivilege["/wol"]=1;
			minPrivilege["/wolfram"]=1
		];
		
		privilege=BotUserPrivilege[bot,targetID];
		
		Which[
			cmd=="/start",
				If[
					minPrivilege[cmd]<=privilege,
					response="Up and running, how can I /help you?",
					
					If[
						targetUser!="",
						
						If[
							MemberQ[bot["BlackList"],targetUser,2],
							
							response="You can't register here, sorry :-(",
							
							If[
								MemberQ[bot["TempList"],targetUser,2]==False,
								AppendTo[bot["TempList"],
									{targetUser,targetID}];
								DeleteDuplicates[bot["TempList"]];
								Export[botFile,bot,"JSON"]
							];
							(* Informing the admins about the new member! *)
							BotCall[bot,"sendMessage",
								{"chat_id"->bot["AdminList"][[All,2]],
								"text"->"@"<>targetUser<>
									" wants to be a member of this "<>
									"bot."<>"\n"<>"/accept@"<>targetUser<>
									" or /decline@"<>targetUser},
								logPath];
							response="You're not a member yet! a request "<>
								"to my admin has been sent to be considered"
						],
						
						response="Looks like your Telegram accound doesn't "<>
							"have a username, so you can't register here :-("
					]
				];
				response=<|"text"->response,
					"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
				followup[targetID]={},
				
			cmd=="/accept",
				If[
					minPrivilege[cmd]<=privilege,
					
					If[
						MemberQ[bot["TempList"],newUser,2],
						
						newID=Cases[bot["TempList"],{newUser,_}][[1,2]];
						BotCall[bot,"sendMessage",{"chat_id"->newID,
								"text"->"You are now a member! Welcome :-)"<>
									"\n"<>
									"Use /help or check your membership "<>
									"status with /whoami"},
								logPath];
						bot["TempList"]=DeleteCases[bot["TempList"],
							{newUser,_}];
						AppendTo[bot["MemberList"],{newUser,newID}];
						DeleteDuplicates[bot["MemberList"]];
						Export[botFile,bot,"JSON"];
						response="Tnx, @"<>newUser<>" is now a member!",
						
						response=err1
					],
					
					response=err2
				];
				response=<|"text"->response,
					"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
				followup[targetID]={},
				
			cmd=="/decline",
				If[
					minPrivilege[cmd]<=privilege,
					
					If[
						MemberQ[bot["TempList"],newUser,2],
						
						newID=Cases[bot["TempList"],{newUser,_}][[1,2]];
						BotCall[bot,"sendMessage",{"chat_id"->newID,
								"text"->"Your request has been declined :-("},
								logPath];
						bot["TempList"]=DeleteCases[bot["TempList"],
							{newUser,_}];
						AppendTo[bot["BlackList"],{newUser,newID}];
						DeleteDuplicates[bot["BlackList"]];
						Export[botFile,bot,"JSON"];
						response="OK, @"<>newUser<>" was added to BlackList!",
						
						response=err1
					],
					
					response=err2
				];
				response=<|"text"->response,
					"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
				followup[targetID]={},
				
			cmd=="/temp",
				If[
					minPrivilege[cmd]<=privilege,
					
					If[
						0<Length[bot["TempList"]],
						
						response="There are "<>
							ToString[Length[bot["TempList"]]]<>
							" users in the waiting list:"<>"\n\n"<>
							StringJoin[Flatten[Thread[List[Map[
								StringJoin["@",#]&,bot["TempList"][[All,1]]],
								"\n",
								Map[StringJoin["/accept@",#]&,
									bot["TempList"][[All,1]]],
								" or ",
								Map[StringJoin["/decline@",#]&,
									bot["TempList"][[All,1]]],
								"\n\n"]]]],
						
						response="The waiting list is empty!"
					],
					
					response=err2
				];
				response=<|"text"->response,
					"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
				followup[targetID]={},
				
			cmd=="/whoami",
				If[
					minPrivilege[cmd]<=privilege,
					Which[
						privilege==2,
							response="Admin!",
						privilege==1,
							response="Member!",
						privilege==0,
							response="I don't know you!"
					],
					response=err2
				];
				response=<|"text"->response,
					"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
				followup[targetID]={},
				
			cmd=="/ping",
				If[
					minPrivilege[cmd]<=privilege,
					response="pong: "<>ToString[UnixTime[]-date]<>" s",
					response=err2
				];
				response=<|"text"->response,
					"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
				followup[targetID]={},
				
			cmd=="/wol"||cmd=="/wolfram",
				If[
					minPrivilege[cmd]<=privilege,
					If[
						text!="",
						
						If[
							SyntaxQ[text],
							
							response=ToExpression[text];
							followup[targetID]={},
							
							response=<|"text"->"Syntax is wrong! Try again.",
								"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
							followup[targetID]={"/wol_text"}
						],
						
						response=<|"text"->err1,
							"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
						followup[targetID]={"/wol_text"}
					],
					response=<|"text"->err2,
						"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
					followup[targetID]={}
				],
				
			cmd=="/shell",
				If[
					minPrivilege[cmd]<=privilege,
					
					If[
						$OperatingSystem=="MacOSX"&&
						$CloudEvaluation==False&&
						text!="",
						
						Which[
							text=="Turn off server's display",
								RunProcess[$SystemShell,"StandardOutput",
									"pmset displaysleepnow"<>"\n"<>
									"exit"<>"\n"];
								response=<|"text"->"Done!",
									"keyboard"->BotKeyboardArray[{{}},0,0,1]|>,
							
							StringContainsQ[text,"Server screenshot"],
								(* The "text" variable in this case, has the
								following format:
								"Server screenshot@saveDir"
								The "saveDir" part which is the location of
								screenshot will be extracted here. *)
								response=FileNameJoin[
									{StringSplit[text,"@"][[-1]],
									DateString[{"Year","Month","Day",
										"Hour","Minute","Second"}]<>".jpg"}];
								If[
									StringTake[response,2]=="~/",
									
									response=StringInsert[response,"\"",3]<>
										"\"",
									
									If[
										StringTake[response,6]=="/Users",
										
										response=StringInsert[response,
											"\"",2]<>"\""
									]
								];
								RunProcess[$SystemShell,
									"StandardOutput","screencapture -x -C "<>
									response<>"\n"<>"exit"<>"\n"];
								response=StringReplace[response,"\""->""];
								response=<|"filePath"->response,
									"keyboard"->BotKeyboardArray[{{}},0,0,1]|>,
								
							True,
								response=RunProcess[$SystemShell,
									"StandardOutput",text<>"\n"<>"exit"<>"\n"];
								response="```"<>"\n"<>response<>"\n"<>"```";
								response=<|"text"->response,
									"parse_mode"->"Markdown",
									"keyboard"->BotKeyboardArray[{{}},0,0,1]|>
						];
						followup[targetID]={},
						
						If[
							$OperatingSystem=="MacOSX"&&
							$CloudEvaluation==False&&
							text=="",
						
							response=<|"text"->"Enter a shell command or "<>
								"select an item \[DownArrow]",
								"keyboard"->BotKeyboardArray[
									{{"Server screenshot"},
									{"Turn off server's display"},
									{"/keyboardoff"}},1,1,0]|>;
							followup[targetID]={"/shell_text"},

							response=<|"text"->err1,
								"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
							followup[targetID]={}
						]
					],
					
					response=<|"text"->err2,
						"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
					followup[targetID]={}
				],
				
			cmd=="/open",
				If[
					minPrivilege[cmd]<=privilege,
					If[
						$OperatingSystem=="MacOSX"&&
						$CloudEvaluation==False&&
						text!="",
						
						Which[
							(* The "text" variable either starts with "~/" or
							"/Users/" which means the user wants to open a
							file, or doesn't start with these strings, which
							means there user tries to open an application. 
							
							The paths of files and folders that have
							whitespace in their names should have quotation
							marks, so they can be treated using system shell *)
							StringTake[text,2]=="~/",
								text=StringInsert[text,"\"",3]<>"\"";
								RunProcess[$SystemShell,
									"StandardOutput","open "<>text<>"\n"<>
									"exit"<>"\n"],
							
							StringTake[text,6]=="/Users",
								text=StringInsert[text,"\"",2]<>"\"";
								RunProcess[$SystemShell,
									"StandardOutput","open "<>text<>"\n"<>
									"exit"<>"\n"],
							
							True,
								(* Some apps have whitespace in their names.
								the next 2 If blocks will take care of that! *)
								If[
									StringTake[text,1]!="\"",
									text="\""<>text
								];
								If[
									StringTake[text,-1]!="\"",
									text=text<>"\""
								];
								RunProcess[$SystemShell,
									"StandardOutput","open -a "<>text<>
									"\n"<>"exit"<>"\n"];
								
						];
						response=<|"text"->"Done!",
							"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
						followup[targetID]={},
						
						If[
							$OperatingSystem=="MacOSX"&&
							$CloudEvaluation==False,
							
							SetDirectory["/Applications"];
							response=Map[
								List,
								Join[
									{"/keyboardoff"},
									Map[FileBaseName,FileNames["*.app"]]]];
							response=<|"text"->"Select an app \[DownArrow], or enter "<>
								"the path to your file.",
								"keyboard"->BotKeyboardArray[
									response,1,1,0]|>;
							followup[targetID]={"/open_text"};
							SetDirectory[$HomeDirectory],
							
							response=<|"text"->err1,
								"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
							followup[targetID]={}
						]
					],
					
					response=<|"text"->err2,
						"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
					followup[targetID]={}
				],
				
			cmd=="/music",
				If[
					minPrivilege[cmd]<=privilege,
					
					(* The next "If" block can be removed! *)
					If[
						(* Doing some stuff to control iTunes *)
						($OperatingSystem=="MacOSX"&&privilege==2&&text!="")||
						(privilege==2&&text=="generate music"),
						
						text=ToLowerCase[text];
						Which[
							text=="on"||text=="start"||text=="play",
								RunProcess[$SystemShell,
									"StandardOutput",
									"osascript -e 'tell "<>
									"application \"iTunes\" to play'"<>
									"\n"<>"exit"<>"\n"];
								response="";
								followup[targetID]={"/music_text"},
							
							text=="pause",
								RunProcess[$SystemShell,
									"StandardOutput",
									"osascript -e 'tell "<>
									"application \"iTunes\" to pause'"<>
									"\n"<>"exit"<>"\n"];
								response="";
								followup[targetID]={"/music_text"},
							
							text=="\:23f9"||text=="stop"||text=="off",
								RunProcess[$SystemShell,
									"StandardOutput",
									"osascript -e 'tell "<>
									"application \"iTunes\" to stop'"<>
									"\n"<>"exit"<>"\n"];
								response="";
								followup[targetID]={"/music_text"},
							
							text=="\:23ef",
								response=RunProcess[$SystemShell,
									"StandardOutput",
									"osascript -e 'tell "<>
									"application \"iTunes\" to get player "<>
									"state'"<>
									"\n"<>"exit"<>"\n"];
								If[
									response=="playing\n",
									
									RunProcess[$SystemShell,
										"StandardOutput",
										"osascript -e 'tell "<>
										"application \"iTunes\" to pause'"<>
										"\n"<>"exit"<>"\n"],
									
									RunProcess[$SystemShell,
										"StandardOutput",
										"osascript -e 'tell "<>
										"application \"iTunes\" to play'"<>
										"\n"<>"exit"<>"\n"]
								];
								response="";
								followup[targetID]={"/music_text"},
							
							text=="\:23e9"||text=="n"||text=="next"||
							text=="next track",
								RunProcess[$SystemShell,
									"StandardOutput",
									"osascript -e 'tell "<>
									"application \"iTunes\" to "<>
									"next track'"<>
									"\n"<>"exit"<>"\n"];
								response="";
								followup[targetID]={"/music_text"},
								
							text=="\:23ea"||text=="p"||text=="prev"||
							text=="previous track",
								RunProcess[$SystemShell,
									"StandardOutput",
									"osascript -e 'tell "<>
									"application \"iTunes\" to "<>
									"previous track'"<>
									"\n"<>"exit"<>"\n"];
								response="";
								followup[targetID]={"/music_text"},
							
							StringContainsQ[text,"vol"],
								text=StringSplit[text," "][[-1]];
								Which[
									text=="0",
										text="0",
										
									text=="40",
										text="3",
										
									text=="60",
										text="4",
										
									text=="100",
										text="7",
										
									True,
										text=ToString[
											ToExpression[text]*7./100]
								];
								
								RunProcess[$SystemShell,
									"StandardOutput",
									"osascript -e 'tell application "<>
									"\"iTunes\" to set sound volume to 100'"<>
									"\n"<>"exit"<>"\n"];
								
								RunProcess[$SystemShell,
									"StandardOutput",
									"osascript -e \"set Volume "<>text<>"\""<>
									"\n"<>"exit"<>"\n"];
								
								response="";
								followup[targetID]={"/music_text"},
							
							text=="generate music",
								response=Sound[Play[Sin[# 2Pi t],{t,0,0.5},
									SampleRate->2000]&/@
									RandomChoice[({1,9/8,5/4,4/3,3/2,
									5/3,15/8,2/1}*440*3/5),20]];
								response=<|"audio"->response,
									"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
								followup[targetID]={},
								
							True,
								response=<|"text"->err1,
									"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
								followup[targetID]={}
						],
						
						Which[
							$OperatingSystem=="MacOSX"&&
							$CloudEvaluation==False&&
							privilege==2&&
							text=="",
								response=<|"text"->"Let's ROCK!",
									"keyboard"->BotKeyboardArray[
										{{"\:23ea","\:23ef","\:23f9","\:23e9"},
										{"vol 0","vol 40","vol 60","vol 100"},
										{"generate music","/keyboardoff"}}
										,0,1,0]|>;
								followup[targetID]={"/music_text"},
							
							($OperatingSystem!="MacOSX"||
								$CloudEvaluation==True)&&
							privilege==2&&
							text=="",
								response=<|"text"->"Music Time!",
									"keyboard"->BotKeyboardArray[
										{{"generate music","/keyboardoff"}}
										,0,1,0]|>;
								followup[targetID]={"/music_text"},
							
							privilege==1,
								response=Sound[Play[Sin[# 2Pi t],{t,0,0.5},
									SampleRate->2000]&/@
									RandomChoice[({1,9/8,5/4,4/3,3/2,
									5/3,15/8,2/1}*440*3/5),20]];
								response=<|"audio"->response,
									"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
								followup[targetID]={},
							
							True,		
								response=<|"text"->err2,
									"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
								followup[targetID]={}
						]
					],
					response=<|"text"->err2,
						"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
					followup[targetID]={}
				],
				
			cmd=="/location",
				(* Providing the user with some predefined
				location-based information. This command
				uses data from Wolfram and DarkSky.net *)
				If[
					minPrivilege[cmd]<=privilege,
					If[
						KeyExistsQ[command,"data"],
						
						BotCall[bot,"sendChatAction",{"chat_id"->targetID,
							"action"->"find_location"}];
						text=GeoPosition[{command["data","latitude"],
							command["data","longitude"]}];
						response="Your Geoposition is: "<>ToString[text[[1]]]<>
						"\n\n";
						If[
							GeoWithinQ[text,GeoNearest["City",text][[1]]],
							
							response=response<>"You're in "<>
							EntityValue[GeoNearest["City",text][[1]],"Name"]<>
								"\n",
							
							response=response<>"The nearest city is "<>
							EntityValue[GeoNearest["City",text][[1]],"Name"]<>
								", "<>
							EntityValue[GeoNearest["Country",text][[1]],
								"Name"]<>
							" (about "<>ToString[Round[
								QuantityMagnitude[
									GeoDistance[
										text,
										GeoNearest["City",text][[1]]
									],
									"Kilometers"],
								.1]]<>" km away)"<>"\n"
						];
						response=response<>"Elevation: "<>
							ToString[
								Round[
									QuantityMagnitude[
										GeoElevationData[text],
										"Meters"
									],
								1]
							]<>" m"<>"\n\n";
						
						If[
							(* The Dark Sky API has a free plan, which
							allows 1000 call per day. This block checks if
							the last call was made in the previous day.
							
							"FromUnixTime" returns a "DateObject", in
							which the first 3 parts are {y,m,d}. *)
							DateList[FromUnixTime[date]][[1;;3]]=!=
								DateList[FromUnixTime[bot["DarkSkyAPI",
									"LastCallDate"]]][[1;;3]],
							
							bot["DarkSkyAPI","TotalAPICalls"]=0;
							bot["DarkSkyAPI","LastCallDate"]=date
						];
						
						If[
							(* If total API calls is more than a threshold,
							user "WeatherData" function, instead of the
							Dark Sky API. *)
							bot["DarkSkyAPI","TotalAPICalls"]<990,
							
							darkskyURL=URLBuild[
								{"https://api.darksky.net/forecast",
								bot["DarkSkyAPI","Token"],
								ToString[text[[1,1]]]<>","<>
								ToString[text[[1,2]]]},
								{"units"->"ca",
								"exclude"->"minutely,hourly,flags"}];
								(* The "ca" in the "unit" variable means that
								the quantities are reported in SI system,
								except for the windspeed, which will be
								reported in km/h *)
							
							darkskyResult=URLExecute[darkskyURL,{},"RawJSON"];
							
							response=response<>"Weather Summary:"<>"\n\n"<>
								"Condition: "<>"\n\n"<>
								"Now: "<>
								darkskyResult["currently","summary"]<>
								"\n"<>"Today: "<>
								darkskyResult["daily",
									"data"][[1]]["summary"]<>"\n\n"<>
								"T: "<>
								ToString[
									Round[
										darkskyResult["currently",
											"temperature"],
										1
									]
								]<>" \[Degree]C"<>
								If[
									(* The API has an "apparentTemperature"
									variable. If it differs significantly
									from the true temperature, it is also
									reported. *)
									KeyExistsQ[darkskyResult["currently"],
										"apparentTemperature"],
								
									If[
										5<Abs[darkskyResult["currently",
											"apparentTemperature"]-
											darkskyResult["currently",
												"temperature"]],
									
										"\n"<>"Feels like "<>
										ToString[
											Round[
												darkskyResult["currently",
													"apparentTemperature"],
												1
											]
										]<>" \[Degree]C",
									
										""
									]
								]<>"\n\n"<>
								"P: "<>
								ToString[
									Round[
										darkskyResult["currently",
											"pressure"],
										1
									]
								]<>" millibars"<>"\n\n"<>
								"Wind: "<>
								ToString[
									Round[
										darkskyResult["currently",
											"windSpeed"],
										1
									]
								]<>" km/h ";
								If[
									KeyExistsQ[darkskyResult["currently"],
										"windBearing"],
								
									windBearing=darkskyResult["currently",
										"windBearing"];
									response=response<>"from "<>
									Which[
										(* The API returns the wind direction
										in degrees from the true north. This
										Which block converts it to more
										sensible directions *)
										0<windBearing<=45,
											"N-NE",
										45<windBearing<=90,
											"NE-E",
										90<windBearing<=135,
											"E-SE",
										135<windBearing<=180,
											"SE-S",
										180<windBearing<=225,
											"S-SW",
										225<windBearing<=270,
											"SW-W",
										270<windBearing<=315,
											"W-NW",
										315<windBearing<=359,
											"NW-N"
									]
								];

							response=response<>
								"\n"<>"Precipitation chance: "<>
								ToString[
									Round[
										100*darkskyResult["currently",
											"precipProbability"],
										1
									]
								]<>"%"<>"\n\n"<>
								"Next 7 days: "<>"\n"<>
								darkskyResult["daily","summary"]<>
							
								If[
									(* Supporting weather alerts! *)
									KeyExistsQ[darkskyResult,"alerts"],
								
									"\n\n"<>"----------------"<>"\n"<>
									"Weather ALERT!!!"<>
									Table[
										"\n\n"<>
										darkskyResult["alerts"][[i]]["uri"],
										{i,1,Length[darkskyResult["alerts"]]}
									]<>"\n"<>"----------------",
								
									""
								]<>
							
								"\n\n"<>"Nearest weather station is "<>
								ToString[Round[
									QuantityMagnitude[
									GeoDistance[
										text,
										GeoNearest["WeatherStation",text][[1]]
									],
									"Kilometers"],
								.1
								]]<>" km away."<>"\n\n"<>
								"[Powered by Dark Sky]"<>
								"(https://darksky.net/poweredby/)";
							
							response=<|"text"->response,
								"parse_mode"->"Markdown",
								"disable_web_page_preview"->True,
								"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
							
							(* Change the configFile to account for the new
							Dark Sky API call *)
							bot["DarkSkyAPI","TotalAPICalls"]+=1;
							Export[botFile,bot,"JSON"],
						
							response=response<>"T: "<>
								ToString[WeatherData[text,"Temperature"]]<>
								"\n"<>"P: "<>
								ToString[WeatherData[text,"Pressure"]]<>
								"\n"<>"Nearest weather station is "<>
								ToString[Round[
									QuantityMagnitude[
										GeoDistance[
											text,
											GeoNearest["WeatherStation",
												text][[1]]
										],
										"Kilometers"],
									.1
								]]<>" km away.";
							response=<|"text"->response,
								"keyboard"->BotKeyboardArray[{{}},0,0,1]|>
						];
						followup[targetID]={},
						
						response=<|"text"->"OK, send me your location",
							"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
						followup[targetID]={"/location_location"}
					],
					response=<|"text"->err2,
							"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
					followup[targetID]={}
				],
				
			cmd=="/today",
				If[
					minPrivilege[cmd]<=privilege,
					
					response=ToShamsi[
						DateList[
							FromUnixTime[date]
						][[1;;3]]
					];
					response=DateString[response,
						{"Shamsi: ","Year","/","Month","/","Day"}]<>
						"\n"<>DateString[DateList[],
							{"Gregorian: ","Year","/","Month","/","Day"}]<>
						"\n\n"<>"Have a nice day :-)",
					
					response=err2
				];
				response=<|"text"->response,
					"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
				followup[targetID]={},
				
			cmd=="/qr",
				If[
					minPrivilege[cmd]<=privilege,
					
					If[
						KeyExistsQ[command,"data"],
						
						Which[
							Head[command["data"]]===Image,
								response=BarcodeRecognize[command["data"],
									{"Data","Format"}];
								If[
									response=={},
									response="Scan unsuccessful :-(",
									response="Format: "<>response[[2]]<>
									"\n\n"<>"Data:"<>"\n"<>response[[1]]
								],
								
							(* QR codes can be made from other file types
							as well, like contacts. Add them here and in
							the followup and the end of the next If block. *)
								
							True,
								response=err1
						];
						response=<|"text"->response,
							"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
						followup[targetID]={},
								
						If[
							text!="",
							
							response=BarcodeImage[text,"QR"];
							response=<|"photo"->response,
								"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
							followup[targetID]={},
							
							response="OK, send me a picture to scan, or "<>
								"some text to generate a QR code.";
							response=<|"text"->response,
								"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
							followup[targetID]={"/qr_text","/qr_photo"}
						]
					],
					
					response=<|"text"->err2,
						"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
					followup[targetID]={}
				],
				
			cmd=="/files",
				(* This is a file explorer function for the bot, base on
				the button reply_markup capabilities of the Telegram Bot's
				API. For this command, the "followup" association will
				be used to represent the last visited directory as well
				as telling the "BotProcessMessage" function weather to
				expect other inputs from the user. This way, there is
				no need to introduce another variable to save the last
				directory visisted by the user. *)
				If[
					minPrivilege[cmd]<=privilege,
					
					If[
						text!="",
						
						If[
							text=="/keyboardoff"||
							(text!="/ParentDirectory"&&
								FileExistsQ[followup[targetID]<>text]==False),
							
							response=<|"text"->"File explorer closed!",
								"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
							(* Calling the "BotKeyboardArray" with its 4th
							argument equal to 1, is for removing the custom
							keyboard created before *)
							followup[targetID]={},
							
							If[
								text=="/ParentDirectory",
								
								followup[targetID]=FileNameTake[
									followup[targetID],{1,-2}],
							
								followup[targetID]=followup[targetID]<>text
								(* This If block is to give the user a way to
								navigate back to previous folder *)
							];
							If[
								FileType[followup[targetID]]===Directory,
							
								SetDirectory[followup[targetID]];
								response=SortBy[
									Select[
										FileNames[],
										StringTake[#,1]!="."&
									],
									FileExtension[#]&&FileType[#]&];
								response=Map["/"<>#&,response];
								If[
									followup[targetID]!=$HomeDirectory,
									
									response=Prepend[response,
										"/ParentDirectory"]
									(* This If block is to prevent user from
									accessing root folders of the system *)
								];
								response=Prepend[response,"/keyboardoff"];
								If[
									Length[response]<240,
									
									response=Map[List,response];
									response=<|"text"->"Select an item \[DownArrow]",
										"keyboard"->BotKeyboardArray[response,
											1,1,0]|>,
									
									response=Partition[response,UpTo[100]];
									response=Map[StringJoin[Riffle[#,"\n"]]&,
										response];
									response=<|"text"->response,
										"keyboard"->BotKeyboardArray[{{}},
											0,0,1]|>
									(* Telegram Bot's custom keyboard markup
									can't have more that 244 buttons (in 1
									column, at least) so if a folder has more
									that approximately 240 items in total, the
									list of items are broken into smaller
									sublest and are sent to user as messages,
									not as buttons *)
								],
								
								If[
									FileType[followup[targetID]]===File,
									
									SetDirectory[$HomeDirectory];
									response=<|"filePath"->followup[targetID],
										"keyboard"->BotKeyboardArray[{{}},
										0,0,1]|>;
									followup[targetID]={}
									(* If the user selects a file, that file 
									will be sent to him and the task of the
									file explorer is finished. *)
								]
							]
						],
						
						(* this section happens when the user commands "/files"
						for the first time *)
						followup[targetID]=$HomeDirectory;
						SetDirectory[followup[targetID]];
						response=FileNames[];
						response=Select[response,StringTake[#,1]!="."&];
						response=Map["/"<>#&,response];
						response=Prepend[response,"/keyboardoff"];
						response=Map[List,response];
						response=<|"text"->"Select an item",
							"keyboard"->BotKeyboardArray[response,1,1,0]|>;
					],
					response=err2;
					followup[targetID]={}
				],
				
			cmd=="/dump",
				If[
					minPrivilege[cmd]<=privilege,
					
					If[
						KeyExistsQ[command,"data"],
						
						If[
							command["data"]=="",
							
							response="Download unsuccessful :-(";
							followup[targetID]={"/dump_text","/dump_document",
								"/dump_photo","/dump_audio","/dump_voice",
								"/dump_video"},
							
							If[
								$OperatingSystem=="MacOSX",
								
								response=<|"text"->"Got it. Your file can "<>
									"be found here:"<>
									"\n\n"<>command["data"]<>"\n\n"<>
									"Use /files to download it again."<>"\n"<>
									"You can also open it in the server "<>
									"with the default app using this "<>
									"button \[DownArrow]. Discard by /keyboardoff",
									"keyboard"->BotKeyboardArray[
										{{"/open "<>command["data"]}}
										,1,1,0]|>;
								followup[targetID]={},
								
								response="Got it. Your file is saved under "<>
									"the name:"<>
									"\n\n"<>
									FileBaseName[command["data"]]<>"."<>
									FileExtension[command["data"]];
								response=<|"text"->response,
									"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
								followup[targetID]={}
							]		 
						],
						
						response="OK, send me a file or a URL to save on "<>
							"the server.";
						response=<|"text"->response,
							"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
						followup[targetID]={"/dump_text","/dump_document",
							"/dump_photo","/dump_audio","/dump_voice",
							"/dump_video"}
					],
					
					response=<|"text"->err2,
						"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
					followup[targetID]={}
				],
			
			cmd=="/keyboardoff",
				(* If somehow the custom keyboard from previous commands
				is still in place, this command can remove it. *)
				If[
					minPrivilege[cmd]<=privilege,
					
					response=<|"text"->"Done!",
						"keyboard"->BotKeyboardArray[{{}},0,0,1]|>,
						
					response=err2
				];
				followup[targetID]={},
				
			cmd=="/help",
				(* Provides some instruction for the user, based on his/her
				privileges *)
				If[
					privilege==2,
					
					response="Send your location for some "<>
						"weather info."<>
						"\n\n"<>
						"Use the these commands without arguments:"<>
						"\n\n"<>
						"/whoami"<>"\n"<>"/ping"<>"\n"<>"/today"<>"\n"<>
						"/keyboardoff"<>"\n"<>"/help"<>"\n"<>"/botstop"<>
						"\n\n"<>
						"Use the these commands with text:"<>"\n\n"<>
						"/wol or /wolfram"<>"\n"<>"/shell"<>
						"\n\n"<>
						"Use the /open command to remotely open "<>
						"an application or a file on the server."<>
						"\n\n"<>
						"The /music command controls iTunes and can also "<>
						"generate a simple music. "<>
						"\n\n"<>
						"/qr command can be used to scan/generate QR codes."<>
						"\n\n"<>
						"The /files command enables you to remotely "<>
						"download your files."<>
						"\n\n"<>
						"With /dump command, you can save your files/URLs "<>
						"in a temp folder in the server.",
					
					If[
						privilege==1,
						
						response="Send me your location for some "<>
							"weather info."<>
							"\n\n"<>
							"Use the these commands without arguments:"<>
							"\n\n"<>
							"/whoami"<>"\n"<>"/ping"<>"\n"<>"/today"<>"\n"<>
							"/keyboardoff"<>"\n"<>"/help"<>
							"\n\n"<>
							"The /music command generates a simple music."<>
							"\n\n"<>
							"/qr command can be used to scan/generate QR "<>
							"codes."<>
							If[
								$CloudEvaluation==True,
								
								"\n\n"<>
								"Use /wol command to calculate expressions:"<>
								"\n\n"<>
								"/wol 2+2"<>
								"\n\n"<>
								"/wol N[Pi,100]"<>
								"\n\n"<>
								"/wol Plot3D[Sin[x+y^2],{x,-3,3},{y,-2,2}]"<>
								"\n\n"<>
								"/wol Solve[a*x^2+b*x+c==0,x]",
								
								""
							],
						
						response=err2
					]
				];
				response=<|"text"->response,
					"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
				followup[targetID]={},
				
			cmd=="/botstop",
				(* This command will terminate the "BotProcessMessage"
				Function *)
				If[
					minPrivilege[cmd]<=privilege,
					response=cmd,
					response=err2
				];
				followup[targetID]={},
				
			True,
				response=err1<>"\n\n"<>"Did you mean:"<>"\n\n"<>
					Nearest[Keys[minPrivilege],cmd,1];
				response=<|"text"->response,
					"keyboard"->BotKeyboardArray[{{}},0,0,1]|>;
				followup[targetID]={}
		];
		{response,followup}
	]


BotAnswer[bot_Association,message_,messageID_Integer,targetID_Integer,
	saveDir_String]:=
	(* This function determines the head of the generated message
	and sends it to the user.
	
	If the answer is an image or a sound, first it is saved in
	the location given by "saveDir" and then is sent to user.
	
	This function supports chat actions! Read more at:
	https://core.telegram.org/bots/api#sendchataction *)
	Block[
		{msg,args,fileName,res,err1},
		
		err1="Answer's head wasn't recognized :-("<>"\n";
		
		msg=message;
		
		(* The arguments used to send messages to the user are
		usually the same, so it is easier to create it beforehand *)
		args={"chat_id"->targetID,
			"reply_to_message_id"->messageID,
			If[
				AssociationQ[msg]&&KeyExistsQ[msg,"keyboard"],
				
				"reply_markup"->msg["keyboard"],
				
				Nothing
			],
			If[
				AssociationQ[msg]&&KeyExistsQ[msg,"parse_mode"],
				
				"parse_mode"->msg["parse_mode"],
				
				Nothing
			],
			If[
				AssociationQ[msg]&&
				KeyExistsQ[msg,"disable_web_page_preview"],
				
				"disable_web_page_preview"->msg["disable_web_page_preview"],
				
				Nothing
			]
		};
		
		
		Which[
			MemberQ[{Integer,Real,Complex,List,
				Symbol,Power,Times,Plus,Quantity},Head[msg]],
				msg=ToString[InputForm[msg]];
				(* Changing the "msg" to InputForm is to avoid 
				sending multi-line texts such as fractions or
				square roots *)
				res=BotCall[bot,"sendMessage",
					Append[args,"text"->msg]],
				
			MemberQ[{String},Head[msg]],
				res=BotCall[bot,"sendMessage",
					Append[args,"text"->msg]],
					
			MemberQ[{Association},Head[msg]],
				(* Associations are the results of calculations or
				some of the commands, like "/files" which produces
				an Association if the user selects a file to download *)
				Which[
					KeyExistsQ[msg,"text"],
						If[
							Head[msg["text"]]===List,
							(* If the message is too long, it can be
							partitioned to several smaller messages.
							In this case, the "text" variable will be
							a list. *)
							Do[
								res=BotCall[bot,"sendMessage",
									Append[args,"text"->msg["text"][[i]]]],
								{i,1,Length[msg["text"]],1}
							],
						
							res=BotCall[bot,"sendMessage",
								Append[args,"text"->msg["text"]]]
						],
						
					KeyExistsQ[msg,"filePath"],
						If[
							FileExistsQ[msg["filePath"]],
							
							BotCall[bot,"sendChatAction",{"chat_id"->targetID,
								"action"->"upload_document"}];
							res=BotFileCall[bot,msg["filePath"],
								"sendDocument","document",args],
							
							res=BotCall[bot,"sendMessage",
								Append[args,"text"->
									"There was no file to be sent!"]]
						],
					
					KeyExistsQ[msg,"photo"],
						fileName=FileNameJoin[{saveDir,
							DateString[{"Year","Month","Day",
							"Hour","Minute","Second"}]<>".png"}];
						Export[fileName,msg["photo"]];
						BotCall[bot,"sendChatAction",{"chat_id"->targetID,
							"action"->"upload_photo"}];
						res=BotFileCall[bot,fileName,"sendPhoto","photo",args],
					
					KeyExistsQ[msg,"audio"],
						fileName=FileNameJoin[{saveDir,
							DateString[{"Year","Month","Day",
							"Hour","Minute","Second"}]<>".mp3"}];
						Export[fileName,msg["audio"]];
						BotCall[bot,"sendChatAction",{"chat_id"->targetID,
							"action"->"upload_audio"}];
						res=BotFileCall[bot,fileName,"sendAudio","audio",args],
					
					True,
						msg=ToString[msg];
						res=BotCall[bot,"sendMessage",
							Append[args,"text"->msg]]
				],
				
			MemberQ[{Graphics,Graphics3D,Graph,GeoGraphics,
				Image,Dataset},Head[msg]],
				fileName=FileNameJoin[{saveDir,
					DateString[{"Year","Month","Day",
						"Hour","Minute","Second"}]<>".png"}];
				Export[fileName,msg];
				BotCall[bot,"sendChatAction",{"chat_id"->targetID,
						"action"->"upload_photo"}];
				res=BotFileCall[bot,fileName,"sendPhoto","photo",args],
					
			MemberQ[{Sound,Audio},Head[msg]],
				fileName=FileNameJoin[{saveDir,
					DateString[{"Year","Month","Day",
						"Hour","Minute","Second"}]<>".mp3"}];
				Export[fileName,msg];
				BotCall[bot,"sendChatAction",{"chat_id"->targetID,
						"action"->"upload_audio"}];
				res=BotFileCall[bot,fileName,"sendAudio","audio",args],
					
			MemberQ[{File},Head[msg]],
				fileName=msg[[1]];
				fileName=FileNameJoin[{saveDir,
					DateString[{"Year","Month","Day",
						"Hour","Minute","Second"}]<>"."<>
					FileExtension[fileName]}];
				CopyFile[msg[[1]],fileName];
				BotCall[bot,"sendChatAction",{"chat_id"->targetID,
						"action"->"upload_document"}];
				res=BotFileCall[bot,fileName,"sendDocument","document",args],
					
			True,
				msg="Head: "<>
					ToString[Head[msg]]<>"\n"<>ToString[msg];
				msg=StringJoin[err1,msg];
				res=BotCall[bot,"sendMessage",
					Append[args,"text"->msg]]
		];
		res
	]


BotProcessMessage[botFile_String,logSizeLimit_Integer:10^5,pollTime_:3,
	webhookUpdates_String]:=
	(* This function, is the engine of the bot!
	
	It processes logs (using "BotSelectUpdate" or "BotSelectUpdateWebhook")
	and answers each unanswered update using "BotCommand" & "BotAnswer"
	functions and writes the results in the log files.
	
	To indicate "getUpdates" method, the "webhookUpdates" argument has to
	be an empty string (or ""). Otherwise, this function assumes that the
	bot uses webhooks. The string in "webhookUpdates" should be the result
	of something like this in the APIFunction:
	
	ToString[HTTPRequestData["Body"]]
	
	User inputs are prepared here in a way that can be used in
	the "BotCommand" function; for example when the user sends
	his or her location, it won't be a telegram command, but this
	function converts that to something like:
	<|"command"\[Rule]"/location","data"\[Rule]<|"latitude"\[Rule]35.7,"longitude"\[Rule]53.1|>|>
	which the "BotCommand" function understands.
	
	If the bot uses "getUpdates" method to obtain new updates, when all
	updates are answered, the function waits for new inputs from the user.
	
	If the bot has a webhook, this function will stop after answering all
	updates.
	
	The engine can be shut down using "/botstop" command from the admin.
	The effect of this command can be felt only when using "getUpdates"
	method; because in webhook method, as soon as a new message arrives,
	this function starts over.
	
	This re-starting of the function, causes the "followup" association
	to be reset, which will disable the bot from expecting multi-step
	commands from the user. To overcome this, the "followup" association
	is imported from a file in when the function starts up and is exported
	when it's about to shut down. *)
	Block[
		{saveDir,logPath,followupPath,bot,botStop,logBaseName,newLogPath,
		botLog,index,temp,keys,updateID,messageID,targetUser,targetID,date,
		type,content,followup,msg,cmdPosition,fileName,res,
		err1,err2,err3},
		
		err1="That wasn't a command! Use /help";
		err2="Head wasn't recognized :-("<>"\n";
		err3="I don't know what to do with a ";
		
		Quiet[
			Check[
				bot=Import[botFile,"RawJSON"];
				saveDir=FileNameJoin[{DirectoryName[botFile],
					bot["BotUsername"]<>"_Temp"}];
				Quiet[CreateDirectory[saveDir]];
				logPath=FileNameJoin[{saveDir,"log.txt"}];
				followupPath=FileNameJoin[{saveDir,"followup.json"}];
				botStop=False,
				
				botStop=True
				(* If the "bot" can't be imported from "botFile", the main
				body of the function will not start. *)
			]
		];
		
		(* The "followup" association indicate weather there will be 
		further interaction with each user. It has the following structure:
		followup=<|userID_1\[Rule]{type_1,...,type_n},userID_2\[Rule]{...},...|> *)
		If[
			webhookUpdates=="",
				
			followup=<||>,
			
			Quiet[
				Check[
					followup=Import[followupPath,"RawJSON"];
					followup=KeyMap[ToExpression,followup],
				
					followup=<||>
				]
			]
		];
		
		While[
			botStop==False,
			
			bot=Import[botFile,"RawJSON"];
			
			If[
				(* This If block is to avoid creating large log files, which
				will slow down the importing process. This is done by creating
				a ".txt" file which has the paths to the existing log files
				and those that are to be generated. *)
				FileExistsQ[logPath],
				
				logBaseName=bot["BotUsername"]<>"_log_";
				Quiet[
					Check[
						newLogPath=Last[StringSplit[Import[logPath,"Text"],
							"\n"]];
						If[
							logSizeLimit<=FileByteCount[newLogPath],
							
							newLogPath=FileNameJoin[{DirectoryName[logPath],
								"resultLogs",
								logBaseName<>
								DateString[{"Year","_","Month","_","Day","_",
									"Hour","Minute","Second"}]<>".json"}];
							Export[
								logPath,
								Append[
									StringSplit[Import[logPath,"Text"],"\n"],
									newLogPath
								],
								"Text"
							]
						],
						
						
						Quiet[CreateDirectory[FileNameJoin[{
							DirectoryName[logPath],
							"resultLogs"}]]];
						newLogPath=FileNameJoin[{DirectoryName[logPath],
							"resultLogs",
							logBaseName<>
							DateString[{"Year","_","Month","_","Day","_",
								"Hour","Minute","Second"}]<>".json"}];
						Export[logPath,newLogPath,"Text"]
					]
				],
				
				logBaseName=bot["BotUsername"]<>"_log_";
				newLogPath=FileNameJoin[{DirectoryName[logPath],"resultLogs",
					logBaseName<>
					DateString[{"Year","_","Month","_","Day","_",
						"Hour","Minute","Second"}]<>".json"}];
				Export[logPath,newLogPath,"Text"]
			];
			
			
			(* Importing general stuff from the log *)
			
			
			If[
				(* This If block is a turning point in this function.
				It decides weather to the bot uses "getUpdates" method
				or webhook to get new updates. *)
				webhookUpdates=="",
				
				{botLog,index}=BotSelectUpdate[bot,newLogPath,pollTime],
				
				{botLog,index}=BotSelectUpdateWebhook[
					ImportString[webhookUpdates,"RawJSON"],newLogPath]
			];
			
			If[
				(* If the bot uses webhooks, there is no need to stay
				in the WHILE loop for new updates; so when all updates
				are answered, the "index" will be 0 and that can be
				used to exit this WHILE loop. *)
				webhookUpdates!=""&&index==0,
				
				botStop=True,
			
				temp=botLog[[index]];
				keys=Keys[temp["update_result"]];
				updateID=temp["update_result","update_id"];
				messageID=temp["update_result","message_id"];
				targetUser=temp["update_result","from","username"];
				targetID=temp["update_result","from","id"];
				date=temp["update_result","date"];
		
				(* Importing specific keys and values from the log *)
				Which[
					MemberQ[keys,"text"],
						type="text";
						content=temp["update_result","text"];
						If[
							KeyExistsQ[followup,targetID],
							
							Which[
								MemberQ[followup[targetID],"/wol_text"],
									content=<|"command"->"/wol",
										"text"->content|>;
									{msg,followup}=BotCommand[botFile,content,
										followup,targetUser,targetID,date,
										newLogPath],
								
								MemberQ[followup[targetID],"/open_text"],
									content=<|"command"->"/open",
										"text"->content|>;
									{msg,followup}=BotCommand[botFile,content,
										followup,targetUser,targetID,date,
										newLogPath],
								
								MemberQ[followup[targetID],"/music_text"],
									content=<|"command"->"/music",
										"text"->content|>;
									{msg,followup}=BotCommand[botFile,content,
										followup,targetUser,targetID,date,
										newLogPath],
										
								MemberQ[followup[targetID],"/qr_text"],
									content=<|"command"->"/qr",
										"text"->content|>;
									{msg,followup}=BotCommand[botFile,content,
										followup,targetUser,targetID,date,
										newLogPath],
								
								MemberQ[followup[targetID],"/box_text"],
									content=<|"command"->"/box",
										"text"->content|>;
									{msg,followup}=BotCommand[botFile,content,
										followup,targetUser,targetID,date,
										newLogPath],
												
								MemberQ[followup[targetID],"/shell_text"],
									If[
										(* Taking screenshots using shell
										requires a location to save the image.
										The "BotCommand" function doesn't need
										a "saveDir" location except for this
										command; so the location will be
										passed on to it just here. *)
										content=="Server screenshot",
										content="Server screenshot"<>"@"<>
											saveDir
									];
									content=<|"command"->"/shell",
										"text"->content|>;
									{msg,followup}=BotCommand[botFile,content,
										followup,targetUser,targetID,date,
										newLogPath],
								
								MemberQ[followup[targetID],"/dump_text"],
									fileName=FileNameJoin[{saveDir,
										DateString[{"Year","Month",
										"Day","Hour","Minute","Second"}]}];
									Quiet[
										Check[
											(* First create an empty file
											without extension and download
											the url in it *)
											URLDownload[content,
												CreateFile[fileName]];
											content=fileName<>"."<>
												ToLowerCase[
													FileFormat[fileName]];
											(* Then rename the file using its
											format as extension *)
											RenameFile[fileName,content],
											
											content=""
										]
									]; 
									content=<|"command"->"/dump",
										"data"->content|>;
									{msg,followup}=BotCommand[botFile,content,
										followup,targetUser,targetID,date,
										newLogPath],
										
								StringContainsQ[ToString[followup[targetID]],
									$HomeDirectory],
									(* Telegram Bot's API, interprets Farsi
									commands as texts and so this section has
									become necessary for dealing with files
									and folders with Farsi names *)
									content=<|"command"->"/files",
										"text"->content|>;
									{msg,followup}=BotCommand[botFile,content,
										followup,targetUser,targetID,date,
										newLogPath],
								
								True,
									msg=err1;
									followup[targetID]={}
							],
								
							msg=err1;
							followup[targetID]={}
						],
						
					MemberQ[keys,"command"],
						type="command";
						content=temp["update_result","command"];
						If[
							(* If the followup association of the user has the
							string "/Users/username", it means:
							1. The "BotProcessMessage" should expect another
							   input from user &
							2. The last visited directory by the user is the
							   value of followup[targetID].
							
							The first sectoion of this If block is for the
							"/files" command *)
							StringContainsQ[ToString[followup[targetID]],
								$HomeDirectory],
							content=<|"command"->"/files","text"->content|>;
							{msg,followup}=BotCommand[botFile,
								content,followup,targetUser,targetID,date,
								newLogPath],
							
							(* Telegram commands are like this:
							/command
							or
							/command STRING
							In the next few lines the function tries to
							separate the "/command" part from the "STRING"
							part (if there is one). *)
							content=StringSplit[content, " ", 2];
							(* "content" is now a list of strings with
							1 or 2 members, only *)
					
							If[
								Length[content]==2,
						
								content=<|"command"->content[[1]],
									"text"->content[[2]]|>;
								{msg,followup}=BotCommand[botFile,content,
									followup,targetUser,targetID,date,
									newLogPath],
							
								content=<|"command"->content[[1]]|>;
								{msg,followup}=BotCommand[botFile,content,
									followup,targetUser,targetID,date,
									newLogPath];
								If[
									(* This gives the admin a way to remotely
									stop the bot! *)
									msg=="/botstop",
									
									botStop=True;
									(* Remove the custom keyboard while your
									at it! *)
									msg=<|"text"->"Shutting down...",
										"keyboard"->
											BotKeyboardArray[{{}},0,0,1]|>
								]
							]
						],
						
					MemberQ[keys,"document"],
						type="document";
						If[
							KeyExistsQ[followup,targetID],
							
							Which[
								MemberQ[followup[targetID],"/dump_document"],
									content=temp["update_result","document",
										"file_id"];
									content=BotGetFile[bot,content,saveDir,
										newLogPath];
									content=content["getfile_result",
										"file_path"];
									content=<|"command"->"/dump",
										"data"->content|>;
									{msg,followup}=BotCommand[botFile,content,
										followup,targetUser,targetID,date,
										newLogPath],
								
								True,
									msg=err3<>type<>"!";
									followup[targetID]={}
							],
							
							msg=err3<>type<>"!";
							followup[targetID]={}
						],
						
					MemberQ[keys,"photo"],
						type="photo";
						If[
							KeyExistsQ[followup,targetID],
							
							Which[
								MemberQ[followup[targetID],"/qr_photo"],
									content=temp["update_result","photo",
										"file_id"];
									content=BotGetFile[bot,content,saveDir,
										newLogPath];
									content=Import[content["getfile_result",
										"file_path"],"Image"];
									content=<|"command"->"/qr",
										"data"->content|>;
									{msg,followup}=BotCommand[botFile,content,
										followup,targetUser,targetID,date,
										newLogPath],
										
								MemberQ[followup[targetID],"/dump_photo"],
									content=temp["update_result","photo",
										"file_id"];
									content=BotGetFile[bot,content,saveDir,
										newLogPath];
									content=content["getfile_result",
										"file_path"];
									content=<|"command"->"/dump",
										"data"->content|>;
									{msg,followup}=BotCommand[botFile,content,
										followup,targetUser,targetID,date,
										newLogPath],
								
								True,
									msg=err3<>type<>"!";
									followup[targetID]={}
							],
							
							msg=err3<>type<>"!";
							followup[targetID]={}
						],
						
					MemberQ[keys,"audio"],
						type="audio";
						If[
							KeyExistsQ[followup,targetID],
							
							Which[
								MemberQ[followup[targetID],"/dump_audio"],
									content=temp["update_result","audio",
										"file_id"];
									content=BotGetFile[bot,content,saveDir,
										newLogPath];
									content=content["getfile_result",
										"file_path"];
									content=<|"command"->"/dump",
										"data"->content|>;
									{msg,followup}=BotCommand[botFile,content,
										followup,targetUser,targetID,date,
										newLogPath],
								
								True,
									msg=err3<>type<>"!";
									followup[targetID]={}
							],
							
							msg=err3<>type<>"!";
							followup[targetID]={}
						],
						
					MemberQ[keys,"voice"],
						type="voice";
						If[
							KeyExistsQ[followup,targetID],
							
							Which[
								MemberQ[followup[targetID],"/dump_voice"],
									content=temp["update_result","voice",
										"file_id"];
									content=BotGetFile[bot,content,saveDir,
										newLogPath];
									content=content["getfile_result",
										"file_path"];
									content=<|"command"->"/dump",
										"data"->content|>;
									{msg,followup}=BotCommand[botFile,content,
										followup,targetUser,targetID,date,
										newLogPath],
								
								True,
									msg=err3<>type<>"!";
									followup[targetID]={}
							],
							
							msg=err3<>type<>"!";
							followup[targetID]={}
						],
						
					MemberQ[keys,"video"],
						type="video";
						If[
							KeyExistsQ[followup,targetID],
							
							Which[
								MemberQ[followup[targetID],"/dump_video"],
									content=temp["update_result","video",
										"file_id"];
									content=BotGetFile[bot,content,saveDir,
										newLogPath];
									content=content["getfile_result",
										"file_path"];
									content=<|"command"->"/dump",
										"data"->content|>;
									{msg,followup}=BotCommand[botFile,content,
										followup,targetUser,targetID,date,
										newLogPath],
								
								True,
									msg=err3<>type<>"!";
									followup[targetID]={}
							],
							
							msg=err3<>type<>"!";
							followup[targetID]={}
						],
						
					MemberQ[keys,"contact"],
						type="contact";
						If[
							KeyExistsQ[followup,targetID],
							
							Which[
								MemberQ[followup[targetID],"/qr_contact"],
									content=temp["update_result","contact"];
									content=<|"command"->"/qr",
										"data"->content|>;
									{msg,followup}=BotCommand[botFile,content,
										followup,targetUser,targetID,date,
										newLogPath],
								
								True,
									msg=err3<>type<>"!";
									followup[targetID]={}
							],
							
							msg=err3<>type<>"!";
							followup[targetID]={}
						],
						
					MemberQ[keys,"location"],
						type="location";
						content=<|"command"->"/location",
							"data"->temp["update_result","location"][[All]]|>;
						(* The location information is now essentially
						a command which can be used in the "BotCommand"
						function *)
						{msg,followup}=BotCommand[botFile,content,
							followup,targetUser,targetID,date,newLogPath];
				];
				
				(* Sending the results back to user *)
				If[
					msg=!="",
					
					res=BotAnswer[bot,msg,messageID,targetID,saveDir];
					msg="",
					
					res=<||>
				];
				
				(* Updating the log files *)
				temp["update_result","answered"]=True;
				botLog[[index]]=temp;
				Export[newLogPath,botLog,"JSON"];
				BotLog[res,newLogPath];
				If[
					webhookUpdates=="",
				
					BotCall[bot,"getUpdates",{"offset"->updateID+1}],
					
					followup=KeyMap[ToString,followup];
					(* The keys in a JSON file can't be integers *)
					Export[followupPath,followup,"JSON"]
				];
			]
		]
	]


End[]


EndPackage[]
