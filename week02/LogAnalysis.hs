{-# OPTIONS_GHC -Wall #-}
-- module LogAnalysis where

import Text.Read
import Log

-- parseMessage01 :: String -> LogMessage
-- parseMessage01 s = Unknown s


-- parseMessageType :: String -> MessageType


makeError :: String -> Maybe MessageType
makeError severity = case (readMaybe severity) of
                     Nothing -> Nothing
                     Just i -> Just $ Error i

extractMessageType :: [String] -> Maybe MessageType
extractMessageType ws = case ws of
                        [] -> Nothing
                        (w:ws') -> case w of 
                                   "I" -> Just Info
                                   "W" -> Just Warning
                                   "E" -> case ws' of
                                          [] -> Nothing
                                          (w':_) -> makeError w'
                                   _   -> Nothing

getTimeStamp :: [String] -> Maybe TimeStamp
getTimeStamp ws = case ws of
                      [] -> Nothing
                      (w:_) -> readMaybe w

timeStampIndex :: MessageType -> Int
timeStampIndex m = case m of
                     Error _ -> 2
                     _       -> 1

-- dropByMessageType :: MessageType -> [String] -> [String]
-- dropByMessageType mt ws = drop (timeStampIndex mt) ws


-- parseMessage02 :: String -> LogMessage
-- 
-- parseMessage02 s = parseMessageH s (words s)
--     where parseMessageH s' (t:s:ws) = LogMessage Info 33 s'
--           parseMessageH s' _        = Unknown s'

parseMessage :: String -> LogMessage
parseMessage m =
    let messageParts = words m
        messageType  = extractMessageType messageParts
        dropIndex mt = timeStampIndex mt
        timeStamp mt = getTimeStamp (drop (dropIndex mt) messageParts)
    in case messageType of
       Nothing -> Unknown m
       Just t  -> case (timeStamp t) of
                  Nothing -> Unknown m
                  Just ts -> LogMessage t ts (unwords (drop ((dropIndex t)+ 1) messageParts))

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)




main :: IO ()
main =  do
    putStrLn "Hello, Week02"
--     print $ parseMessage01 "Hello, world!"
--     print $ parseMessage02 "Hello 22 3123"
--     print $ parseMessage02 "Hello, world! kjlkj"
--     print $ parseMessage02 "Hello, world!    "
    
    print $ extractMessageType (words "E 1 1137 'How queer it seems,' Alice said to herself, 'to be going messages for")
    print $ extractMessageType (words "I 5478 #78 (red, Cherruppows 00000fed, cootered SSDT BOOTMEM")
    print $ extractMessageType (words "I 2802 some time without interrupting it.")
    print $ extractMessageType (words "I 731 mode.0: persicesoupda] BOOTMEM")
    print $ extractMessageType (words "I 3989 'I could tell you my adventures--beginning from this morning,' said")
    print $ extractMessageType (words "I 917 'Fury said to a")
    print $ extractMessageType (words "I 2546 Alice felt dreadfully puzzled. The Hatter's remark seemed to have no")
    print $ extractMessageType (words "I 2179 For he can thoroughly enjoy")
    print $ extractMessageType (words "I 2481 pcmci_bus dec an0: reseresoftecter tork que 15-0x0000 GiB)")
    print $ extractMessageType (words "W 2429 [drm: 0xf4280 -> 0xfff")
    print $ extractMessageType (words "I 165 ACPI by 14506 7 (v01000000 - 00 (us regis 00000017 [io 000: LNKBD,PNP0A08-0xcffer")


    print $ parseMessage "I 1020 pci_cacput13, oundow [io for isted"
    print $ parseMessage "E 1 1137 'How queer it seems,' Alice said to herself, 'to be going messages for"
    print $ parseMessage "I 5478 #78 (red, Cherruppows 00000fed, cootered SSDT BOOTMEM"
    print $ parseMessage "I 2802 some time without interrupting it."
    print $ parseMessage "I 731 mode.0: persicesoupda] BOOTMEM"
    print $ parseMessage "I 3989 'I could tell you my adventures--beginning from this morning,' said"
    print $ parseMessage "I 917 'Fury said to a"
    print $ parseMessage "I 2546 Alice felt dreadfully puzzled. The Hatter's remark seemed to have no"
    print $ parseMessage "I 2179 For he can thoroughly enjoy"
    print $ parseMessage "I 2481 pcmci_bus dec an0: reseresoftecter tork que 15-0x0000 GiB)"
    print $ parseMessage "W 2429 [drm: 0xf4280 -> 0xfff"
    print $ parseMessage "I 165 ACPI by 14506 7 (v01000000 - 00 (us regis 00000017 [io 000: LNKBD,PNP0A08-0xcffer"
    print $ parseMessage "I 3373 Alice did not much like keeping so close to her: first, because the"



























