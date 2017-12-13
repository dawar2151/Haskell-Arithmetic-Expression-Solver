module Commands(
Handler(..),
Command(..),
commandes,
isCommand,
getCommandByName
)
where
import Expression
import System.Exit

type Handler = [String] -> Store -> IO Store
--data Commands = []
data Command = Command {
 name :: String, -- Nom de la commande
 description :: String, -- Description de la commande
 -- utilise ’ par la commande -help
 exits :: Bool,
 run :: Handler
}

quit = Command {
name="quit",
description="on sort du programme",
exits = True,
run = quitHandler
}

help = Command {
name="help",
description="on affiche la liste des commandes existants avec des explications",
exits = False,
run = helpHandler
}

store = Command {
name="store",
description="on affiche le contenu du store ",
exits = False,
run = storeHandler
}

set = Command {
name="set",
description="on ajoute au store la variable x",
exits = False,
run = setHandler
}
unset = Command {
name="unset",
description="on enlève x du store courant.",
exits = False,
run = unsetHandler
}
commandes = [store,set,unset,quit,help]
--let Commands = {quit,help,store,set,unset}

quitHandler ::Handler
quitHandler xs s = do 
  putStrLn "Au revoir"
  exitSuccess
  return s

setHandler ::Handler
setHandler = \args store -> do
 --store<-(args !! 1, 3.3)++store
 return $ (args !! 1, read (args !! 2)::Double):store

unsetHandler ::Handler
unsetHandler = \args store -> do
 --store<-(args !! 1, 3.3)++store
 return $ removeItem (args !! 1) store

storeHandler ::Handler
storeHandler = \args store -> do
 putStrLn $ show store
 return store

showCommands::[Command]->String
showCommands [] = ""
showCommands (x:xs) = (name (x) ++ " : " ++ description (x)) ++ "\n" ++ showCommands xs

helpHandler ::Handler
helpHandler = \args store -> do
 putStrLn (showCommands commandes)
 return store

isCommand::[Char]->[Command]->Bool
isCommand c [] = False
isCommand c (x:xs) = c == (name x) || isCommand c xs

getCommandByName :: [Char]->[Command]->Command
getCommandByName s (x:xs) = if s == (name x) then x else getCommandByName s xs

removeItem :: String->Store->Store
removeItem _ [] = []
removeItem x (y:ys) | x == fst y    = removeItem x ys
                    | otherwise = y : removeItem x ys 
