data PlayerStatus
  = Tired
  | Hungry
  | Content

instance Show PlayerStatus where
    show Tired   = "You have not rested in some time"
    show Hungry  = "You are very hungry"
    show Content = "You are content"

data CharacterStats =  CharacterStats
  { health :: Int
  , strength :: Int }

data State = State
  { playerStatus :: PlayerStatus
  , playerStats :: CharacterStats }

startingStats :: CharacterStats
startingStats = CharacterStats { health=100, strength=1 }

initial :: State
initial = State { playerStatus=Hungry, playerStats=startingStats }

describe :: State -> String
describe State {playerStatus=status} = (show status)

data Action
  = Eat
  | Sleep
  deriving (Read)

update :: State -> Action -> State
update state@State { playerStatus=Hungry } Eat = state { playerStatus = Content }
update state@State { playerStatus=Tired } Sleep = state { playerStatus = Content }
update state _ = state

turn :: State -> IO String
turn state = do
  putStrLn $ describe state
  putStrLn "What do you want to do? Eat | Sleep"
  command <- getLine
  turn $ update initial (read command :: Action)

main :: IO String
main = do turn initial
