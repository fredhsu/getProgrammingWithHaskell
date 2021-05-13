robot :: (a, Integer, Integer) -> ((a, Integer, Integer) -> t) -> t
robot (name, attack, hp) = \message -> message (name, attack, hp)

killerRobot = robot ("Kill3r",25,200)

name (n,_, _) = n
attack :: (a, Integer, Integer) -> Integer
attack (_, a, _) = a
hp :: (a, Integer, Integer) -> Integer
hp (_, _, hp) = hp

getName aRobot = aRobot name
getAttack :: (((a, Integer, Integer) -> Integer) -> t) -> t
getAttack aRobot = aRobot attack
getHP :: (((a, Integer, Integer) -> Integer) -> t) -> t
getHP aRobot = aRobot hp

setName aRobot newName = aRobot (\(n,a,h) ->  robot(newName, a,h ))
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot(n,newAttack,h))
setHp aRobot newHp = aRobot (\(n,a,h) -> robot(n,a,newHp))

printRobot aRobot = aRobot (\(n,a,h) -> n ++
                                        " attack:" ++ (show a) ++
                                        " hp:"++ (show h))

damage aRobot attackDamage = aRobot (\(n,a,h) ->
                                      robot (n,a,h-attackDamage))

fight aRobot defender = damage defender attack
    where attack = if getHP aRobot > 10
                   then getAttack aRobot
                   else 0

gentleGiant = robot ("Mr. Friendly", 10, 300)

superRobot = setHp killerRobot 500
ulitimateRobot = setHp killerRobot 1000
robots = [killerRobot, gentleGiant, superRobot]

ultimateFight = fight ulitimateRobot
-- threeRoundFight a b = if getHP (fight a b) < getHP (fight a b)
--                         then b
--                         else a

b1 a b = fight a b
a1 a b = fight b a


-- robotAround3 = fight robotBround2 robotBround2
                                    -- robotBround3 = fight robotAround2 robotAround2
                                    -- robotAround2 = fight robotBround1 robotAround1
                                    -- robotBround2 = fight robotAround1 robotBround1