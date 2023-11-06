
module TestInputs() where
import Boxes


blankGame = ([],[], P1, 3)
-- .   .   .
--
-- .   .   .
--
-- .   .   .

oneBoxGame = ([((1,1) "Right"), ((1,1)"Down"), ((2,1)"Down"), ((1,2)"Right")], [((1,1), P1)], P1, 3)
-- . — .   .
-- | 1 |
-- . — .   .
--
-- .   .   .