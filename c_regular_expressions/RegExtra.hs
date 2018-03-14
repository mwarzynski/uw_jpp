-- C Language Regular Expressions interpreter.
--
-- Author:
-- Mateusz Warzyński (371854) <m.warzynski@students.uw.edu.pl>
--

-- Rozwiązania mają być samodzielne. Wszelkie zapożyczenia z internetu itp.
--    należy wyrażnie zaznaczyć.
-- Należy oddać wyłącznie plik RegExtra.hs. Nie może on importować nic ponadto
--    co jest już importowane w RegExtra0.hs
-- Rozwiązania będą oceniane pod kątem:
--    - spełnienia warunków zadania; rozwiązania nie przechodzące testów będą nisko oceniane,
--          nawet na 0p; rozwiązania będą też poddawane dodatkowym testom.
--    - właściwego wykorzystania mechanizmów paradygmatu funkcyjnego i języka Haskell,
--          tudzież czytelności i stylu.
--    - rozwiązania skrajnie nieefektywne będą karane; przy porządnym rozwiązaniu testy
--          przechodzą w ok 1s. Rozwiązanie, gdzie będzie to trwało ponad 2 minuty
--          uznamy za nieefektywne.

module RegExtra where
import Mon
import Reg
import Data.List

