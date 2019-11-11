{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( someFunc
    ) where

import Control.Monad ( replicateM )
import Control.Monad.Random ( Rand, RandomGen, getRandomR, evalRandIO )
import Data.Hashable ( Hashable )
import qualified Data.HashMap.Strict as HMS
import GHC.Generics (Generic)

data Course = Course
    { courseName :: String
    , courseMaxStudents :: Int
    } deriving ( Show, Eq )

newtype CourseId = CourseId { unCourseId :: Int }
    deriving ( Show, Eq, Generic )

instance Hashable CourseId

newtype CourseMap = CourseMap { unCourseMap :: HMS.HashMap CourseId Course }
    deriving ( Show, Eq )

data Student = Student
    { studFirstName :: String
    , studLastName :: String
    , studClass :: String
    , studPreference :: [ CourseId ]
    } deriving ( Show, Eq )

type Students = [ Student ]

newtype Individual = Individual { unIndividual :: HMS.HashMap CourseId Students }
    deriving ( Show )

randomIndividual :: (RandomGen g)
    => CourseId -- ^ max course ID, IDs must be in [ 1 .. maxCid ]
    -> Students
    -> Rand g Individual
randomIndividual maxCid students = Individual <$> result
    where
        result = HMS.fromListWith (++) <$> mapM go students
        go student = getRandomR (1, unCourseId maxCid) >>=
            \cid -> pure (CourseId cid, [ student ])

newtype Population = Population { unPopulation :: [ Individual ] }
    deriving ( Show )

randomPopulation :: (RandomGen g)
    => Int -- ^ population size
    -> CourseId -- ^ see randomIndividual
    -> Students
    -> Rand g Population
randomPopulation ps maxCid students = Population <$> replicateM ps (randomIndividual maxCid students)

someFunc :: IO ()
someFunc = do
    let
        studs =
            [ Student "Falko" "Schleif" "3b" [ ]
            , Student "Matthias" "Treydte" "2b" [ ]
            , Student "Helene" "Fischer" "4a" [ ]
            , Student "Summer" "Glau" "3b" [ ]
            , Student "Sarah" "Connor" "7a" [ ]
            ]

    values <- evalRandIO (randomPopulation 1 (CourseId 3) studs)
    putStrLn (show values)
