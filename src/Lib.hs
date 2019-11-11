module Lib
    ( someFunc
    ) where

import qualified Data.HashMap.Strict as HMS

data Course = Course
    { courseName :: String
    , courseMaxStudents :: Int
    } deriving ( Show, Eq )

newtype CourseId = CourseId { unCourseId :: Int }
    deriving ( Show, Eq )

newtype CourseMap = CourseMap { unCourseMap :: HMS.HashMap CourseId Course }
    deriving ( Show, Eq )

data Student = Student
    { studFirstName :: String
    , studLastName :: String
    , studClass :: String
    , studPreference :: [ CourseId ]
    } deriving ( Show, Eq )

newtype Individual = Individual { unIndividual :: HMS.HashMap CourseId [ Student ] }

someFunc :: IO ()
someFunc = putStrLn "someFunc"
