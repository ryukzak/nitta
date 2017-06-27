{-# LANGUAGE FlexibleContexts #-}
import           Base
import           Data.Default
import           Data.Map     (fromList)
import           FRAM
import           Test.Hspec

main = hspec $ do
  return ()
  -- let busy = def { actions=MC [Pull [1]] def }
  -- let with_queue = def { actions=[MC [Pull [2]] def] }
  -- let with_queue' = def { actions=[MC [Pull [2] def], MC [Push 2] def] }
  -- let only_final = def { final=MC [Pull [3]] def }
  -- let free = def
  -- describe "Testing freeCell" $ do
    -- it "find free cell in memory" $ do
      -- (freeCell $ fromList [ (0, busy), (1, free)]) `shouldBe` Just (1, free)
    -- it "find mostly unloaded free cell in memory" $ do
      -- (freeCell $ fromList [ (0, with_queue), (1, only_final), (3, free)]) `shouldBe` Just (3, free)
    -- it "can't find free cell in memory" $ do
      -- (freeCell $ fromList [ (0, busy) ]) `shouldBe` Nothing

  -- describe "FRAM action variants" $ do
    -- it "no variants" $ do
      -- variants def `shouldBe` ([] :: [(Interaction Int, Times Int)])
    -- it "only one cell" $ do
      -- let vars cells = map fst $ ( variants def { memoryState=fromList cells } :: [(Interaction Int, Times Int)] )
      -- vars [ (0, busy)                                   ] `shouldBe` [ Pull [1]                     ]
      -- vars [            (1, with_queue)                  ] `shouldBe` [ Pull [2]                     ]
      -- vars [            (1, with_queue')                 ] `shouldBe` [           Pull [2], Push 2   ]
      -- vars [                             (2, only_final) ] `shouldBe` [                     Pull [3] ]
      -- vars [            (1, with_queue), (2, only_final) ] `shouldBe` [           Pull [2], Pull [3] ]
      -- vars [ (0, busy), (1, with_queue), (2, only_final) ] `shouldBe` [ Pull [1], Pull [2], Pull [3] ]
    -- it "only remains" $ do
      -- let vars mcs = map fst $ ( variants def { remains=mcs } :: [(Interaction Int, Times Int)] )
      -- vars [ MC [Pull [4], Push 5]                ] `shouldBe` [Pull [4]          ]
      -- vars [ MC [Pull [4], Push 5], MC [Pull [6]] ] `shouldBe` [Pull [4], Pull [6]]
    -- it "cell and remain" $ do
      -- let vars cells remains = map fst $ ( variants def
                                           -- { memoryState=fromList cells
                                           -- , remains=remains
                                           -- } :: [(Interaction Int, Times Int)] )
      -- vars [ (0, busy) ] [ MC [Pull [4], Push 5] ] `shouldBe` [ Pull [1], Pull [4] ]



    -- it "one of the cells have next" $ do
      -- variants def `shouldBe` ([] :: [Action String])
    -- it "one of the cells have last" $ do
      -- variants def `shouldBe` ([] :: [Action String])

