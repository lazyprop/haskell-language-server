main = do
    x <- return $ foo 42
    return (foo x)
foo, bar :: Int -> Int
foo x = x + 1
bar = (+ 1) . foo
