STry [] _ stmF -> execute context (SBlock stmF)
STry (t:stmT) stmC stmF -> 
      case execute context t of
         Left e -> 
            case execute context (SBlock stmC) of
               Left e -> Left e
               Right y -> execute (y) (SBlock stmF)
      
         Right x -> (STry stmT stmC stmF) 
      
            



            {-case execute context f of
               Left e -> execute(x) (STry stmT (c:stmC) (f:stmF))
               Right w -> execute(x) (STry stmT (c:stmC) (f:stmF)) 
                          execute(w) (SBlock stmF)-}
            -- (STry stmT) (execute context f) (SBlock stmF)


--STry (t:stmT) (c:stmC) (f:stmF) -> execute(execute context t) (STry )